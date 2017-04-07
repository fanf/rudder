package com.normation.rudder.services.servers
import net.liftweb.common._
import com.normation.inventory.domain.NodeId
import com.unboundid.ldif.LDIFChangeRecord
import com.normation.rudder.domain.NodeDit
import com.normation.ldap.sdk.LDAPConnectionProvider
import com.normation.rudder.repository.ldap.LDAPEntityMapper
import net.liftweb.common.Loggable
import com.normation.rudder.domain.RudderDit
import com.normation.rudder.repository.WoNodeGroupRepository
import com.normation.rudder.repository.RoNodeGroupRepository
import com.normation.eventlog.EventActor
import com.normation.rudder.domain.nodes.ModifyNodeGroupDiff
import com.normation.inventory.ldap.core.LDAPFullInventoryRepository
import com.normation.inventory.domain.InventoryStatus
import com.normation.inventory.domain.{AcceptedInventory,RemovedInventory}
import com.normation.rudder.repository.EventLogRepository
import com.normation.rudder.services.nodes.NodeInfoService
import com.normation.rudder.domain.eventlog._
import com.normation.utils.ScalaReadWriteLock
import com.normation.rudder.domain.nodes.NodeInfo
import com.normation.inventory.services.core.MachineRepository
import com.normation.inventory.services.core.WriteOnlyMachineRepository
import com.normation.inventory.services.core.ReadOnlyMachineRepository
import com.normation.eventlog.ModificationId
import com.normation.inventory.ldap.core.InventoryHistoryLogRepository
import com.normation.inventory.ldap.core.InventoryDit
import com.normation.ldap.sdk.RwLDAPConnection
import com.normation.utils.Control.sequence
import net.liftweb.util.Helpers.tryo
import com.normation.rudder.repository.CachedRepository
import com.normation.rudder.repository.UpdateExpectedReportsRepository
import com.normation.rudder.hooks.RunHooks
import com.normation.rudder.hooks.HookEnvPairs
import com.normation.rudder.services.policies.write.PathComputer
import com.normation.rudder.services.policies.write.NodePromisesPaths
import com.normation.rudder.domain.Constants



trait RemoveNodeService {

  /**
   * Remove a node from the Rudder
   * For the moment, it really deletes it, later it would be useful to actually move it
   * What it does :
   * - clean the ou=Nodes
   * - clean the groups
   */
  def removeNode(nodeId : NodeId, modId: ModificationId, actor:EventActor) : Box[Seq[LDIFChangeRecord]]
}


class RemoveNodeServiceImpl(
      nodeDit                   : NodeDit
    , rudderDit                 : RudderDit
    , ldap                      : LDAPConnectionProvider[RwLDAPConnection]
    , ldapEntityMapper          : LDAPEntityMapper
    , roNodeGroupRepository     : RoNodeGroupRepository
    , woNodeGroupRepository     : WoNodeGroupRepository
    , nodeInfoService           : NodeInfoService
    , fullNodeRepo              : LDAPFullInventoryRepository
    , actionLogger              : EventLogRepository
    , groupLibMutex             : ScalaReadWriteLock //that's a scala-level mutex to have some kind of consistency with LDAP
    , nodeInfoServiceCache      : NodeInfoService with CachedRepository
    , nodeConfigurationsRepo    : UpdateExpectedReportsRepository
    , pathComputer              : PathComputer
    , HOOKS_D                   : String
    , HOOKS_IGNORE_SUFFIXES     : List[String]
) extends RemoveNodeService with Loggable {

  /*
   * This method is an helper that does the policy server lookup for
   * you.
   */
  def getNodePath(node: NodeInfo): Box[NodePromisesPaths] = {
    //accumumate all the node infos from node id to root throught relay servers
    def recGetParent(node: NodeInfo): Box[Map[NodeId, NodeInfo]] = {
      if(node.id == Constants.ROOT_POLICY_SERVER_ID) {
        Full(Map((node.id, node)))
      } else {
        for {
          opt    <- nodeInfoService.getNodeInfo(node.policyServerId)
          parent <- Box(opt) ?~! s"The policy server '${node.policyServerId.value}' for node ${node.hostname} ('${node.id.value}') was not found in Rudder"
          rec    <- recGetParent(parent)
        } yield {
          rec + (node.id -> node)
        }
      }
    }
    for {
      nodeInfos <- recGetParent(node)
      paths     <- pathComputer.computeBaseNodePath(node.id, Constants.ROOT_POLICY_SERVER_ID, nodeInfos)
    } yield {
      paths
    }
  }

  /**
   * the removal of a node is a multi-step system
   * First, fetch the node, then remove it from groups, and clear all node configurations
   * Move the node to the removed inventory (and don't forget to change its container dn)
   * Then find its container, to see if it has others nodes on it
   *        if so, copy the container to the removed inventory
   *        if not, move the container to the removed inventory
   *
   * Return a couple with 2 boxes, one about the LDIF change, and one containing the result of the clear cache
   * The main goal is to separate the clear cache as it could fail while the node is correctly deleted.
   * A failing clear cache should not be considered an error when deleting a Node.
   */
  def removeNode(nodeId : NodeId, modId: ModificationId, actor:EventActor) : Box[Seq[LDIFChangeRecord]] = {
    logger.debug("Trying to remove node %s from the LDAP".format(nodeId.value))
    nodeId.value match {
      case "root" => Failure("The root node cannot be deleted from the nodes list.")
      case _ => {

        val systemEnv = {
          import scala.collection.JavaConverters._
          HookEnvPairs.build(System.getenv.asScala.toSeq:_*)
        }

        for {
          optNodeInfo   <- nodeInfoService.getNodeInfo(nodeId)

          nodeInfo      <- optNodeInfo match {
                             case None    => Failure(s"The node with id ${nodeId.value} was not found and can not be deleted")
                             case Some(x) => Full(x)
                           }
          nodePaths     <- getNodePath(nodeInfo)
          startPreHooks =  System.currentTimeMillis
          preHooks      <- RunHooks.getHooks(HOOKS_D + "/node-pre-deletion", HOOKS_IGNORE_SUFFIXES)

          //
          // TODO : we need to stop or continu the process given the HookReturnCode,
          // and return an Either[HookReturnCode.Stop, Seq[LDIFChangeRecord]]
          // Then, on the call site at /rudder-web/src/main/scala/com/normation/rudder/web/services/DisplayNode.scala#887
          // we need to check for the kind of Stop. If it is interrupt, then display an adapted pop-up explaining that
          // the node was not removed (but this is a guard, not an error)
          //


          _             <- RunHooks.syncRun(preHooks, HookEnvPairs.build(
                               ("RUDDER_NODEID"             , nodeId.value)
                             , ("RUDDER_NODE_POLICY_SERVER" , nodeInfo.policyServerId.value)
                             , ("RUDDER_NODE_ROLES"         , nodeInfo.serverRoles.map(_.value).mkString(","))
                             , ("RUDDER_POLICIES_DIRECTORY_CURRENT" , nodePaths.baseFolder)
                             , ("RUDDER_POLICIES_DIRECTORY_NEW"     , nodePaths.newFolder)
                             , ("RUDDER_POLICIES_DIRECTORY_ARCHIVE" , nodePaths.backupFolder)
                           ), systemEnv)
          timePreHooks  =  (System.currentTimeMillis - startPreHooks)
          _             =  logger.debug(s"Node-pre-deletion scripts hooks ran in ${timePreHooks} ms")

          moved         <- groupLibMutex.writeLock {atomicDelete(nodeId, modId, actor) } ?~! "Error when archiving a node"
          closed        <- nodeConfigurationsRepo.closeNodeConfigurations(nodeId)
          eventLogged   <- {
                             val invLogDetails = InventoryLogDetails(
                                                     nodeId           = nodeInfo.id
                                                   , inventoryVersion = nodeInfo.inventoryDate
                                                   , hostname         = nodeInfo.hostname
                                                   , fullOsName       = nodeInfo.osDetails.fullName
                                                   , actorIp          = actor.name
                                                 )
                              val eventlog = DeleteNodeEventLog.fromInventoryLogDetails(
                                                principal = actor
                                              , inventoryDetails = invLogDetails
                                             )
                              actionLogger.saveEventLog(modId, eventlog)
                            }
        } yield {
          //clear node info cached
          nodeInfoServiceCache.clearCache
          // run post-deletion hooks
          val postHooksTime =  System.currentTimeMillis
          for {
            postHooks             <- RunHooks.getHooks(HOOKS_D + "/node-post-deletion", HOOKS_IGNORE_SUFFIXES)
            _                     <- RunHooks.syncRun(postHooks, HookEnvPairs.build(
                                           ("RUDDER_NODEID"             , nodeId.value)
                                         , ("RUDDER_NODE_POLICY_SERVER" , nodeInfo.policyServerId.value)
                                         , ("RUDDER_NODE_ROLES"         , nodeInfo.serverRoles.map(_.value).mkString(","))
                                         , ("RUDDER_POLICIES_DIRECTORY_CURRENT" , nodePaths.baseFolder)
                                         , ("RUDDER_POLICIES_DIRECTORY_NEW"     , nodePaths.newFolder)
                                         , ("RUDDER_POLICIES_DIRECTORY_ARCHIVE" , nodePaths.backupFolder)
                                       ), systemEnv
                                     )
          } yield {
            ()
          }
          val timeRunPostGenHooks   =  (System.currentTimeMillis - postHooksTime)
          logger.debug(s"Node-post-deletion scripts hooks ran in ${timePreHooks} ms")
          moved
        }
      }
    }
  }


  private[this] def atomicDelete(nodeId : NodeId, modId: ModificationId, actor:EventActor) : Box[Seq[LDIFChangeRecord]] = {
    for {
      cleanGroup            <- deleteFromGroups(nodeId, modId, actor) ?~! "Could not remove the node '%s' from the groups".format(nodeId.value)
      cleanNode             <- deleteFromNodes(nodeId) ?~! "Could not remove the node '%s' from the nodes list".format(nodeId.value)
      moveNodeInventory     <- fullNodeRepo.move(nodeId, AcceptedInventory, RemovedInventory)
    } yield {
      cleanNode ++ moveNodeInventory
    }
  }

  /**
   * Deletes from ou=Node
   */
  private def deleteFromNodes(nodeId:NodeId) : Box[Seq[LDIFChangeRecord]]= {
    logger.debug("Trying to remove node %s from ou=Nodes".format(nodeId.value))
    for {
      con    <- ldap
      dn     =  nodeDit.NODES.NODE.dn(nodeId.value)
      result <- con.delete(dn)
    } yield {
      result
    }
  }

   /**
   * Delete all node cnfiguration
   */

  /**
   * Look for the groups containing this node in their nodes list, and remove the node
   * from the list
   */
  private def deleteFromGroups(nodeId: NodeId, modId: ModificationId, actor:EventActor): Box[Seq[ModifyNodeGroupDiff]]= {
    logger.debug("Trying to remove node %s from all the groups were it is referenced".format(nodeId.value))
    for {
      nodeGroupIds <- roNodeGroupRepository.findGroupWithAnyMember(Seq(nodeId))
      deleted      <- sequence(nodeGroupIds) { nodeGroupId =>
                        for {
                          nodeGroup    <- roNodeGroupRepository.getNodeGroup(nodeGroupId).map(_._1)
                          updatedGroup =  nodeGroup.copy(serverList = nodeGroup.serverList - nodeId)
                          msg          =  Some("Automatic update of group due to deletion of node " + nodeId.value)
                          diff         <- (if(nodeGroup.isSystem) {
                                            woNodeGroupRepository.updateSystemGroup(updatedGroup, modId, actor, msg)
                                          } else {
                                            woNodeGroupRepository.update(updatedGroup, modId, actor, msg)
                                          }) ?~! "Could not update group %s to remove node '%s'".format(nodeGroup.id.value, nodeId.value)
                        } yield {
                          diff
                        }
                      }
    } yield {
      deleted.flatten
    }
  }


}
