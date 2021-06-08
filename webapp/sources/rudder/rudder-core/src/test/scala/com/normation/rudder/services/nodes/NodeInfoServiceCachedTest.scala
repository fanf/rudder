package com.normation.rudder.services.nodes

import com.normation.errors.IOResult
import com.normation.inventory.domain.{InventoryStatus, MachineUuid, NodeId}
import com.normation.inventory.ldap.core.{InventoryDit, InventoryMapper}
import com.normation.inventory.ldap.core.LDAPConstants.{A_CONTAINER_DN, A_DESCRIPTION, A_HOSTNAME, A_NAME, A_NODE_UUID, A_POLICY_SERVER_UUID}
import com.normation.ldap.sdk.LDAPIOResult.LDAPIOResult
import com.normation.ldap.sdk.{LDAPConnectionProvider, LDAPEntry, RoLDAPConnection}
import com.normation.rudder.domain.{NodeDit, RudderDit}
import com.normation.rudder.domain.nodes.{MachineInfo, Node, NodeInfo}
import com.normation.rudder.domain.nodes.NodeState.Enabled
import com.normation.rudder.repository.ldap.LDAPEntityMapper
import com.unboundid.ldap.sdk.{DN, RDN}
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import scala.collection.mutable.{Map => MutMap}

/*
 * Test the cache behaviour
 */
@RunWith(classOf[JUnitRunner])
class NodeInfoServiceCachedTest extends Specification {

  sequential

  var nodeCache = Option.empty[LocalNodeInfoCache]

  def DN(rdn: String, parent: DN) = new DN(new RDN(rdn),  parent)
  val LDAP_BASEDN = new DN("cn=rudder-configuration")
  val LDAP_INVENTORIES_BASEDN = DN("ou=Inventories", LDAP_BASEDN)
  val LDAP_INVENTORIES_SOFTWARE_BASEDN = LDAP_INVENTORIES_BASEDN

  val rudderDit = new RudderDit(DN("ou=Rudder", LDAP_BASEDN))
  val nodeDit = new NodeDit(new DN("cn=rudder-configuration"))
  val inventoryDit = new InventoryDit(DN("ou=Accepted Inventories", DN("ou=Inventories", LDAP_BASEDN)), LDAP_INVENTORIES_SOFTWARE_BASEDN, "Accepted inventories")

  val nodeInfoService = new NodeInfoServiceCached() {
    override def ldap: LDAPConnectionProvider[RoLDAPConnection] = ???
    override def nodeDit: NodeDit = ???
    override def inventoryDit: InventoryDit = ???
    override def removedDit: InventoryDit = ???
    override def pendingDit: InventoryDit = ???
    override def ldapMapper: LDAPEntityMapper = ???
    override def inventoryMapper: InventoryMapper = ???
    override protected[this] def checkUpToDate(lastKnowModification: DateTime, lastModEntryCSN: Seq[String]): IOResult[Boolean] = ???
    override def getNodeInfoEntries(con: RoLDAPConnection, attributes: Seq[String], status: InventoryStatus): LDAPIOResult[Seq[LDAPEntry]] = ???
    override def getNewNodeInfoEntries(con: RoLDAPConnection, lastKnowModification: DateTime, searchAttributes: Seq[String]): LDAPIOResult[Seq[LDAPEntry]] = ???
    def setNodeCache(newCache : Option[LocalNodeInfoCache]) = {
      nodeCache = newCache
    }
  }
  def createNodeInfo(
      id: NodeId
    , machineUuid: Option[MachineUuid]) : NodeInfo = {
    NodeInfo(
        Node(id, id.value, id.value, Enabled, false, false, new DateTime(), null, null, null)
      , id.value
      , machineUuid.map(x => MachineInfo(x, null, None, None)), null, List(), new DateTime(0), null, Seq(), NodeId("root"), "root", Set(), None, None, None
    )
  }

  // create the ldap node ifo, with an option for the machine entry (which is not mandatory)
  def createLdapNodeInfo(
     node: NodeInfo
  ) : LDAPNodeInfo = {
    val nodeEntry = nodeDit.NODES.NODE.nodeModel(node.id)
    nodeEntry +=! (A_NAME, node.name)
    nodeEntry +=! (A_DESCRIPTION, node.name)

    val machineEntry = node.machine.map(x => inventoryDit.MACHINES.MACHINE.model(x.id))

    val nodeInvEntry = inventoryDit.NODES.NODE.genericModel(node.id)

    nodeInvEntry +=! (A_HOSTNAME, node.name)
    nodeInvEntry +=! (A_POLICY_SERVER_UUID, node.policyServerId.value)
    machineEntry.map( mac => nodeInvEntry +=! (A_CONTAINER_DN, mac.dn.toString))

    LDAPNodeInfo(nodeEntry, nodeInvEntry, machineEntry)

  }

  " with a standard cache " should {
    val nodes = Map("1" -> Some("M1")
                  , "2" -> Some("M2")
                  , "3" -> Some("M3")
                  , "4" -> None
                  , "5" -> None )

    val nodeInfos = nodes.map { case (id, machineId) =>
      createNodeInfo(NodeId(id), machineId.map(MachineUuid(_)))
    }

    val ldapNodesInfos = nodeInfos.map { case nodeinfo =>
      (nodeinfo.id, (createLdapNodeInfo(nodeinfo), nodeinfo))
    }.toMap

    nodeInfoService.setNodeCache(Some(LocalNodeInfoCache(ldapNodesInfos, new DateTime(), Seq(), ldapNodesInfos.size)))

    " be idempotent" in {
      val truc = ldapNodesInfos.values.map(_._1).map(x => (x.nodeEntry, x.nodeInventoryEntry, x.machineEntry))

      val nodeEntries = MutMap() ++ (for {
        entry <- truc
        nodeEntry = entry._1
      } yield {
        (nodeEntry.value_!(A_NODE_UUID), nodeEntry)
      }).toMap
      val nodeInventoriesEntries = MutMap() ++ (for {
        entry <- truc
        nodeInventoryEntry = entry._2
      } yield {
        (nodeInventoryEntry.value_!(A_NODE_UUID), nodeInventoryEntry)
      }).toMap
      val machineEntries = MutMap() ++ (for {
        entry <- truc
        machineEntry <- entry._3
        machineDn = machineEntry.dn.toString
      } yield {
        (machineDn, machineEntry)
      }).toMap
println(nodeEntries)
println(nodeInventoriesEntries)
      println(machineEntries)
      val ldap = nodeInfoService.constructNodes(nodeEntries, nodeInventoriesEntries, machineEntries)

      ldapNodesInfos.values.map(_._1).toSeq === ldap
    }
  }





}
