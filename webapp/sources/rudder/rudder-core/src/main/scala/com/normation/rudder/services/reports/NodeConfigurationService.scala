/*
*************************************************************************************
* Copyright 2021 Normation SAS
*************************************************************************************
*
* This file is part of Rudder.
*
* Rudder is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU General Public License version 3, the copyright holders add
* the following Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU General
* Public License version 3, when you create a Related Module, this
* Related Module is not considered as a part of the work and may be
* distributed under the license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
*
* Rudder is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Rudder.  If not, see <http://www.gnu.org/licenses/>.

*
*************************************************************************************
*/

package com.normation.rudder.services.reports
import com.normation.box._
import com.normation.errors._
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.logger.{ReportLogger, ReportLoggerPure}
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.reports.{NodeAndConfigId, NodeExpectedReports}
import com.normation.rudder.repository.{CachedRepository, FindExpectedReportRepository}
import com.normation.rudder.services.nodes.NodeInfoService
import com.normation.zio._
import net.liftweb.common._
import zio._
import zio.syntax._

/**
 * That service retrieve node configurations (nodeexpectedreports) from the expectedreportsjdbcrepository, unless its already in cache
 * cache is driven by reporting serviceimpl
 * init add all nodes, withtout anything attached
 * initial setting of nodeexpectedreport is less prioritary than update
 * deletion removes the entry
 * if an entry exists but without anything, then it will query the database
 * if an entry exists but wthout the right nodeconfigid, it will query the database (but not update the cache)
 */
trait NodeConfigurationService {
  /**
   * retrieve expected reports by config version
   */
  def findNodeExpectedReports(
         nodeConfigIds: Set[NodeAndConfigId]
      ): IOResult[Map[NodeAndConfigId, Option[NodeExpectedReports]]]

  /**
   * get the current expected reports
   * fails if request expected reports for a non existent node
   */
  def getCurrentExpectedReports(nodeIds: Set[NodeId]): Box[Map[NodeId, Option[NodeExpectedReports]]]

  /**
   * get the nodes applying the rule
   *
   */
  def findNodesApplyingRule(ruleId: RuleId): Box[Set[NodeId]]
}

class CachedNodeConfigurationService(
    val confExpectedRepo: FindExpectedReportRepository
  , val nodeInfoService : NodeInfoService
  ) extends NodeConfigurationService with CachedRepository {


  val semaphore = Semaphore.make(1).runNow

  val logEffect = ReportLogger.Cache


  /**
   * The cache is managed node by node.
   * A missing nodeId mean that the cache wasn't initialized for
   * that node, and should fail
   *
   * This cache is populated by ReportingServiceImpl:
   * * init by adding all existing nodes (and NodeExpectedReports if available)
   * * update after a policy generation to change the value for a nodeid
   * * add a new node when accepting a node
   * * deleting a node
   *
   * Note that a clear cache will None the cache
   *
   * A query to fetch nodeexpectedreports that is not in the cache will return None
   * (if node exists), or fail if node does not exists
   */
  private[this] var cache = Map.empty[NodeId, Option[NodeExpectedReports]]

  /**
   * The queue of invalidation request.
   * The queue size is 1 and new request need to merge with existing request
   * It's a List and not a Set, because we want to keep the precedence in
   * invalidation request.
   * // unsure if its a CacheComplianceQueueAction or another queueaction
   */
  private[this] val invalidateNodeConfigurationRequest = Queue.dropping[List[(NodeId, CacheComplianceQueueAction)]](1).runNow

  /**
   * We need a semaphore to protect queue content merge-update
   */
  private[this] val invalidateMergeUpdateSemaphore = Semaphore.make(1).runNow


  // Init to do
  // what's the best method ? init directly from db, fetching all nodeconfigurations
  // that are empty
  // or initing it with all nodes, and nothing in it, and only then fetching by batch
  // batching saves memory, but is slower
  // i think it's safer to do the batching part, but i'd like to be sure of that
  def init() : IOResult[Unit] = {
    logEffect.debug("Init cache in NodeConfigurationService")
    for {
      // first, get all nodes
      nodeIds <- nodeInfoService.getAllNodesIds()
      _       <- semaphore.withPermit {
        // void the cache
        IOResult.effect({ cache = nodeIds.map(_ -> None).toMap })
      }
    } yield ()

  }
  /**
   * Update logic. We take message from queue one at a time, and process.
   * we need to keep order
   */
  val updateCacheFromRequest: IO[Nothing, Unit] = invalidateNodeConfigurationRequest.take.flatMap(invalidatedIds =>
    ZIO.foreach_(invalidatedIds.map(_._2) : List[CacheComplianceQueueAction])(action =>
    {
      (for {
        _  <- performAction(action)
      } yield ()).catchAll(err => ReportLoggerPure.Cache.error(s"Error when updating NodeConfiguration cache for node: [${action.nodeId.value}]: ${err.fullMsg}"))
    }
    )
  )
  // start updating
  updateCacheFromRequest.forever.forkDaemon.runNow
  /**
   * Clear cache. Try a reload asynchronously, disregarding
   * the result
   */
  override def clearCache(): Unit = {
    init().runNow
    logEffect.debug("Node expected reports cache cleared")
  }


  /**
   * Do something with the action we received
   */
  private[this] def performAction(action: CacheComplianceQueueAction): IOResult[Unit] = {
    import CacheComplianceQueueAction._
    // in a semaphore
    semaphore.withPermit(
       action match {
                          case insert: InsertNodeInCache => IOResult.effectNonBlocking {  cache = cache + (insert.nodeId -> None) }
                          case delete: RemoveNodeInCache => IOResult.effectNonBlocking {  cache = cache.removed(delete.nodeId) }
                          case update: UpdateNodeConfiguration => IOResult.effectNonBlocking {  cache = cache + (update.nodeId -> Some(update.nodeConfiguration)) }
                          case something =>
                            Inconsistency(s"NodeConfiguration service cache received unknown command : ${something}").fail
                           // should not happen
                   }
    )
  }

  /**
   * invalidate with an action to do something
   * order is important
   */
  def invalidateWithAction(actions: Seq[(NodeId, CacheComplianceQueueAction)]): IOResult[Unit] = {
    ZIO.when(actions.nonEmpty) {
      ReportLoggerPure.Cache.debug(s"Node Configuration cache: invalidation request for nodes with action: [${actions.map(_._1).map { _.value }.mkString(",")}]") *>
        invalidateMergeUpdateSemaphore.withPermit(for {
          elements     <- invalidateNodeConfigurationRequest.takeAll
          allActions   =  (elements.flatten ++ actions)
          _            <- invalidateNodeConfigurationRequest.offer(allActions)
        } yield ())
    }
  }
  // ? question ?
  // how to properly ensure that cache is synchro ?
  // we have the begin date of the nodeexpectedreport that my offer a way to ensure that we don't replace
  // a value with something older

  // Merge the data with the cache
  // Strategy:
  // * if nodeId is not in cache, remove the nodeId
  // * if the nodeId is in cache, and value is None, augment cache
  // * if the nodeid is in cache, and value is not None, pick the most recent value to augment cache
  // Returns the values merged with cache
  private[this] def mergeDataFromDBWithCache(fromDb: Map[NodeId, Option[NodeExpectedReports]]):IOResult[ Map[NodeId, Option[NodeExpectedReports]] ] = {
    // In a semaphore, nothing should change the cache
    semaphore.withPermit(
      IOResult.effect({
      val mergedEntries = fromDb.map {  case (nodeId, expFromDb) => cache.get(nodeId) match {
        case None => None // nodeid not in cache, dropping
        case Some(value) => value match { // node id in cache
          case None => Some((nodeId -> expFromDb)) // with no value, the one from DB is better (even if none)
          case Some(entry) => // now we must compare with the one from DB
            expFromDb match {
              case None => Some((nodeId -> value))
              case Some(db) if db.beginDate.isAfter(entry.beginDate) => Some((nodeId -> expFromDb)) // data fom db is newer
              case _ => // value from cache is newer
                Some((nodeId -> value))
            }
        }
      }}.flatten.toMap
      // Now merge with the cache
      cache = cache ++ mergedEntries

      // returns the mergedEntries
      mergedEntries
    })
    )
  }
  /**
   * get the current expected reports
   */
  def getCurrentExpectedReports(nodeIds: Set[NodeId]): Box[Map[NodeId, Option[NodeExpectedReports]]] = {
    // First, get all nodes from cache (even the none)
    val dataFromCache = cache.filter{  case (nodeId, _) => nodeIds.contains(nodeId) }

    // now fetch others from database, if necessary
    // if the configuration is none, then cache isn't inited for it
    val dataUnitialized = dataFromCache.filter{ case (nodeId, option) => option.isEmpty}.keySet

    for {
      fromDb     <- confExpectedRepo.getCurrentExpectedsReports(dataUnitialized)
      _          = logEffect.trace(s"Fetch from DB ${fromDb.size} current expected reports")
      mergedData <- mergeDataFromDBWithCache(fromDb).toBox
    } yield {
      dataFromCache ++ mergedData
    }
  }

  /**
   * get the nodes applying the rule
   *
   */
  def findNodesApplyingRule(ruleId: RuleId): Box[Set[NodeId]] = {
    // I wanted to put this in the semaphore, but couldn't get the types to works
    // check if any node is not in cache
    val nodesNotInCache = cache.filter{ case (_, value) => value.isEmpty }.keySet

    val dataFromCache = cache.filter { case (_, option) => option match {
      case None => false
      case Some(nodeExpectedReports) =>
        nodeExpectedReports.ruleExpectedReports.map(_.ruleId).contains(ruleId)
    }}.keySet

    if (nodesNotInCache.isEmpty) {
      Full(dataFromCache)
    } else {
      // query the repo and merge
      for {
        fromRepo <- confExpectedRepo.findCurrentNodeIdsForRule(ruleId, nodesNotInCache)
      } yield {
        dataFromCache ++ fromRepo
      }
    }
  }


  /**
   * retrieve expected reports by config version
   */
  def findNodeExpectedReports(
      nodeConfigIds: Set[NodeAndConfigId]
    ): IOResult[Map[NodeAndConfigId, Option[NodeExpectedReports]]] = {
    // first get them in cache
    val inCache = cache.map { case (id, expected) => expected match {
      case None => None
      case Some(nodeExpectedReport) =>
        val nodeAndConfigId = NodeAndConfigId(id, nodeExpectedReport.nodeConfigId)
        if (nodeConfigIds.contains(nodeAndConfigId)) {
          Some((nodeAndConfigId, expected)) // returns from the cache if it match
        } else {
          None
        }
    }}.flatten.toMap

    // search for all others in repo
    // here, we could do something clever by filtering all those with None enddate and add them in repo
    // but I don't want to be double clever
    val missingNodeConfigIds = nodeConfigIds -- inCache.keySet

    for {
      fromDb <- confExpectedRepo.getExpectedReports(missingNodeConfigIds)
    } yield {
      fromDb ++ inCache
    }
  }.toIO
}



/**
 * simple implementation
 * simply call the repo, as a passthrough
 */
class NodeConfigurationServiceImpl(
  confExpectedRepo: FindExpectedReportRepository
) extends NodeConfigurationService {
  /**
   * retrieve expected reports by config version
   */
  def findNodeExpectedReports(
        nodeConfigIds: Set[NodeAndConfigId]
      ): IOResult[Map[NodeAndConfigId, Option[NodeExpectedReports]]] = {
    confExpectedRepo.getExpectedReports(nodeConfigIds).toIO
  }

  /**
   * get the current expected reports
   * fails if request expected reports for a non existent node
   */
  def getCurrentExpectedReports(nodeIds: Set[NodeId]): Box[Map[NodeId, Option[NodeExpectedReports]]] = {
    confExpectedRepo.getCurrentExpectedsReports(nodeIds)
  }

  /**
   * get the nodes applying the rule
   *
   */
  def findNodesApplyingRule(ruleId: RuleId): Box[Set[NodeId]] = {
    confExpectedRepo.findCurrentNodeIds(ruleId)
  }
}

