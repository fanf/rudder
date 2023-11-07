/*
 *************************************************************************************
 * Copyright 2023 Normation SAS
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

package com.normation.rudder.facts.nodes

import com.normation.errors.IOResult
import com.normation.rudder.domain.nodes.ModifyNodeDiff
import com.normation.rudder.facts.nodes.MinimalNodeFactInterface.toNode
import com.normation.rudder.repository.EventLogRepository
import com.normation.rudder.services.reports.CacheComplianceQueueAction
import com.normation.rudder.services.reports.CacheExpectedReportAction
import com.normation.rudder.services.reports.InvalidateCache
import zio.ZIO

/*
 * This file store callbacks for node events.
 * The canonical example case is event log records
 */

trait NodeFactChangeEventCallback[A <: MinimalNodeFactInterface] {
  def name: String
  def run(change: NodeFactChangeEventCC[A]): IOResult[Unit]
}

case class CoreNodeFactChangeEventCallback(
    name: String,
    exec: NodeFactChangeEventCC[MinimalNodeFactInterface] => IOResult[Unit]
) extends NodeFactChangeEventCallback[MinimalNodeFactInterface] {
  override def run(change: NodeFactChangeEventCC[MinimalNodeFactInterface]): IOResult[Unit] = {
    exec(change)
  }
}

/*
 * Manage event logs related to nodes: register a change in properties, a node acceptation, etc
 */
class EventLogsNodeFactChangeEventCallback(
    actionLogger: EventLogRepository
) extends NodeFactChangeEventCallback[MinimalNodeFactInterface] {
  override def name: String = "node-fact-cec: register even log"

  override def run(change: NodeFactChangeEventCC[MinimalNodeFactInterface]): IOResult[Unit] = {
    def modifyEventLog(
        cc:   ChangeContext,
        old:  MinimalNodeFactInterface,
        next: MinimalNodeFactInterface
    ): IOResult[Unit] = {
      val diff = ModifyNodeDiff(toNode(old), toNode(next))
      actionLogger.saveModifyNode(cc.modId, cc.actor, diff, cc.message).unit
    }

    change.event match {
      case NodeFactChangeEvent.UpdatedPending(old, next) => modifyEventLog(change.cc, old, next)
      case NodeFactChangeEvent.Updated(old, next)        => modifyEventLog(change.cc, old, next)
      case NodeFactChangeEvent.NewPending(node)          => ???
      case NodeFactChangeEvent.Accepted(node)            => ???
      case NodeFactChangeEvent.Refused(node)             => ???
      case NodeFactChangeEvent.Deleted(node)             => ???
      case NodeFactChangeEvent.Noop(nodeId)              => ZIO.unit
    }
  }
}

/*
 * Callback related to cache invalidation when a node changes
 */
class CacheInvalidateNodeFactEventCallback(
    cacheExpectedReports: InvalidateCache[CacheExpectedReportAction],
    cacheConfiguration:   InvalidateCache[CacheComplianceQueueAction]
) extends NodeFactChangeEventCallback[MinimalNodeFactInterface] {
  override def name: String = "node-fact-cec: invalidate caches"

  override def run(change: NodeFactChangeEventCC[MinimalNodeFactInterface]): IOResult[Unit] = {
    change.event match {
      case NodeFactChangeEvent.NewPending(node)                 => ???
      case NodeFactChangeEvent.UpdatedPending(oldNode, newNode) => ???
      case NodeFactChangeEvent.Accepted(node)                   =>
        val a = CacheExpectedReportAction.InsertNodeInCache(node.id)
        for {
          _ <- cacheConfiguration.invalidateWithAction(Seq((node.id, CacheComplianceQueueAction.ExpectedReportAction(a))))
          _ <- cacheExpectedReports.invalidateWithAction(Seq((node.id, a)))
        } yield ()
      case NodeFactChangeEvent.Refused(node)                    => ???
      case NodeFactChangeEvent.Updated(oldNode, newNode)        => ???
      case NodeFactChangeEvent.Deleted(node)                    =>
        val a = CacheExpectedReportAction.RemoveNodeInCache(node.id)
        for {
          _ <- cacheConfiguration.invalidateWithAction(Seq((node.id, CacheComplianceQueueAction.ExpectedReportAction(a))))
          _ <- cacheExpectedReports.invalidateWithAction(Seq((node.id, a)))
        } yield ()

      case NodeFactChangeEvent.Noop(nodeId) => ZIO.unit
    }
  }
}
