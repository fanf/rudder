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
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.nodes.Node
import com.normation.rudder.domain.nodes.NodeInfo
import com.normation.rudder.domain.nodes.NodeKind
import com.normation.rudder.services.nodes.NodeInfoService

import zio._
import zio.stream.ZSink
import zio.syntax._

/*
 * Proxy for node fact to full inventory / node inventory / machine inventory / node info and their repositories
 */
class NodeInfoServiceProxy(backend: NodeFactRepository) extends NodeInfoService {
  override def getNodeInfo(nodeId: NodeId): IOResult[Option[NodeInfo]] = {
    backend.get(nodeId).map(_.map(_.toNodeInfo))
  }

  override def getNodeInfos(nodeIds: Set[NodeId]): IOResult[Set[NodeInfo]] = {
    backend.getAll().collect { case n if (nodeIds.contains(n.id)) => n.toNodeInfo }.run(ZSink.collectAllToSet)
  }

  override def getNodeInfosSeq(nodeIds: Seq[NodeId]): IOResult[Seq[NodeInfo]] = {
    backend.getAll().collect { case n if (nodeIds.contains(n.id)) => n.toNodeInfo }.run(ZSink.collectAll).map(_.toSeq)
  }

  override def getNumberOfManagedNodes: IOResult[Int] = {
    backend.getAll().run(ZSink.count).map(_.toInt)
  }

  override def getAll(): IOResult[Map[NodeId, NodeInfo]] = {
    backend.getAll().map(_.toNodeInfo) run (ZSink.collectAllToMap[NodeInfo, NodeId](_.node.id)((a, b) => b))
  }

  override def getAllNodesIds(): IOResult[Set[NodeId]] = {
    backend.getAll().map(_.id).run(ZSink.collectAllToSet)
  }

  override def getAllNodes(): IOResult[Map[NodeId, Node]] = {
    backend.getAll().map(_.toNode).run(ZSink.collectAllToMap[Node, NodeId](_.id)((a, b) => b))
  }

  override def getAllNodeInfos(): IOResult[Seq[NodeInfo]] = {
    backend.getAll().map(_.toNodeInfo).run(ZSink.collectAll).map(_.toSeq)
  }

  override def getAllSystemNodeIds(): IOResult[Seq[NodeId]] = {
    backend.getAll().collect { case n if (n.rudderSettings.kind != NodeKind.Node) => n.id }.run(ZSink.collectAll).map(_.toSeq)
  }

  override def getPendingNodeInfos(): IOResult[Map[NodeId, NodeInfo]] = {
    backend.getAllPending().map(_.toNodeInfo).run(ZSink.collectAllToMap[NodeInfo, NodeId](_.id)((a,b) => b))
  }

  override def getPendingNodeInfo(nodeId: NodeId): IOResult[Option[NodeInfo]] = {
    backend.getPending(nodeId).map(_.map(_.toNodeInfo))
  }

  // not supported anymore
  override def getDeletedNodeInfos(): IOResult[Map[NodeId, NodeInfo]] = {
    Map().succeed
  }

  // not supported anymore
  override def getDeletedNodeInfo(nodeId: NodeId): IOResult[Option[NodeInfo]] = {
    None.succeed
  }
}

