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

package com.normation.rudder.services.queries

import com.normation.box._
import com.normation.errors.IOResult
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.logger.FactQueryProcessorPure
import com.normation.rudder.domain.nodes.NodeFact
import com.normation.rudder.domain.nodes.NodeKind
import com.normation.rudder.domain.queries.And
import com.normation.rudder.domain.queries.CriterionLine
import com.normation.rudder.domain.queries.NodeAndRootServerReturnType
import com.normation.rudder.domain.queries.Query
import com.normation.rudder.domain.queries.ResultTransformation
import net.liftweb.common.Box
import zio._
import zio.syntax._

trait NodeFactRepository {
  def getAll: IOResult[Chunk[NodeFact]]
}

/*
 * A NodeFactMatcher is the transformation of a query into a method that is able to
 * eval a NodeFact for that Query.
 * It takes into account:
 * - the different case (node, root, invert, etc)
 * - the query criteria
 *
 * NodeFactMatcher is a group for AND and for OR
 */
final case class NodeFactMatcher(debugString: String, matches: NodeFact => IOResult[Boolean])

object NodeFactMatcher {
  val nodeAndRelayMatcher = {
    val s = "only matches node and relay"
    NodeFactMatcher(
      s,
      (n: NodeFact) => {
        for {
          res <- (n.rudderSettings.kind != NodeKind.Root).succeed
          _   <- FactQueryProcessorPure.trace(s"    - [${res}] for $s on '${n.rudderSettings.kind}'")
        } yield res
      }
    )
  }
}

trait Group {
  def compose(a: NodeFactMatcher, b: NodeFactMatcher): NodeFactMatcher
  def inverse(a: NodeFactMatcher): NodeFactMatcher
  def zero: NodeFactMatcher
}
object GroupAnd extends Group {
  def compose(a: NodeFactMatcher, b: NodeFactMatcher) = {
    (a, b) match {
      case (`zero`, _) => b
      case (_, `zero`) => a
      case _           => NodeFactMatcher(s"(${a.debugString}) && (${b.debugString})", (n: NodeFact) => (a.matches(n) && b.matches(n)))
    }
  }
  def inverse(a: NodeFactMatcher)                     = NodeFactMatcher(s"!(${a.debugString})", (n: NodeFact) => a.matches(n).map(!_))
  val zero                                            = NodeFactMatcher("true", _ => true.succeed)
}
object GroupOr extends Group {
  def compose(a: NodeFactMatcher, b: NodeFactMatcher) = {
    (a, b) match {
      case (`zero`, _) => b
      case (_, `zero`) => a
      case _           => NodeFactMatcher(s"(${a.debugString}) || (${b.debugString})", (n: NodeFact) => (a.matches(n) || b.matches(n)))
    }
  }
  def inverse(a: NodeFactMatcher)                     = NodeFactMatcher(s"!(${a.debugString})", (n: NodeFact) => a.matches(n).map(!_))
  val zero                                            = NodeFactMatcher("false", _ => false.succeed)
}

class NodeFactQueryProcessor(nodeFactRepo: NodeFactRepository) extends QueryProcessor with QueryChecker {

  def process(query: Query):       Box[Seq[NodeId]] = processPure(query).map(_.toList.map(_.id)).toBox
  def processOnlyId(query: Query): Box[Seq[NodeId]] = processPure(query).map(_.toList.map(_.id)).toBox

  def check(query: Query, nodeIds: Option[Seq[NodeId]]): IOResult[Set[NodeId]]     = { ??? }
  def processPure(query: Query):                         IOResult[Chunk[NodeFact]] = {
    val m = analyzeQuery(query)
    nodeFactRepo.getAll.flatMap(nodes => {
      FactQueryProcessorPure.debug(m.debugString) *>
      ZIO.foreach(nodes)(node => processOne(m, node).map(b => if (b) Some(node) else None)).map(_.flatten)
    })
  }

  /*
   * transform the query into a function to apply to a NodeFact and that say "yes" or "no"
   */
  def analyzeQuery(query: Query): NodeFactMatcher = {
    val group      = if (query.composition == And) GroupAnd else GroupOr
    // build matcher for criterion lines
    val lineResult =
      query.criteria.foldLeft(group.zero)((matcher, criterion) => group.compose(matcher, analyseCriterion(criterion)))
    // inverse now if needed, because we don't want to return root if not asked *even* when inverse is present
    val inv        = if (query.transform == ResultTransformation.Invert) group.inverse(lineResult) else lineResult
    // finally, filter out root if need
    val res        =
      if (query.returnType == NodeAndRootServerReturnType) inv else GroupAnd.compose(NodeFactMatcher.nodeAndRelayMatcher, inv)
    res
  }

  def analyseCriterion(c: CriterionLine): NodeFactMatcher = {
    NodeFactMatcher(
      s"[${c.objectType.objectType}.${c.attribute.name} ${c.comparator.id} ${c.value}]",
      (n: NodeFact) => c.attribute.nodeCriterionMatcher.matches(n, c.comparator, c.value)
    )
  }

  def processOne(matcher: NodeFactMatcher, n: NodeFact): IOResult[Boolean] = {
    for {
      res <- matcher.matches(n)
      _   <- FactQueryProcessorPure.debug(s"  - on '${n.fqdn}'(${n.id.value}): ${res}")
    } yield res
  }

}
