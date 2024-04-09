/*
 *************************************************************************************
 * Copyright 2011 Normation SAS
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

import com.normation.box.*
import com.normation.errors.IOResult
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.queries.Query
import com.normation.rudder.facts.nodes.CoreNodeFact
import com.normation.rudder.facts.nodes.QueryContext
import net.liftweb.common.Box
import zio.Chunk

trait QueryProcessor {

  /**
   * Process a query and (hopefully) return the list of entry that match it.
   * @param query - the query to process
   */
  def process(query: Query): Box[Seq[NodeId]] = processPure(query).map(_.map(_.id)).toBox

  /**
   * Only get node ids corresponding to that request, with minimal consistency check.
   * This method is useful to maximize performance (low memory, high throughout) for ex for dynamic groups.
   */
  def processOnlyId(query:     Query): Box[Seq[NodeId]]      = processOnlyIdPure(query).toBox
  def processOnlyIdPure(query: Query): IOResult[Seq[NodeId]] = processPure(query).map(_.map(_.id))
  def processPure(query:       Query): IOResult[Chunk[CoreNodeFact]]
}

trait QueryChecker {

  /**
   * Each server denoted by its id is tested against query to see if it
   * matches. Return the list of matching server ids.
   * @param Query
   *    the query to test
   * @param Seq[NodeId]
   *    list of server which have to be tested for query
   * @return
   *   Empty or Failure in case of a error during the process
   *   Full(seq) with seq being the list of nodeId which verify
   *   query.
   */
  def check(query: Query, nodeIds: Option[Seq[NodeId]])(implicit qc: QueryContext): IOResult[Set[NodeId]]

}
