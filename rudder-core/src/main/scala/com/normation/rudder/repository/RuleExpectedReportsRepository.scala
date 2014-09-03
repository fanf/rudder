/*
*************************************************************************************
* Copyright 2011 Normation SAS
*************************************************************************************
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU Affero General Public License as
* published by the Free Software Foundation, either version 3 of the
* License, or (at your option) any later version.
*
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU Affero GPL v3, the copyright holders add the following
* Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU Affero GPL v3
* licence, when you create a Related Module, this Related Module is
* not considered as a part of the work and may be distributed under the
* license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/agpl.html>.
*
*************************************************************************************
*/

package com.normation.rudder.repository
import org.joda.time._

import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.reports._

import net.liftweb.common.Box


trait UpdateExpectedReportsRepository {
  /**
   * Return the ruleId currently opened, and their serial and list of nodes
   * It is only used to know which conf expected report we should close
   *
   * For only the last version (and so only one NodeConfigId) is
   * returned for each nodeJoinKey
   */
  //only for update logic
  def findAllCurrentExpectedReportsWithNodesAndSerial(): Map[RuleId, (Int, Int, Map[NodeId, NodeConfigVersions])]


 /**
   * Simply set the endDate for the expected report for this conf rule
   */
  //only for update logic
  def closeExpectedReport(ruleId : RuleId) : Box[Unit]

  /**
   * Insert new expectedReports in base.
   * Not that expectedReports are never "updated". Old
   * one are closed and new one are created (aka saved')
   */
  //only for update logic
  def saveExpectedReports(
      ruleId                   : RuleId
    , serial                   : Int
    , directiveExpectedReports : Seq[DirectiveExpectedReports]
    , nodeConfigurationVersions: Seq[NodeConfigId]
  ) : Box[RuleExpectedReports]


  /**
   * Update the list of nodeConfigVersion for the given nodes
   */
  //only for update logic
  def updateNodeConfigVersion(toUpdate: Seq[(Int, NodeConfigVersions)]): Box[Seq[(Int,NodeConfigVersions)]]
}




trait FindExpectedReportRepository {

  /**
   * Return all the expected reports between the two dates
   * ## used by the advanced reporting module ##
   */
  def findExpectedReports(beginDate : DateTime, endDate : DateTime) : Box[Seq[RuleExpectedReports]]


  /**
   * Return current expectedreports (the one still pending) for this Rule
   */
  def findCurrentExpectedReports(rule : RuleId) : Box[Option[RuleExpectedReports]]


  /*
   * Retrieve the last expected reports for the nodes.
   */
  def getLastExpectedReports(nodeIds: Set[NodeId], filterByRules: Set[RuleId]): Box[Set[RuleExpectedReports]]

  /*
   * Retrieve the expected reports by config version of the nodes
   */
  def getExpectedReports(nodeConfigIds: Set[NodeConfigId], filterByRules: Set[RuleId]): Box[Set[RuleExpectedReports]]


}