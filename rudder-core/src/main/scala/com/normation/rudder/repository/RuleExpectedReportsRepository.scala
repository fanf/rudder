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

import com.normation.rudder.domain.policies.DirectiveId
import net.liftweb.common.Box
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.reports._
import org.joda.time._
import com.normation.rudder.reports.execution.AgentRunId


trait RuleExpectedReportsRepository {

  /**
   * Find expected reports corresponding to a agent run
   */
  def findExpectedReportsByAgentRun(runId: AgentRunId): Box[Seq[RuleExpectedReports]]

  /**
   * Return all the expected reports between the two dates
   * //used by the advanced reporting module
   */
  def findExpectedReports(beginDate : DateTime, endDate : DateTime) : Box[Seq[RuleExpectedReports]]


  /**
   * Return current expectedreports (the one still pending) for this Rule
   * @param rule
   * @return
   */
  def findCurrentExpectedReports(rule : RuleId) : Box[Option[RuleExpectedReports]]

  /**
   * Return the ruleId currently opened, and their serial and list of nodes
   * It is only used to know which conf expected report we should close
   */
  def findAllCurrentExpectedReportsWithNodesAndSerial(): Map[RuleId, (Int, Set[NodeConfigurationId])]


  /**
   * Return currents expectedreports (the one still pending) for this server
   * @param nodeId
   * @return
   */
  def findCurrentExpectedReportsByNode(nodeId : NodeId) : Box[Seq[RuleExpectedReports]]


 /**
   * Simply set the endDate for the expected report for this conf rule
   * @param ruleId
   */
  def closeExpectedReport(ruleId : RuleId) : Box[Unit]

  /**
   * Save an expected reports.
   * I'm not really happy with this API
   * @param ruleId : the id of the rule (the main id)
   * @param directiveId : the id of the directive (secondary id, used to check for the changes)
   * @param nodes : the nodes that are expected to be the target of this rule
   * @param cardinality : the cardinality of the expected reports
   * @return
   */
  def saveExpectedReports(
      ruleId                   : RuleId
    , serial                   : Int
    , directiveExpectedReports : Seq[DirectiveExpectedReports]
    , nodeConfigurationVersions: Seq[NodeConfigurationId]
  ) : Box[RuleExpectedReports]


}