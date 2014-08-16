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

package com.normation.rudder.services.reports

import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.ExpandedRuleVal
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.reports.DirectiveRuleStatusReport
import com.normation.rudder.domain.reports.NodeConfigurationId
import com.normation.rudder.domain.reports.NodeStatusReport
import com.normation.rudder.domain.reports.RuleExpectedReports

import net.liftweb.common.Box

/**
 * The reporting service. It is used to
 * - Save the new reports expectation date
 * - retrieve the expected reports between a time interval
 *
 * Caution, the retrieve part is only on the operation
 *
 */
trait ReportingService {

  /**
   * Update the list of expected reports when we do a deployment
   *
   * For each RuleVal, we check if it was present or modified
   * If it was present and not changed, nothing is done for it
   * If it changed, then the previous version is closed, and the new one is opened
   */
  def updateExpectedReports(
      expandedRuleVals  : Seq[ExpandedRuleVal]
    , deleteRules       : Seq[RuleId]
    , updatedNodeConfigs: Map[NodeId, String]
  ) : Box[Seq[RuleExpectedReports]]


  def findDirectiveRuleStatusReportsByRule(ruleId: RuleId): Box[Seq[DirectiveRuleStatusReport]]

  def findNodeStatusReportsByRules(rulesIds : Set[RuleId]) : Map[RuleId, Box[Seq[NodeStatusReport]]]

  def findNodeStatusReportsByNode(nodeId: NodeId) : Box[Seq[NodeStatusReport]]


}
