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

import com.normation.cfclerk.domain.TrackerVariableSpec
import com.normation.inventory.domain.NodeId
import net.liftweb.common._
import scala.collection.mutable.{Set => MutSet}
import scala.collection.mutable.{Map => MutMap}
import org.joda.time._
import org.slf4j.{Logger,LoggerFactory}
import com.normation.cfclerk.domain.{
  TrackerVariable,Variable,
  Cf3PolicyDraft,Cf3PolicyDraftId
}
import com.normation.rudder.domain._
import com.normation.rudder.domain.reports.RuleExpectedReports
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.reports._
import com.normation.rudder.services.reports._
import com.normation.rudder.repository._
import com.normation.rudder.domain.policies.RuleVal
import com.normation.rudder.domain.reports.DirectiveExpectedReports
import com.normation.cfclerk.xmlparsers.CfclerkXmlConstants._
import com.normation.rudder.domain.policies.ExpandedRuleVal
import com.normation.utils.Control._
import scala.collection.mutable.Buffer
import com.normation.cfclerk.domain.PredefinedValuesVariableSpec
import com.normation.cfclerk.domain.PredefinedValuesVariableSpec
import com.normation.rudder.domain.policies.ExpandedDirectiveVal
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.reports.execution.RoReportsExecutionRepository
import com.normation.rudder.reports.execution.AgentRunId
import com.normation.rudder.reports.execution.ReportExecution
import com.normation.rudder.reports.execution.ReportExecution
import com.normation.rudder.reports.ComplianceMode
import com.normation.rudder.domain.policies.ExpandedRuleVal
import com.normation.rudder.reports.execution.ReportExecution
import scala.tools.nsc.transform.Flatten
import com.normation.rudder.reports.execution.ReportExecution
import com.normation.rudder.reports.FullCompliance
import com.normation.rudder.reports.ChangesOnly
import com.normation.rudder.reports.execution.ReportExecution
import com.normation.rudder.reports.execution.AgentRunId

class ReportingServiceImpl(
    confExpectedRepo   : FindExpectedReportRepository
  , reportsRepository  : ReportsRepository
  , agentRunRepository : RoReportsExecutionRepository
  , getAgentRunInterval: () => Int
  , getComplianceMode  : () => Box[ComplianceMode]
) extends ReportingService with Loggable {



  override def findDirectiveRuleStatusReportsByRule(ruleId: RuleId): Box[Seq[RuleNodeStatusReport]] = {

    val agentRunInterval = getAgentRunInterval()

    for {
      compliance  <- getComplianceMode()
      optExpected <- confExpectedRepo.findCurrentExpectedReports(ruleId)
      res         <- optExpected match {
                       case None => Full(Seq())
                       case Some(expected) =>
                         val nodeIds = expected.directivesOnNodes.flatMap( _.nodeConfigurationIds.keySet ).toSet
                         for {
                           runs    <- agentRunRepository.getNodesLastRun(nodeIds)
                           nodeStatusReports <- buildNodeStatusReports(runs, Seq(expected), Set(ruleId), compliance, agentRunInterval)
                         } yield {
                           nodeStatusReports
                         }
                     }
    } yield {
      res
    }
  }




  private[this] def getExecTime(runIds:Set[AgentRunId], nodeId: NodeId): Option[DateTime] = {
    runIds.find( _.nodeId == nodeId).map( _.date)
  }

  override def findNodeStatusReports(nodeIds: Set[NodeId], ruleIds: Set[RuleId]) : Box[Seq[RuleNodeStatusReport]] = {
    //TODO: interval by node for the interpretation
    val agentRunInterval = getAgentRunInterval()

    for {
      compliance           <- getComplianceMode()
      runs                 <- agentRunRepository.getNodesLastRun(nodeIds)

      /*
       * We want to find what nodes need to get their
       * expected reports based on the last available or by version,
       * and what runs are ok given that
       */
      nodesForOpenExpected = compliance match {
                               //in full compliance, only the open expected reports are meaningful
                               //even if we could get them by version, we prefer to do only one request
                               //on the underlying Db
                               case FullCompliance => runs.keySet
                               case ChangesOnly => (runs.collect {
                                 case(nodeId, None) => nodeId
                                 case(nodeId, Some(x) ) if(x.nodeConfigVersion.isEmpty) => nodeId
                               }).toSet
                             }
      //get nodeConfigId for nodes not in the nodesForOpenExpected and with a config version
      nodesByConfigId      = runs.collect { case (nodeId, Some(ReportExecution(_,Some(v),_))) if(!nodesForOpenExpected.contains(nodeId)) =>
                               NodeConfigId(nodeId, v)
                             }.toSet
      lastExpectedReports  <- confExpectedRepo.getLastExpectedReports(nodesForOpenExpected, ruleIds)

      byVersionExpectedReports <- confExpectedRepo.getExpectedReports(nodesByConfigId, ruleIds)

      allExpected =  (byVersionExpectedReports ++ lastExpectedReports).toSeq

      nodeStatusReports <- buildNodeStatusReports(runs, allExpected, ruleIds, compliance, agentRunInterval)
    } yield {
      nodeStatusReports
    }
  }

  /*
   * Given a set of agen runs and expected reports, retrieve the corresponding
   * execution reports and then nodestatusreports, being smart about what to
   * query for
   */
  private[this] def buildNodeStatusReports(
      runs              : Map[NodeId,Option[ReportExecution]]
    , allExpectedReports: Seq[RuleExpectedReports]
    , ruleIds           : Set[RuleId]
    , compliance        : ComplianceMode
    , agentRunInterval  : Int
  ): Box[Seq[RuleNodeStatusReport]] = {

    val now = DateTime.now
    val runIds = (runs.collect { case(_, Some(ReportExecution(runId, _, _)))
                   if(!ExecutionBatch.isExpired(runId.date, compliance, agentRunInterval, now) ) => runId
                 }).toSet

    for {
      /*
       * now get reports for agent rules.
       * We don't want to reach for node reports that are out of date, i.e in full compliance mode,
       * reports older that agent's run interval + 5 minutes compare to now
       */
      reports              <- reportsRepository.getExecutionReports(runIds, ruleIds)
    } yield {

      val nodeVersions =  runs.flatMap( _._2.flatMap( x => x.nodeConfigVersion.map(v => ((x.runId.nodeId, v)) ) )).toMap

      //we want to have nodeStatus for all asked node, not only the ones with reports
      runs.keySet.toSeq.flatMap { nodeId =>
        ExecutionBatch.getNodeStatusReports(
              nodeId
            , getExecTime(runIds, nodeId)
            , nodeVersions.get(nodeId)
            , allExpectedReports
            , reports.getOrElse(nodeId, Seq())
            , agentRunInterval
            , compliance
        )
      }
    }
  }


}

