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
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.policies.DirectiveId
import scala.collection._
import org.joda.time._
import org.joda.time.format._
import com.normation.rudder.domain.Constants
import com.normation.cfclerk.domain.{Cf3PolicyDraftId}
import com.normation.utils.HashcodeCaching
import scala.collection.mutable.Buffer
import ExecutionBatch._
import com.normation.rudder.domain.logger.ReportLogger
import com.normation.rudder.domain.policies.Directive
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.reports._
import com.normation.rudder.reports.ComplianceMode
import com.normation.utils.Control.sequence
import com.normation.rudder.reports.FullCompliance
import com.normation.rudder.reports.ErrorOnly

/**
 * An execution batch contains the node reports for a given Rule / Directive at a given date
 * An execution Batch is at a given time <- TODO : Is it relevant when we have several node ?
 */

object ExecutionBatch {
  final val matchCFEngineVars = """.*\$(\{.+\}|\(.+\)).*""".r
  final private val replaceCFEngineVars = """\$\{.+\}|\$\(.+\)"""

  /**
   * Takes a string, that should contains a CFEngine var ( $(xxx) or ${xxx} )
   * replace the $(xxx) (or ${xxx}) part by .*
   * and doubles all the \
   * Returns a string that is suitable for a beoing used as a regexp
   */
  final def replaceCFEngineVars(x : String) : String = {
    x.replaceAll(replaceCFEngineVars, ".*").replaceAll("""\\""", """\\\\""")
  }

  case class ContextForNoAnswer(
      agentExecutionInterval: Int
    , complianceMode        : ComplianceMode
  )

  /**
   * This is the main entry point to get the detailed reporting
   * It returns a Sequence of NodeStatusReport which gives, for
   * each node, the status and all the directives associated.
   *
   * The contract is to give to that function a list of expected
   * report for an unique given node
   *
   */
  def getNodeStatusReports(
      nodeId                : NodeId
    , optAgentRunTime       : Option[DateTime]
    , optNodeConfigVersion  : Option[String]
    , expectedReports       : Seq[RuleExpectedReports]
    , agentExecutionReports : Seq[Reports]
    // this is the agent execution interval, in minutes
    , agentExecutionInterval: Int
    , complianceMode        : ComplianceMode
  ) : Seq[NodeStatusReport] = {

    /**
     * NoAnswer is interpreted the same for all reports
     * of ONE node.
     */
    val noAnswerInterpretation = {
      complianceMode match {
        case FullCompliance =>
          //in that case, we should have gotten report after a "reasonable" amount of time
          //after the last rule update, i.e the most recent "begin date" among execpted reports
          val lastExpectReportModification = expectedReports.map( _.beginDate).maxBy( _.getMillis)

          //the "reasonable" time is 2 times the agent interval, in minutes.
          if (lastExpectReportModification.plus(agentExecutionInterval*2*1000*60).isAfter(DateTime.now())) {
            PendingReportType
          } else {
            NoAnswerReportType
          }
        case ErrorOnly =>
          //in that mode, an answer is a SUCCESS that should be display
          //to the user as "No change"
          SuccessReportType
      }
    }

    (for {
      (ruleId, exr) <- expectedReports.groupBy( _.ruleId)
      expectedReport            <- exr
      directivesOnNode          <- expectedReport.directivesOnNodes.filter(x => x.nodeConfigurationIds.exists( _.nodeId == nodeId) )
      directiveStatusReports    =  for {
                                    expectedDirective <- directivesOnNode.directiveExpectedReports
                                  } yield {
                                    // look for each component
                                    val componentsStatus = for {
                                                             expectedComponent <- expectedDirective.components
                                                           } yield {
                                                             val componentFilteredReports = agentExecutionReports.filter(x =>
                                                               x.nodeId == nodeId &&
                                                               x.directiveId == expectedDirective.directiveId &&
                                                               x.component == expectedComponent.componentName
                                                             )

                                                             checkExpectedComponentWithReports(expectedComponent, componentFilteredReports, noAnswerInterpretation)
                                                           }

                                    DirectiveStatusReport(expectedDirective.directiveId, componentsStatus, Seq())
                                  }
    } yield {
      NodeStatusReport(nodeId, optAgentRunTime, optNodeConfigVersion, ruleId, directiveStatusReports, Seq())
    }).toSeq
  }


  /**
   * Allows to calculate the status of component for a node.
   * We don't deal with interpretation at that level,
   * in particular regarding the not received / etc status, we
   * simply put "no answer" for each case where we don't
   * have an actual report corresponding to the expected one
   *
   * The visibility is for allowing tests
   */
  private[reports] def checkExpectedComponentWithReports(
      expectedComponent: ReportComponent
    , filteredReports  : Seq[Reports]
    , noAnswerType     : ReportType
  ) : ComponentStatusReport = {

    // First, filter out all the not interesting reports
    val purgedReports = filteredReports.filter(x => x.isInstanceOf[ResultReports])

    val componentValueStatusReports = for {
      (componentValue, unexpandedComponentValues) <- expectedComponent.groupedComponentValues
    } yield {
      buildComponentValueStatus(
          componentValue
        , purgedReports
        , expectedComponent.componentsValues
        , noAnswerType
        , unexpandedComponentValues
      )
    }

    // must fetch extra entries
    val unexpectedReports = getUnexpectedReports(
        expectedComponent.componentsValues.toList
      , purgedReports
    )
    unexpectedReports.foreach { r =>
      ReportLogger.warn(s"Unexpected report for Directive '${r.directiveId.value}', Rule '${r.ruleId.value}' generated on '${r.executionTimestamp}' "+
          s"on node '${r.nodeId.value}', Component is '${r.component}', keyValue is '${r.keyValue}'. The associated message is : ${r.message}"
      )
    }
    val unexpectedCVSRs = for {
      unexpectedReport <- unexpectedReports
    } yield {
        ComponentValueStatusReport(
           unexpectedReport.keyValue
         , None // <- is it really None that we set there ?
         , UnknownReportType
         , List(unexpectedReport.message)
        )
    }

    ComponentStatusReport(
        expectedComponent.componentName
      , componentValueStatusReports
      , if(unexpectedCVSRs.size < 1) {
          unexpectedReports.map(_.message).toList
        } else {
          purgedReports.map(_.message).toList
        }
      , unexpectedCVSRs
    )
  }

  /*
   * An utility method that fetches the proper status and messages
   * of a component value.
   */
  private[this] def buildComponentValueStatus(
      currentValue   : String
    , filteredReports: Seq[Reports]
    , expectedValues : Seq[String]
    , noAnswerType   : ReportType
    , unexpandedValue: Option[String]
  ) : ComponentValueStatusReport = {
    val unexepectedReports = filteredReports.filterNot(value => expectedValues.contains(value.keyValue))

    /* Refactored this function because it was the same behavior for each case*/
    def getComponentStatus(reports: Seq[Reports], valueMatcher: String => Boolean) : (ReportType, List[String]) = {
       reports.filter( x => x.isInstanceOf[ResultErrorReport]).size match {
          case i if i > 0 => (ErrorReportType, reports.map(_.message).toList)
          case _ => {
            reports.size match {
              /* Nothing was received at all for that component so : No Answer or Pending */
              case 0 if unexepectedReports.size == 0 =>  (noAnswerType, Nil)
              /* Reports were received for that component, but not for that key, that's a missing report */
              case 0 =>  (UnknownReportType, Nil)
              case x if x == expectedValues.filter( x => valueMatcher(x)).size =>
                (ReportType.getWorseReport(reports), reports.map(_.message).toList)
              case _ => (UnknownReportType,filteredReports.map(_.message).toList)
            }
          }
        }
    }

    val (status,message) = currentValue match {
      case "None" =>
        val reports = filteredReports.filter( x => x.keyValue == currentValue )
        getComponentStatus(reports, _ == currentValue)

      case matchCFEngineVars(_) =>
        // convert the entry to regexp, and match what can be matched
        val matchableExpected = replaceCFEngineVars(currentValue)
        val matchedReports = filteredReports.filter( x => x.keyValue.matches(matchableExpected))
        getComponentStatus(matchedReports, _.matches(matchableExpected))

      case _: String =>
        // for a given component, if the value is not "None", then we are
        // checking that what the value is is equals to what we wish.
        // We can have more reports that what we expected, because of
        // name collision, but it would be resolved by checking the total
        // number of received reports for that component.
        val keyReports =  filteredReports.filter( x => x.keyValue == currentValue)
        getComponentStatus(keyReports, _ == currentValue)
    }
    ComponentValueStatusReport(
        currentValue
      , unexpandedValue
      , status
      , message
    )
  }

  /**
   * Retrieve all the reports that should not be there (due to
   * keyValue not present)
   */
  private[this] def getUnexpectedReports(
      keyValues: List[String]
    , reports  : Seq[Reports]
  ) : Seq[Reports] = {

    val isExpected = (head:String, s:String) => head match {
      case matchCFEngineVars(_) =>
        val matchableExpected = replaceCFEngineVars(head)
        s.matches(matchableExpected)
      case x => x == s
    }

    keyValues match {
      case Nil          => reports
      case head :: tail =>
        getUnexpectedReports(tail, reports.filterNot(r => isExpected(head, r.keyValue)))
    }
  }

  def getNodeStatusReportsByRule(
      ruleExpectedReports   : RuleExpectedReports
    , reportsParam          : Seq[Reports]
    // this is the agent execution interval, in minutes
    , agentExecutionInterval: Int
    , complianceMode        : ComplianceMode
  ): Seq[NodeStatusReport] = {
    (for {
      directiveOnNode                      <- ruleExpectedReports.directivesOnNodes
      NodeConfigurationId(nodeId, version) <- directiveOnNode.nodeConfigurationIds
    } yield {
      getNodeStatusReports(nodeId, None, Some(version), Seq(ruleExpectedReports), reportsParam, agentExecutionInterval, complianceMode)
    }).flatten
  }


  /**
   * Get the actual status of a Rule, it returns a list of every directive contained by that Rule
   */
  def getRuleStatus(
      ruleExpectedReports   : RuleExpectedReports
    , reportsParam          : Seq[Reports]
    // this is the agent execution interval, in minutes
    , agentExecutionInterval: Int
    , complianceMode        : ComplianceMode
  ) : Seq[DirectiveRuleStatusReport]={

    //start by getting NodeStatusReports for all nodes involved
    val nodeStatusReports = getNodeStatusReportsByRule(ruleExpectedReports, reportsParam, agentExecutionInterval, complianceMode)

    ruleExpectedReports.directivesOnNodes.flatMap { d  =>
      d.directiveExpectedReports
    }.groupBy( x => x.directiveId).map { case (directiveId, directiveExpectedReports) =>
        // we fetch the component reports for this directive
        val componentReports = nodeStatusReports.flatMap { nodeStatus =>
          // we filter by directiveId
          val directivesStatus = nodeStatus.directives.filter(_.directiveId == directiveId)
          getComponentRuleStatus(nodeStatus.nodeId, directiveId, directiveExpectedReports.flatMap(x=> x.components), directivesStatus)
        }.groupBy(_.component).map { case (componentName, componentReport) =>
          val componentValueReports = componentReport.flatMap(_.componentValues).
            groupBy(x=> (x.unexpandedComponentValue)).
            flatMap { case (unexpandedComponentValue, componentValueReport) =>
              // if unexpandedComponentValue exists, then we may have different values, hence the worst type
              // has to be computed there; else it has to be computed on the values level
              unexpandedComponentValue match {
                case Some(unexpended) =>
                  componentValueReport.groupBy(x => x.componentValue).map { case (componentValue, reports) =>
                    ComponentValueRuleStatusReport(
                        directiveId
                      , componentName
                      , componentValue
                      , unexpandedComponentValue
                      , reports.flatMap(_.nodesReport)
                    )
                  }
                case None =>
                  componentValueReport.groupBy(x => x.componentValue).map { case (componentValue, reports) =>
                    ComponentValueRuleStatusReport(
                        directiveId
                      , componentName
                      , componentValue
                      , unexpandedComponentValue
                      , reports.flatMap(_.nodesReport)
                    )
                  }
              }
           }.toSeq
           ComponentRuleStatusReport(directiveId,componentName,componentValueReports)
        }.toSeq
        DirectiveRuleStatusReport(directiveId,componentReports)
      }.toSeq
  }

  /**
   * Get the status of every component of the directive passed as a parameter
   * Parameters:
   * directiveId : Components we are looking for are contained in that directive
   * components  : Expected component report format
   * directive   : Latest directive reports
   */
  private[this] def getComponentRuleStatus(nodeId: NodeId, directiveid:DirectiveId, components:Seq[ReportComponent], directive:Seq[DirectiveStatusReport]) : Seq[ComponentRuleStatusReport]={
     components.map{ component =>
       val id = component.componentName
       val componentvalues = directive.flatMap{ nodestatus =>
         val components = nodestatus.components.filter(_.component==id)
         getComponentValuesRuleStatus(nodeId, directiveid, id, component.groupedComponentValues,components) ++
         getUnexpectedComponentValuesRuleStatus(nodeId, directiveid, id, components.flatMap(_.unexpectedCptValues))
       }
       ComponentRuleStatusReport(directiveid,id,componentvalues)
     }
 }
  /**
   * Get the status of expected values of the component passed as a parameter
   * Parameters:
   * directiveId : Values we are looking for are contained in that directive
   * component   : Values we are looking for are contained in that component
   * values      : Expected values format
   * components  : Latest components report
   */
 private[this] def getComponentValuesRuleStatus(nodeId: NodeId, directiveid:DirectiveId, component:String, values:Seq[(String, Option[String])], components:Seq[ComponentStatusReport]) : Seq[ComponentValueRuleStatusReport]={
     values.map{
       case (value, unexpanded) =>
         val componentValues = components.flatMap(_.componentValues.filter(_.componentValue==value))
         val nodes = componentValues.map(value => NodeReport(nodeId, value.reportType, value.message))
         ComponentValueRuleStatusReport(
             directiveid
           , component
           , value
           , unexpanded
           , nodes)
     }
 }

   /**
   * Get the status of expected values of the component passed as a parameter
   * Parameters:
   * directiveId : Unexpected Values have been received for that directive
   * component   : Unexpected Values have been received for that component
   * values      : Unexpected values received for that component
   */
 private[this] def getUnexpectedComponentValuesRuleStatus(nodeId: NodeId, directiveid:DirectiveId, component:String, values:Seq[ComponentValueStatusReport]) : Seq[ComponentValueRuleStatusReport]={
     values.map{
       value =>
         val nodes = Seq(NodeReport(nodeId, value.reportType, value.message))
         ComponentValueRuleStatusReport(
             directiveid
           , component
           , value.componentValue
           , value.unexpandedComponentValue
           , nodes
         )
     }
 }

}
