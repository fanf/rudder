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
import org.joda.time._
import org.joda.time.format._
import com.normation.rudder.domain.Constants
import com.normation.cfclerk.domain.{Cf3PolicyDraftId}
import com.normation.utils.HashcodeCaching
import scala.collection.mutable.Buffer
import com.normation.rudder.domain.logger.ReportLogger
import com.normation.rudder.domain.policies.Directive
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.reports._
import com.normation.rudder.reports.ComplianceMode
import com.normation.utils.Control.sequence
import com.normation.rudder.reports.FullCompliance
import com.normation.rudder.reports.ChangesOnly
import com.normation.rudder.reports.execution.AgentRunId
import net.liftweb.common.Loggable

/**
 * An execution batch contains the node reports for a given Rule / Directive at a given date
 * An execution Batch is at a given time <- TODO : Is it relevant when we have several node ?
 */

object ExecutionBatch extends Loggable {
  final val matchCFEngineVars = """.*\$(\{.+\}|\(.+\)).*""".r
  final private val replaceCFEngineVars = """\$\{.+\}|\$\(.+\)"""

  /**
   * Takes a string, that should contains a CFEngine var ( $(xxx) or ${xxx} )
   * replace the $(xxx) (or ${xxx}) part by .*
   * and doubles all the \
   * Returns a string that is suitable for a being used as a regexp
   */
  final def replaceCFEngineVars(x : String) : String = {
    x.replaceAll(replaceCFEngineVars, ".*").replaceAll("""\\""", """\\\\""")
  }

  case class ContextForNoAnswer(
      agentExecutionInterval: Int
    , complianceMode        : ComplianceMode
  )


  /**
   * Method to check if an "date" is experied or no (compared to a reference
   * time, usually now), given:
   * - the compliance mode,
   * - the agent run frequency
   *
   * We ASSUME that the time to check and the reference one are comparable, i.e
   * that the machine they come from have a clock correctly configured & synchronized.
   * This is important for complianceMode = FullCompliance only.
   * See for example: http://www.rudder-project.org/redmine/issues/5272
   *
   * "Expiration" happen after a time of min(runInterval, 5 min)+runInterval
   * (i.e, we let the agent a full run interval to actually runs, and then again
   *  5 min to let bits find there way)
   *
   * The reference point is "now".
   * GenTime        +1 interval +5 min
   *    |--------------|---------|--------|
   *           |             |        |
   *         "now" 1       "now" 2   "now" 3
   *
   *  now1 and now2 imply that reports may be pending
   *  now3 implies that reports are in no answer
   */
  def isExpired(time: DateTime, complianceMode: ComplianceMode, agentExecutionInterval: Int, referenceTime: DateTime = DateTime.now) : Boolean = {
    complianceMode match {
      case ChangesOnly => false
      case FullCompliance => time.plusMinutes(agentExecutionInterval + Math.min(agentExecutionInterval,5)).isBefore(referenceTime)
    }
  }

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
      nodeId                 : NodeId
      // time reported by the run -- can be empty if there is actually no run
    , optAgentRunTime        : Option[DateTime]
      //config version reported by the run
    , optRunNodeConfigVersion: Option[NodeConfigVersion]
    , expectedReports        : Seq[RuleExpectedReports]
    , agentExecutionReports  : Seq[Reports]
    // this is the agent execution interval, in minutes
    , agentExecutionInterval : Int
    , complianceMode         : ComplianceMode
  ) : Seq[RuleNodeStatusReport] = {

    /*
     * we define two comptatible option(nodeconfig version) as:
     * if we know both, they must be equals, else they are compatible
     */
    def compatibleVersions(x:Option[NodeConfigVersion], y:Option[NodeConfigVersion]) = {
      (x,y) match {
        case (Some(v1), Some(v2)) => v1 == v2
        case _ => true
      }
    }


    /**
     * Question: what to do if no expected reports ?
     * Shouldn't we mark each received reports as "unexpected / error" ?
     * The historical interpretation is to not process that case.
     */
    if(expectedReports.isEmpty) return Seq()


    /**
     * NoAnswer is interpreted the same for all reports
     * of ONE node.
     */
    val noAnswerInterpretation = {
      complianceMode match {
        case FullCompliance =>
          //in that case, we should have gotten report after a "reasonable" amount of time
          //after the last rule update, i.e the most recent "begin date" among expected reports
          val lastExpectReportModification = expectedReports.map( _.beginDate).maxBy( _.getMillis)

          if(isExpired(lastExpectReportModification, complianceMode, agentExecutionInterval)) {
            NoAnswerReportType
          } else {
            PendingReportType
          }

        case ChangesOnly =>
          //in that mode, an answer is a SUCCESS that should be display
          //to the user as "No change"
          SuccessReportType
      }
    }

    (for {
      // start by getting expected directive (grouping by rules, then by directives)
      (ruleId, exr)             <- expectedReports.groupBy( _.ruleId)
      expectedReport            <- exr
      ( (nodeId, optConfigId)
        , directivesOnNode    ) <- expectedReport.directivesOnNodes.collect { x =>
                                     x.nodeConfigurationIds.find( _._1 == nodeId) match {
                                       case None => None
                                       case Some(nodeConfigId) => Some((nodeConfigId, x))
                                     }
                                   }.flatten
      directiveStatusReports    =  for {
                                    expectedDirective <- directivesOnNode.directiveExpectedReports
                                  } yield {
                                    //now, build component/component value/message status reports
                                    val componentsStatus = for {
                                                             expectedComponent <- expectedDirective.components
                                                           } yield {
                                                             //we are using a check on version two say that in fact,
                                                             //the report you get were not for the version needed

                                                             val componentFilteredReports = {
                                                               if(compatibleVersions(optRunNodeConfigVersion, optConfigId)) {
                                                                 agentExecutionReports.filter(x =>
                                                                   x.nodeId == nodeId &&
                                                                   x.serial == expectedReport.serial &&
                                                                   x.directiveId == expectedDirective.directiveId &&
                                                                   x.component == expectedComponent.componentName
                                                                 )
                                                               } else {
                                                                 Seq()
                                                               }
                                                             }

                                                             checkExpectedComponentWithReports(expectedComponent, componentFilteredReports, noAnswerInterpretation)
                                                           }

                                    DirectiveStatusReport(
                                        expectedDirective.directiveId
                                      , ComponentStatusReport.merge(componentsStatus)
                                    )
                                  }
    } yield {
      RuleNodeStatusReport(
          nodeId
        , ruleId
        , expectedReport.serial
        , optAgentRunTime
        , optConfigId
        , DirectiveStatusReport.merge(directiveStatusReports)
      )
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
      expectedComponent: ComponentExpectedReport
    , filteredReports  : Seq[Reports]
    , noAnswerType     : ReportType
  ) : ComponentStatusReport = {

    // First, filter out all the not interesting reports
    // (here, interesting means non log, info, etc)
    val purgedReports = filteredReports.filter(x => x.isInstanceOf[ResultReports])

    // build the list of unexpected ComponentValueStatusReport
    // they may be duplicate componentValue name, will need to merge it
    val unexpectedStatusReports = {
      val unexpectedReports = getUnexpectedReports(
          expectedComponent.componentsValues.toList
        , purgedReports
      )
      unexpectedReports.foreach { r =>
        ReportLogger.warn(s"Unexpected report for Directive '${r.directiveId.value}', Rule '${r.ruleId.value}' generated on '${r.executionTimestamp}' "+
            s"on node '${r.nodeId.value}', Component is '${r.component}', keyValue is '${r.keyValue}'. The associated message is : ${r.message}"
        )
      }
      for {
        unexpectedReport <- unexpectedReports
      } yield {
        val message = unexpectedReport.message.trim match {
          case "" => None
          case s => Some(s)
        }
        ComponentValueStatusReport(
           unexpectedReport.keyValue
         , None // <- is it really None that we set there ?
         , List(MessageStatusReport(UnknownReportType, message))
        )
      }

    }


    case class ValueKind(
        none  : Seq[(String, Option[String])] = Seq()
      , simple: Seq[(String, Option[String])] = Seq()
      , cfeVar: Seq[(String, Option[String])] = Seq()
    )

    //now, we group values by what they look like: None, cfengine variable, simple value
    val valueKind = (ValueKind()/:expectedComponent.groupedComponentValues) {
      case (kind,  n@("None", _)) => kind.copy(none = kind.none :+ n)
      case (kind,  v@(value, unexpanded)) =>
        value match {
          case matchCFEngineVars(_) => kind.copy(cfeVar = kind.cfeVar :+ v)
          case _ => kind.copy(simple = kind.simple :+ v)
        }
    }

    def getUnexpanded(seq: Seq[Option[String]]): Option[String] = {
      val unexpanded = seq.toSet
      if(unexpanded.size > 1) {
        logger.debug("Several same looking expected component values have different unexpanded value, which is not supported: " + unexpanded.mkString(","))
      }
      unexpanded.head
    }

    val noneReport = if(valueKind.none.isEmpty){
      Seq()
    } else {
      Seq(buildComponentValueStatus(
          "None"
        , purgedReports.filter(r => r.keyValue == "None")
        , unexpectedStatusReports.nonEmpty
        , valueKind.none.size
        , noAnswerType
        , getUnexpanded(valueKind.none.map( _._2))
      ))
    }

    val simpleValueReports = {
      for {
        (value, seq) <- valueKind.simple.groupBy( _._1 )
      } yield {
        buildComponentValueStatus(
            value
          , purgedReports.filter(r => r.keyValue == value)
          , unexpectedStatusReports.nonEmpty
          , seq.size
          , noAnswerType
          , getUnexpanded(seq.map( _._2))
        )
      }
    }

    val cfeVarReports = for {
      (pattern, seq) <- valueKind.cfeVar.groupBy( x => replaceCFEngineVars(x._1) )
    } yield {
      /*
       * Here, for a given pattern, we can have different source cfengine vars, and
       * a list of report that matches.
       * We have no way to know what source cfe var goes to which reports.
       * We can only check that the total number of expected reports for a given
       * pattern is equal to the number of report that matches that pattern.
       * If it's not the case, all is "Unexpected".
       * Else, randomly assign values to source pattern
       */

      val matchingReports = purgedReports.filter(r => r.keyValue.matches(pattern))

      if(matchingReports.size > seq.size) {
        seq.map { case (value, unexpanded) =>
          ComponentValueStatusReport(value, unexpanded, MessageStatusReport(ErrorReportType, None)::Nil)
        }
      } else {
        //seq is >= matchingReports
        (seq.zip(matchingReports).map { case ((value, unexpanded), r) =>
          ComponentValueStatusReport(value, unexpanded, MessageStatusReport(ReportType(r), r.message)::Nil)
        } ++
        seq.drop(matchingReports.size).map { case (value, unexpanded) =>
          ComponentValueStatusReport(value, unexpanded, MessageStatusReport(noAnswerType, None)::Nil)
        })
      }
    }


    ComponentStatusReport(
        expectedComponent.componentName
      , ComponentValueStatusReport.merge(unexpectedStatusReports ++ noneReport ++ simpleValueReports ++ cfeVarReports.flatten)
    )
  }

  /*
   * An utility method that fetches the proper status and messages
   * of a component value.
   */
  private[this] def buildComponentValueStatus(
      currentValue        : String
    , filteredReports     : Seq[Reports]
    , hasUnexpectedReports: Boolean
    , cardinality         : Int
    , noAnswerType        : ReportType
    , unexpandedValue     : Option[String]
  ) : ComponentValueStatusReport = {

    val messageStatusReports = {
       filteredReports.filter( x => x.isInstanceOf[ResultErrorReport]).size match {
          case i if i > 0 =>
            filteredReports.map(r => MessageStatusReport(ErrorReportType, r.message)).toList
          case _ => {
            filteredReports.size match {
              /* Nothing was received at all for that component so : No Answer or Pending */
              case 0 if hasUnexpectedReports =>  MessageStatusReport(noAnswerType, None) :: Nil
              /* Reports were received for that component, but not for that key, that's a missing report */
              case 0 =>  MessageStatusReport(UnknownReportType, None) :: Nil
              //check if cardinality is ok
              case x if x == cardinality =>
                filteredReports.map { r =>
                  MessageStatusReport(ReportType(r), r.message)
                }.toList
              case _ =>
                filteredReports.map(r => MessageStatusReport(UnknownReportType, r.message)).toList
            }
          }
        }
    }

    ComponentValueStatusReport(
        currentValue
      , unexpandedValue
      , messageStatusReports
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

}
