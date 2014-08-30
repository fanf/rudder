/*
*************************************************************************************
* Copyright 2014 Normation SAS
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

package com.normation.rudder.domain.reports

import org.joda.time.DateTime

import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.policies.RuleId

/**
 * That file contains all the kind of status reports:
 *
 * for a component, a directive, a node, etc.
 *
 */

//simple data structure to hold percentages of different compliance
//the ints are actual numbers, percents are computed with the pc_ variants
case class ComplianceLevel(
    pending      : Int = 0
  , success      : Int = 0
  , repaired     : Int = 0
  , error        : Int = 0
  , noAnswer     : Int = 0
  , notApplicable: Int = 0
) {
  val total = pending+success+repaired+error+noAnswer+notApplicable

  private[this] def pc(i:Int) = if(total == 0) 0 else i * 100 / total

  val pc_pending       = pc(pending)
  val pc_success       = pc(success)
  val pc_repaired      = pc(repaired)
  val pc_error         = pc(error)
  val pc_noAnswer      = pc(noAnswer)
  val pc_notApplicable = pc(notApplicable)

  def +(compliance: ComplianceLevel): ComplianceLevel = {
    ComplianceLevel(
        this.pending + compliance.pending
      , this.success + compliance.success
      , this.repaired + compliance.repaired
      , this.error + compliance.error
      , this.noAnswer + compliance.noAnswer
      , this.notApplicable + compliance.notApplicable
    )
  }

  def +(report: ReportType): ComplianceLevel = this+ComplianceLevel.compute(Seq(report))
  def +(reports: Iterable[ReportType]): ComplianceLevel = this+ComplianceLevel.compute(reports)
}

object ComplianceLevel {
 def compute(reports: Iterable[ReportType]): ComplianceLevel = {
    if(reports.isEmpty) { ComplianceLevel(notApplicable = 100)}
    else reports.foldLeft(ComplianceLevel()) { case (compliance, report) =>
      report match {
        case NotApplicableReportType => compliance.copy(notApplicable = compliance.notApplicable + 1)
        case SuccessReportType => compliance.copy(success = compliance.success + 1)
        case RepairedReportType => compliance.copy(repaired = compliance.repaired + 1)
        case ErrorReportType | UnknownReportType => compliance.copy(error = compliance.error + 1)
        case NoAnswerReportType => compliance.copy(noAnswer = compliance.noAnswer + 1)
        case PendingReportType => compliance.copy(pending = compliance.pending + 1)
      }
    }
  }

 def sum(compliances: Iterable[ComplianceLevel]): ComplianceLevel = {
   if(compliances.isEmpty) ComplianceLevel()
   else compliances.reduce( _ + _)
 }
}

sealed trait StatusReport {
  def reportType : ReportType
  def compliance : ComplianceLevel

  def getWorseType(reports:Iterable[StatusReport]) = {
    ReportType.getWorseType(reports.map(_.reportType))
  }
}

/**
 * For a component value, store the report status
 */
final case class ComponentValueStatusReport(
    componentValue 		   : String
  , unexpandedComponentValue: Option[String]
  , reportType              : ReportType
  , message                 : List[String]
) extends StatusReport {
  override val compliance = ComplianceLevel.compute(Seq(reportType))
}

/**
 * For a component, store the report status, as the worse status of the component
 * Or error if there is an unexpected component value
 */
final case class ComponentStatusReport(
    component          : String
  , componentValues    : Set[ComponentValueStatusReport]
  , message            : List[String]
  , unexpectedCptValues: Set[ComponentValueStatusReport]
) extends StatusReport {

  val reportType = getWorseType(componentValues ++ unexpectedCptValues)
  override val compliance = ComplianceLevel.sum(componentValues.map(_.compliance) ++ unexpectedCptValues.map(_.compliance))
}


final case class DirectiveStatusReport(
    directiveId         : DirectiveId
  , components	         : Set[ComponentStatusReport]
  , unexpectedComponents: Set[ComponentStatusReport] // for future use, not used yet
) extends StatusReport {

  val reportType = getWorseType(components ++ unexpectedComponents)
  override val compliance = ComplianceLevel.sum(components.map(_.compliance) ++ unexpectedComponents.map(_.compliance))
}

final case class NodeStatusReport(
    nodeId              : NodeId
  , optAgentRunTime     : Option[DateTime]
  , optNodeConfigVersion: Option[NodeConfigVersion]
  , ruleId              : RuleId
  , directives	         : Set[DirectiveStatusReport]
  , unexpectedDirectives: Set[DirectiveStatusReport] // for future use, not used yet
) extends StatusReport {

  val reportType = getWorseType(directives ++ unexpectedDirectives)
  override val compliance = ComplianceLevel.sum(directives.map(_.compliance) ++ unexpectedDirectives.map(_.compliance))
}

final case class NodeReport (
    node      : NodeId
  , reportType: ReportType
  , message   : List[String]
)


final case class MessageReport(
    report         : NodeReport
  , component      : String
  , value          : String
  , unexpandedValue: Option[String]
)

sealed trait RuleStatusReport {

  def nodesReport : Set[NodeReport]

  def processMessageReport(filter: NodeReport => Boolean):Seq[MessageReport]

  lazy val compliance : ComplianceLevel = ComplianceLevel.compute(nodesReport.map(_.reportType))
}

final case class ComponentValueRuleStatusReport(
    directiveId             : DirectiveId
  , component               : String
  , componentValue          : String
  , unexpandedComponentValue: Option[String]
  , nodesReport             : Set[NodeReport]
) extends RuleStatusReport {


  // Key of the component, get the unexpanded value if it exists or else the component value
  val key = unexpandedComponentValue.getOrElse(componentValue)

  def processMessageReport(filter: NodeReport => Boolean):Seq[MessageReport] ={
    nodesReport.toSeq.filter(filter).map(MessageReport(_,component,componentValue, unexpandedComponentValue))
  }
}

final case class ComponentRuleStatusReport (
    directiveId    : DirectiveId
  , component      : String
  , componentValues: Set[ComponentValueRuleStatusReport]
) extends RuleStatusReport {

  override val nodesReport = componentValues.flatMap(_.nodesReport)

//  // since we have "exploded" ComponentValue, we need to regroup them
//  override def computeCompliance = {
//   if (componentValues.size>0){
//     // we need to group the compliances per unexpandedComponentValue
//     val aggregatedComponents = componentValues.groupBy { entry => entry.unexpandedComponentValue.getOrElse(entry.componentValue)}.map { case (key, entries) =>
//       ComponentValueRuleStatusReport(
//             entries.head.directiveId // can't fail because we are in a groupBy
//           , entries.head.component  // can't fail because we are in a groupBy
//           , key
//           , None
//           , entries.flatMap(_.nodesReport)
//          )
//     }
//     Some((aggregatedComponents.map(_.computeCompliance.getOrElse(0))
//         :\ 100)((res:Int,value:Int) => if(value>res)res else value))
//   }
//    else
//      None
//  }

  def processMessageReport(filter: NodeReport => Boolean):Seq[MessageReport] = {
    componentValues.toSeq.flatMap( value => value.processMessageReport(filter))
  }
}

final case class DirectiveRuleStatusReport(
    directiveId: DirectiveId
  , components : Set[ComponentRuleStatusReport]
) extends RuleStatusReport {

  override val nodesReport = components.flatMap(_.nodesReport)

  def processMessageReport(filter: NodeReport => Boolean): Seq[MessageReport] = {
    components.toSeq.flatMap( component => component.processMessageReport(filter))
  }
}


