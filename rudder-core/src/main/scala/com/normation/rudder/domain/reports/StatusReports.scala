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

import scala.collection.Seq

import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.policies.RuleId

/**
 * That file contains all the kind of status reports:
 *
 * for a component, a directive, a node, etc.
 *
 */

sealed trait StatusReport {
  def reportType : ReportType
}

/**
 * For a component value, store the report status
 */
final case class ComponentValueStatusReport(
    componentValue 		   : String
  , unexpandedComponentValue: Option[String]
  , reportType              : ReportType
  , message                 : List[String]
  , nodeId	             : NodeId
) extends StatusReport

/**
 * For a component, store the report status, as the worse status of the component
 * Or error if there is an unexpected component value
 */
final case class ComponentStatusReport(
    component          : String
  , componentValues    : Seq[ComponentValueStatusReport]
  , message            : List[String]
  , unexpectedCptValues: Seq[ComponentValueStatusReport]
) extends StatusReport {

  val reportType = {
    val reports = (componentValues ++ unexpectedCptValues).map(_.reportType)
    ReportType.getWorseType(reports)
  }
}


final case class DirectiveStatusReport(
    directiveId         : DirectiveId
  , components	         : Seq[ComponentStatusReport]
  , unexpectedComponents: Seq[ComponentStatusReport] // for future use, not used yet
) extends StatusReport {

  val reportType = {
    val reports = (components ++ unexpectedComponents).map(_.reportType)
    ReportType.getWorseType(reports)
  }
}

final case class NodeStatusReport(
    nodeId              : NodeId
  , ruleId              : RuleId
  , directives	         : Seq[DirectiveStatusReport]
  , unexpectedDirectives: Seq[DirectiveStatusReport] // for future use, not used yet
) extends StatusReport {

  val reportType = {
    val reports = (directives ++ unexpectedDirectives).map(_.reportType)
    ReportType.getWorseType(reports)
  }
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

  def nodesReport : Seq[NodeReport]

  lazy val reportType = {
    val reports = nodesReport.map(_.reportType)
    ReportType.getWorseType(reports)
  }

  def processMessageReport(filter: NodeReport => Boolean):Seq[MessageReport]

  def computeCompliance : Option[Int] = {
    if (nodesReport.size>0){
      val reportsSize = nodesReport.size.toDouble
      Some((nodesReport.map(report => report.reportType match {
        case SuccessReportType => 1
        case NotApplicableReportType    => 1
        case _                 => 0
    }):\ 0)((res:Int,value:Int) => res+value) * 100 / reportsSize).map{ res =>
      BigDecimal(res).setScale(0,BigDecimal.RoundingMode.HALF_UP).toInt
      }
    }
    else {
      None
    }
  }
}

final case class ComponentValueRuleStatusReport(
    directiveId             : DirectiveId
  , component               : String
  , componentValue          : String
  , unexpandedComponentValue: Option[String]
  , nodesReport             : Seq[NodeReport]
) extends RuleStatusReport {


  // Key of the component, get the unexpanded value if it exists or else the component value
  val key = unexpandedComponentValue.getOrElse(componentValue)

  def processMessageReport(filter: NodeReport => Boolean):Seq[MessageReport] ={
    nodesReport.filter(filter).map(MessageReport(_,component,componentValue, unexpandedComponentValue))
  }
}

final case class ComponentRuleStatusReport (
    directiveId    : DirectiveId
  , component      : String
  , componentValues: Seq[ComponentValueRuleStatusReport]
) extends RuleStatusReport {

  override val nodesReport = componentValues.flatMap(_.nodesReport)

  // since we have "exploded" ComponentValue, we need to regroup them
  override def computeCompliance = {
   if (componentValues.size>0){
     // we need to group the compliances per unexpandedComponentValue
     val aggregatedComponents = componentValues.groupBy { entry => entry.unexpandedComponentValue.getOrElse(entry.componentValue)}.map { case (key, entries) =>
       ComponentValueRuleStatusReport(
             entries.head.directiveId // can't fail because we are in a groupBy
           , entries.head.component  // can't fail because we are in a groupBy
           , key
           , None
           , entries.flatMap(_.nodesReport)
          )
     }
     Some((aggregatedComponents.map(_.computeCompliance.getOrElse(0))
         :\ 100)((res:Int,value:Int) => if(value>res)res else value))
   }
    else
      None
  }

  def processMessageReport(filter: NodeReport => Boolean):Seq[MessageReport] = {
    componentValues.flatMap( value => value.processMessageReport(filter))
  }
}

final case class DirectiveRuleStatusReport(
    directiveId: DirectiveId
  , components : Seq[ComponentRuleStatusReport]
) extends RuleStatusReport {

  override val nodesReport = components.flatMap(_.nodesReport)

  override def computeCompliance =
   if (components.size>0){
     Some((components.map(_.computeCompliance.getOrElse(0))
         :\ 100)((res:Int,value:Int) => if(value>res)res else value))
   }
    else
      None

  def processMessageReport(filter: NodeReport => Boolean): Seq[MessageReport] = {
    components.flatMap( component => component.processMessageReport(filter))
  }
}


