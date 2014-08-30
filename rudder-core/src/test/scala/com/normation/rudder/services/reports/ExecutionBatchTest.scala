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


import org.junit.runner._
import org.specs2.mutable._
import org.specs2.runner._
import scala.collection._
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.policies.DirectiveId
import org.joda.time.DateTime
import com.normation.rudder.domain.reports.DirectiveExpectedReports
import com.normation.rudder.domain.reports._
import com.normation.rudder.reports.ComplianceMode
import com.normation.rudder.reports.FullCompliance

@RunWith(classOf[JUnitRunner])
class ExecutionBatchTest extends Specification {
  private implicit def str2directiveId(s:String) = DirectiveId(s)
  private implicit def str2ruleId(s:String) = RuleId(s)
  private implicit def str2nodeId(s:String) = NodeId(s)
  private implicit def str2nodeConfigIds(ss:Seq[String]) = ss.map(s =>  (NodeId(s), Some(NodeConfigVersion("version_" + s)))).toMap

  def getNodeStatusReportsByRule(
      ruleExpectedReports   : RuleExpectedReports
    , reportsParam          : Seq[Reports]
    // this is the agent execution interval, in minutes
    , agentExecutionInterval: Int
    , complianceMode        : ComplianceMode
  ): Seq[NodeStatusReport] = {
    (for {
      directiveOnNode   <- ruleExpectedReports.directivesOnNodes
      (nodeId, version) <- directiveOnNode.nodeConfigurationIds
    } yield {
      ExecutionBatch.getNodeStatusReports(nodeId, None, version, Seq(ruleExpectedReports), reportsParam, agentExecutionInterval, complianceMode)
    }).flatten
  }

  val getNodeStatusByRule = (getNodeStatusReportsByRule _).tupled
  val getRuleStatus = ExecutionBatch.getRuleStatus _


  // Test the component part
  "A component, with two different keys" should {
    val executionTimestamp = new DateTime()
    val reports = Seq[Reports](
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "foo", executionTimestamp, "message"),
        new ResultSuccessReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "bar", executionTimestamp, "message")
    )

    val badReports = Seq[Reports](
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "foo", executionTimestamp, "message"),
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "foo", executionTimestamp, "message"),
        new ResultSuccessReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "bar", executionTimestamp, "message")
    )

    val expectedComponent = new ReportComponent(
        "component"
      , 2
      , Seq("foo", "bar")
      , Seq("foo", "bar")
    )

    val getComponentStatus = (r:Seq[Reports]) => ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, r, NoAnswerReportType)

    "return a component globally repaired " in {
      getComponentStatus(reports).reportType === RepairedReportType
    }
    "return a component with two key values " in {
      getComponentStatus(reports).componentValues.size === 2
    }
    "return a component with the key values foo which is repaired " in {
      getComponentStatus(reports).componentValues.filter(x => x.componentValue == "foo").size === 1 and
       getComponentStatus(reports).componentValues.filter(x => x.componentValue == "foo").head.reportType ===  RepairedReportType
     }
     "return a component with the key values bar which is a success " in {
      getComponentStatus(reports).componentValues.filter(x => x.componentValue == "bar").size === 1 and
       getComponentStatus(reports).componentValues.filter(x => x.componentValue == "bar").head.reportType ===  SuccessReportType
     }

     " with bad reports return a component globally unknwon" in {
      getComponentStatus(badReports).reportType === UnknownReportType
     }
     "with bad reports return a component with two key values " in {
      getComponentStatus(badReports).componentValues.size === 2
    }
    "with bad reports return a component with the key values foo which is unknwon " in {
      getComponentStatus(badReports).componentValues.filter(x => x.componentValue == "foo").size === 1 and
       getComponentStatus(badReports).componentValues.filter(x => x.componentValue == "foo").head.reportType ===  UnknownReportType
     }
     "with bad reports return a component with the key values bar which is a success " in {
      getComponentStatus(badReports).componentValues.filter(x => x.componentValue == "bar").size === 1 and
       getComponentStatus(badReports).componentValues.filter(x => x.componentValue == "bar").head.reportType ===  SuccessReportType
     }

  }

  // Test the component part
  "A component, with a None keys" should {
    val executionTimestamp = new DateTime()
    val reports = Seq[Reports](
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "None", executionTimestamp, "message"),
        new ResultSuccessReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "None", executionTimestamp, "message")
    )

    val badReports = Seq[Reports](
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "None", executionTimestamp, "message"),
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "None", executionTimestamp, "message"),
        new ResultSuccessReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "None", executionTimestamp, "message")
    )

    val expectedComponent = new ReportComponent(
        "component"
      , 2
      , Seq("None", "None")
      , Seq("None", "None")
    )
    val getComponentStatus = (r:Seq[Reports]) => ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, r, NoAnswerReportType)

    "return a component globally repaired " in {
      getComponentStatus(reports).reportType === RepairedReportType
    }
    "return a component with two key values " in {
      getComponentStatus(reports).componentValues.size === 2
    }
    "return a component with both None key repaired " in {
      getComponentStatus(reports).componentValues.filter(x => x.componentValue == "None").size === 2 and
       getComponentStatus(reports).componentValues.filter(x => x.componentValue == "None").forall(x => x.reportType === RepairedReportType)
    }

    "with bad reports return a component globally unknown " in {
      getComponentStatus(badReports).reportType === UnknownReportType
    }
    "with bad reports return a component with two key values " in {
      getComponentStatus(badReports).componentValues.size === 2
    }
    "with bad reports return a component with both None key unknown " in {
      getComponentStatus(badReports).componentValues.filter(x => x.componentValue == "None").size === 2 and
       getComponentStatus(badReports).componentValues.filter(x => x.componentValue == "None").forall(x => x.reportType === UnknownReportType)
    }
  }

  // Test the component part
  "A component, with a cfengine keys" should {
    val executionTimestamp = new DateTime()
    val reports = Seq[Reports](
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "/var/cfengine", executionTimestamp, "message"),
        new ResultSuccessReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "/var/cfengine", executionTimestamp, "message")
    )

    val badReports = Seq[Reports](
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "/var/cfengine", executionTimestamp, "message"),
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "/var/cfengine", executionTimestamp, "message"),
        new ResultSuccessReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "/var/cfengine", executionTimestamp, "message")
    )

    val expectedComponent = new ReportComponent("component", 2
      , Seq("${sys.bla}", "${sys.foo}")
      , Seq("${sys.bla}", "${sys.foo}")
    )
    val getComponentStatus = (r:Seq[Reports]) => ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, r, NoAnswerReportType)

    "return a component globally repaired " in {
      getComponentStatus(reports).reportType === RepairedReportType
    }
    "return a component with two key values " in {
      getComponentStatus(reports).componentValues.size === 2
    }
    "return a component with both cfengine keys repaired " in {
      getComponentStatus(reports).componentValues.filter(x => x.componentValue == "${sys.bla}").size === 1 and
       getComponentStatus(reports).componentValues.forall(x => x.reportType === RepairedReportType)
    }
  }

  // Test the component part
  "A component, with distinguishable keys" should {
    val executionTimestamp = new DateTime()
    val reports = Seq[Reports](
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "/var/cfengine", executionTimestamp, "message"),
        new ResultSuccessReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "bar", executionTimestamp, "message")
    )

    val badReports = Seq[Reports](
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "/var/cfengine", executionTimestamp, "message"),
        new ResultRepairedReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "/var/cfengine", executionTimestamp, "message"),
        new ResultSuccessReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "bar", executionTimestamp, "message")
    )

    val expectedComponent = new ReportComponent("component", 2
      , Seq("${sys.bla}", "bar")
      , Seq("${sys.bla}", "bar")
    )

    val getComponentStatus = (r:Seq[Reports]) => ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, r, NoAnswerReportType)

    "return a component globally repaired " in {
      getComponentStatus(reports).reportType === RepairedReportType
    }
    "return a component with two key values " in {
      getComponentStatus(reports).componentValues.size === 2
    }
    "return a component with the cfengine keys repaired " in {
      getComponentStatus(reports).componentValues.filter(x => x.componentValue == "${sys.bla}").size === 1 and
       getComponentStatus(reports).componentValues.filter(x => x.componentValue == "${sys.bla}").forall(x => x.reportType === RepairedReportType)
    }
    "return a component with the bar key success " in {
      getComponentStatus(reports).componentValues.filter(x => x.componentValue == "bar").size === 1 and
       getComponentStatus(reports).componentValues.filter(x => x.componentValue == "bar").forall(x => x.reportType === SuccessReportType)
    }
    "with bad reports return a component globally unknown " in {
      getComponentStatus(badReports).reportType === UnknownReportType
    }
    "with bad reports return a component with two key values " in {
      getComponentStatus(badReports).componentValues.size === 2
    }
    "with bad reports return a component with bar as a success " in {
      getComponentStatus(badReports).componentValues.filter(x => x.componentValue == "bar").size === 1 and
       getComponentStatus(badReports).componentValues.filter(x => x.componentValue == "bar").forall(x => x.reportType === SuccessReportType)
    }
    "with bad reports return a component with the cfengine key as unknown " in {
      getComponentStatus(badReports).componentValues.filter(x => x.componentValue == "${sys.bla}").size === 1 and
       getComponentStatus(badReports).componentValues.filter(x => x.componentValue == "${sys.bla}").forall(x => x.reportType === UnknownReportType)
    }
  }

  "A detailed execution Batch, with one component, cardinality one, one node" should {

    val param = (
        RuleExpectedReports(
            "rule"
          , 1
          , Seq(DirectivesOnNodes(42
              , Seq("one")
              , Seq(
                  DirectiveExpectedReports("policy"
                    , Seq(new ReportComponent("component", 1, Seq("value"), Seq() ))
                  )
                )
            ))
        )
      , Seq[Reports](new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value",DateTime.now(), "message"))
      , 5
      , FullCompliance
    )

    val nodeStatus = getNodeStatusByRule(param)
    val ruleStatus = getRuleStatus(param._1, nodeStatus)


    "have one detailed reports when we create it with one report" in {
      nodeStatus.size ==1
    }

    "have one detailed success node when we create it with one success report" in {
      nodeStatus.head.nodeId === str2nodeId("one") and
      nodeStatus.head.reportType === SuccessReportType
    }

    "have no detailed repaired node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == RepairedReportType).size === 0)
    }

    "have no detailed error node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == ErrorReportType).size === 0)
    }

    "have no detailed unknown node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == UnknownReportType).size === 0)
    }

    "have no detailed no answer node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == NoAnswerReportType).size === 0)
    }

    "have no detailed no pending node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == PendingReportType).size == 0)
    }

    "have one detailed rule reports when we create it with one report" in {
      ruleStatus.size ===1
    }

    "have one detailed rule success directive when we create it with one success report" in {
      ruleStatus.head.directiveId === DirectiveId("policy") and
      ruleStatus.head.compliance === ComplianceLevel(success = 100)
    }

    "have no detailed rule non-success directive when we create it with one success report" in {
      val c = ComplianceLevel.sum(ruleStatus.map(_.compliance))
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      )
    }
  }

  "A detailed execution Batch, with one component, cardinality one, wrong node" should {
    val param = (
        RuleExpectedReports(
            "rule"
          , 1
          , Seq(DirectivesOnNodes(42
              , Seq("one")
              , Seq(
                  DirectiveExpectedReports("policy"
                    , Seq(new ReportComponent("component", 1, Seq("value"), Seq() ))
                  )
                )
            ))
        )
      , Seq[Reports](new ResultSuccessReport(DateTime.now(), "rule", "policy", "two", 12, "component", "value",DateTime.now(), "message"))
      , 5
      , FullCompliance
    )

    val nodeStatus = getNodeStatusByRule(param)
    val ruleStatus = getRuleStatus(param._1, nodeStatus)

    "have one detailed reports when we create it with one report" in {
      nodeStatus.size ==1
    }

    "have a pending node when we create it with one wrong success report right now" in {
      nodeStatus.head.nodeId === str2nodeId("one") and
      nodeStatus.head.reportType === PendingReportType
    }

    "have no detailed success node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == SuccessReportType).size === 0)
    }

    "have no detailed repaired node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == RepairedReportType).size === 0)
    }

    "have no detailed error node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == ErrorReportType).size === 0)
    }

    "have no detailed unknown node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == UnknownReportType).size === 0)
    }

    "have no detailed no answer node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == NoAnswerReportType).size === 0)
    }
  }

  "A detailed execution Batch, with one component, cardinality one, one node" should {

    val param = (
        RuleExpectedReports(
            "rule"
          , 1
          , Seq(DirectivesOnNodes(42
              , Seq("one")
              , Seq(
                  DirectiveExpectedReports("policy"
                    , Seq(new ReportComponent("component", 1, Seq("value"), Seq() ))
                  )
                )
            ))
         )
       , Seq[Reports](
             new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value",DateTime.now(), "message")
           , new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value",DateTime.now(), "message")
         )
      , 5
      , FullCompliance
    )
    val nodeStatus = getNodeStatusByRule(param)
    val ruleStatus = getRuleStatus(param._1, nodeStatus)

    "have one detailed reports when we create it" in {
      nodeStatus.size ==1
    }

    "have one unknown node when we create it with one success report" in {
      nodeStatus.head.nodeId === str2nodeId("one") and
      nodeStatus.head.reportType === UnknownReportType
    }

    "have no detailed repaired node when we create it with one extra success report" in {
      (nodeStatus.filter(x => x.reportType == RepairedReportType).size === 0)
    }

    "have no detailed error node when we create it with  one extra success report" in {
      (nodeStatus.filter(x => x.reportType == ErrorReportType).size === 0)
    }

    "have no detailed success node when we create it with  one extra success report" in {
      (nodeStatus.filter(x => x.reportType == SuccessReportType).size === 0)
    }

    "have no detailed no answer node when we create it with  one extra success report" in {
      (nodeStatus.filter(x => x.reportType == NoAnswerReportType).size === 0)
    }

    "have no detailed no pending node when we create it with  one extra success report" in {
      (nodeStatus.filter(x => x.reportType == PendingReportType).size === 0)
    }

    "have one rule detailed reports when we create it" in {
      ruleStatus.size ==1
    }
  }

   "A detailed execution Batch, with one component, cardinality one, two nodes, including one not responding" should {
    val param = (
        RuleExpectedReports(
            "rule"
          , 1
          , Seq(DirectivesOnNodes(42
              , Seq("one", "two")
              , Seq(
                  DirectiveExpectedReports("policy"
                    , Seq(new ReportComponent("component", 1, Seq("value"), Seq() ))
                  )
                )
           ))
        )
      , Seq[Reports](new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value",DateTime.now(), "message"))
      , 5
      , FullCompliance
    )
    val nodeStatus = getNodeStatusByRule(param)
    val ruleStatus = getRuleStatus(param._1, nodeStatus)

    "have two detailed reports when we create it" in {
      nodeStatus.size == 2
    }

    "have one success node" in {
      nodeStatus.exists(x => x.reportType == SuccessReportType)
    }
    "have one pending node" in {
      nodeStatus.exists(x => x.reportType == PendingReportType)
    }
    "have one component per node" in {
      nodeStatus.
            filter(x => x.reportType == SuccessReportType).head.
            directives.head.components.head.componentValues.size == 1
    }

    "have one detailed rule report" in {
      ruleStatus.size == 1
    }
    "have one pending directive" in {
      ruleStatus.exists(x => x.compliance.pending > 0)
    }
    "have one success, and one pending node, in the component detail of the rule" in {
      (ruleStatus.head.components.head.componentValues.head.nodesReport.size == 2) and
      (ruleStatus.head.components.head.componentValues.head.nodesReport.exists(x => x.node == NodeId("one") && x.reportType == SuccessReportType))
    }
  }

  "A detailed execution Batch, with one component, cardinality one, three nodes, including one not responding" should {
    val param = (
        RuleExpectedReports(
            "rule"
          , 1
          , Seq(DirectivesOnNodes(42
              , Seq("one", "two", "three")
              , Seq(
                  DirectiveExpectedReports("policy"
                    , Seq(new ReportComponent("component", 1, Seq("value"), Seq() ))
                  )
                )
           ))
         )
       , Seq[Reports](
             new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value",DateTime.now(), "message")
           , new ResultSuccessReport(DateTime.now(), "rule", "policy", "two", 12, "component", "value",DateTime.now(), "message")
         )
      , 5
      , FullCompliance
    )
    val nodeStatus = getNodeStatusByRule(param)
    val ruleStatus = getRuleStatus(param._1, nodeStatus)

    "have one detailed rule report" in {
      ruleStatus.size == 1
    }
    "have one pending directive" in {
      ruleStatus.exists(x => x.compliance.pending > 0)
    }
    "have one detailed rule report with a 67% compliance" in {
      ruleStatus.head.compliance.success must beEqualTo(67)
    }
    "have one detailed rule report with a component of 67% compliance" in {
      ruleStatus.head.components.head.compliance must beEqualTo(67)
    }
  }

  "A detailed execution Batch, with two directive, two component, cardinality one, three nodes, including one partly responding and one not responding" should {
    val param = (
        RuleExpectedReports(
            "rule"
          , 1
          , Seq(DirectivesOnNodes(42
              , Seq("one", "two", "three")
              , Seq(
                    DirectiveExpectedReports("policy", Seq(
                         new ReportComponent("component", 1, Seq("value"), Seq() )
                       , new ReportComponent("component2", 1, Seq("value"), Seq() )
                     ))
                   , DirectiveExpectedReports("policy2", Seq(
                         new ReportComponent("component", 1, Seq("value"), Seq() )
                       , new ReportComponent("component2", 1, Seq("value"), Seq() )
                     ))
                )
            ))
        )
      , Seq[Reports](
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component2", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy2", "one", 12, "component", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy2", "one", 12, "component2", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "two", 12, "component", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "two", 12, "component2", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy2", "two", 12, "component", "value",DateTime.now(), "message")
        )
      , 5
      , FullCompliance
    )
    val nodeStatus = getNodeStatusByRule(param)
    val ruleStatus = getRuleStatus(param._1, nodeStatus)

    "have two detailed rule report" in {
      ruleStatus.size must beEqualTo(2)
    }
    "have two pending directives" in {
      ruleStatus.filter(x => x.compliance.pending > 0).size must beEqualTo(2)
    }
    "have detailed rule report for policy of 67%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy")).head.compliance.success must beEqualTo(67)
    }
    "have detailed rule report for policy2 of 33%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy2")).head.compliance.success must beEqualTo(33)
    }
  }

  "A detailed execution Batch, with two directive, two component, cardinality three, three nodes, including two not responding" should {
    val param = (
        RuleExpectedReports(
            "rule"
          , 1
          , Seq(DirectivesOnNodes(42
               , Seq("one", "two", "three")
               , Seq(
                     DirectiveExpectedReports("policy", Seq(
                         new ReportComponent("component", 1, Seq("value"), Seq() )
                       , new ReportComponent("component2", 1, Seq("value"), Seq() )
                     ))
                   , DirectiveExpectedReports("policy2",Seq(
                         new ReportComponent("component", 1, Seq("value"), Seq() )
                       , new ReportComponent("component2", 1, Seq("value"), Seq() )
                     ))
                 )
            ))
        )
      , Seq[Reports](
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component2", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy2", "one", 12, "component", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy2", "one", 12, "component2", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "two", 12, "component", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "two", 12, "component2", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy2", "two", 12, "component", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "three", 12, "component", "value",DateTime.now(), "message")
        )
      , 5
      , FullCompliance
    )
    val nodeStatus = getNodeStatusByRule(param)
    val ruleStatus = getRuleStatus(param._1, nodeStatus)

    "have two detailed rule report" in {
      ruleStatus.size must beEqualTo(2)
    }
    "have two pending directives" in {
      ruleStatus.filter(x => x.compliance.pending > 0).size must beEqualTo(2)
    }
    "have detailed rule report for policy of 67%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy")).head.compliance.success must beEqualTo(67)
    }
    "have detailed rule report for policy2 of 33%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy2")).head.compliance.success must beEqualTo(33)
    }
    "have detailed rule report for policy-component of 100%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy")).head.components.filter(x => x.component == "component").head.compliance.success must beEqualTo(100)
    }
    "have detailed rule report for policy-component2 of 67%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy")).head.components.filter(x => x.component == "component2").head.compliance.success must beEqualTo(67)
    }
    "have detailed rule report for policy2-component2 of 33%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy2")).head.components.filter(x => x.component == "component2").head.compliance.success must beEqualTo(33)
    }
  }

  "A detailed execution Batch, with two directive, two component, cardinality three, three nodes, including two not completely responding" should {
    val param = (
        RuleExpectedReports(
            "rule"
          , 1
          , Seq(DirectivesOnNodes(42
               , Seq("one", "two", "three")
               , Seq(
                   DirectiveExpectedReports("policy", Seq(
                       new ReportComponent("component", 1, Seq("value", "value2", "value3"), Seq() )
                   ))
                 )
            ))
        )
      , Seq[Reports](
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value2",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value3",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "two", 12, "component", "value",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "two", 12, "component", "value2",DateTime.now(), "message"),
          new ResultSuccessReport(DateTime.now(), "rule", "policy", "three", 12, "component", "value",DateTime.now(), "message")
        )
      , 5
      , FullCompliance
    )
    val nodeStatus = getNodeStatusByRule(param)
    val ruleStatus = getRuleStatus(param._1, nodeStatus)

    "have one detailed rule report" in {
      ruleStatus.size must beEqualTo(1)
    }
    "have one pending directives" in {
      ruleStatus.filter(x => x.compliance.pending > 0).size must beEqualTo(1)
    }
    "have detailed rule report for policy of 33%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy")).head.compliance.success must beEqualTo(33)
    }
    "have detailed rule report for policy-component of 33%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy")).head.components.filter(x => x.component == "component").head.compliance.success must beEqualTo(33)
    }
    "have detailed rule report for policy-component-value of 100%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy")).head.components.filter(x => x.component == "component").head.componentValues.filter(_.componentValue == "value").head.compliance.success must beEqualTo(100)
    }
    "have detailed rule report for policy-component-value2 of 67%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy")).head.components.filter(x => x.component == "component").head.componentValues.filter(_.componentValue == "value2").head.compliance.success must beEqualTo(67)
    }
    "have detailed rule report for policy-component-value of 33%" in {
      ruleStatus.filter(x => x.directiveId == new DirectiveId("policy")).head.components.filter(x => x.component == "component").head.componentValues.filter(_.componentValue == "value3").head.compliance.success must beEqualTo(33)
    }
  }

   "An execution Batch, with one component with a quote in its value, cardinality one, one node" should {

    val param = (
        RuleExpectedReports(
            "rule"
          , 1
          , Seq(DirectivesOnNodes(42
              , Seq("one")
              , Seq(
                  DirectiveExpectedReports("policy", Seq(
                      new ReportComponent("component", 1, Seq("""some\"text"""), Seq("""some\text""") )
                  ))
                )
            ))
        )
      , Seq[Reports](new ResultSuccessReport(new DateTime(), "rule", "policy", "one", 12, "component", """some\"text""",new DateTime(), "message"))
      , 5
      , FullCompliance
    )
    val nodeStatus = getNodeStatusByRule(param)
    val ruleStatus = getRuleStatus(param._1, nodeStatus)

    "have one detailed reports when we create it with one report" in {
      nodeStatus.size ===1
    }

    "have one detailed success node when we create it with one success report" in {
      nodeStatus.head.nodeId === str2nodeId("one") &&
      nodeStatus.head.reportType === SuccessReportType
    }

    "have no detailed repaired node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == RepairedReportType).size === 0)
    }

    "have no detailed error node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == ErrorReportType).size === 0)
    }

    "have no detailed unknown node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == UnknownReportType).size === 0)
    }


     "have one detailed rule reports when we create it with one report" in {
      ruleStatus.size ===1
    }

    "have one detailed rule success directive when we create it with one success report" in {
      ruleStatus.head.directiveId === DirectiveId("policy") &&
      ruleStatus.head.compliance.pc_success === 100
    }

    "have no detailed rule non-success directive when we create it with one success report" in {
      val c = ComplianceLevel.sum(ruleStatus.map(_.compliance))

      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      )
    }
  }

 "An execution Batch, with one component, one node, but with a component value being a cfengine variable with {, and a an escaped quote as well" should {

    val param = (
        RuleExpectedReports(
            "rule"
          , 1
          , Seq(DirectivesOnNodes(42
              , Seq("nodeId")
              , Seq(
                  DirectiveExpectedReports("policy", Seq(
                      new ReportComponent("component", 1, Seq("""${sys.workdir}/inputs/\"test"""), Seq() )
                  ))
                )
            ))
        )
      , Seq[Reports](new ResultSuccessReport(new DateTime(), "rule", "policy", "nodeId", 12, "component", """/var/cfengine/inputs/\"test""", new DateTime(), "message"))
      , 5
      , FullCompliance
    )

    val nodeStatus = getNodeStatusByRule(param)
    val ruleStatus = getRuleStatus(param._1, nodeStatus)

    "have one detailed reports when we create it with one report" in {
     nodeStatus.size ===1
    }

    "have one detailed success node when we create it with one success report" in {
     nodeStatus.head.nodeId === str2nodeId("nodeId") &&
     nodeStatus.head.reportType === SuccessReportType
    }

    "have no detailed repaired node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == RepairedReportType).size === 0)
    }

    "have no detailed error node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == ErrorReportType).size === 0)
    }

    "have no detailed unknown node when we create it with one success report" in {
      (nodeStatus.filter(x => x.reportType == UnknownReportType).size === 0)
    }


     "have one detailed rule reports when we create it with one report" in {
      ruleStatus.size ===1
    }

    "have one detailed rule success directive when we create it with one success report" in {
      ruleStatus.head.directiveId === DirectiveId("policy") &&
      ruleStatus.head.compliance.success === 100
    }

    "have no detailed rule non-success directive when we create it with one success report" in {
      val c = ComplianceLevel.sum(ruleStatus.map(_.compliance))

      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      )
    }
  }
  "An execution Batch, with one component, one node, but with a component value being a cfengine variable with {, and a quote as well" should {
    val param = (
        RuleExpectedReports(
            "rule"
          , 1
          , Seq(DirectivesOnNodes(42
              , Seq("nodeId")
              , Seq(
                  DirectiveExpectedReports("policy", Seq(
                    new ReportComponent("component", 1, Seq("""${sys.workdir}/inputs/"test"""), Seq("""${sys.workdir}/inputs/"test""") )
                  ))
                )
           ))
        )
      , Seq[Reports](new ResultSuccessReport(new DateTime(), "rule", "policy", "nodeId", 12, "component", """/var/cfengine/inputs/"test""", new DateTime(), "message"))
      , 5
      , FullCompliance
    )
    val nodeStatus = getNodeStatusByRule(param)
    val ruleStatus = getRuleStatus(param._1, nodeStatus)

    "have one detailed reports when we create it with one report" in {
     nodeStatus.size === 1
    }

    "have one detailed success node when we create it with one success report" in {
     nodeStatus.head.nodeId === str2nodeId("nodeId") and
     nodeStatus.head.reportType === SuccessReportType
    }

    "have no detailed repaired node when we create it with one success report" in {
      nodeStatus.filter(x => x.reportType == RepairedReportType).size === 0
    }

    "have no detailed error node when we create it with one success report" in {
      nodeStatus.filter(x => x.reportType == ErrorReportType).size === 0
    }

    "have no detailed unknown node when we create it with one success report" in {
      nodeStatus.filter(x => x.reportType == UnknownReportType).size === 0
    }


     "have one detailed rule reports when we create it with one report" in {
      ruleStatus.size === 1
    }

    "have one detailed rule success directive when we create it with one success report" in {
      ruleStatus.head.directiveId === DirectiveId("policy") and
      ruleStatus.head.compliance.success === 100
    }

    "have no detailed rule non-success directive when we create it with one success report" in {
      val c = ComplianceLevel.sum(ruleStatus.map(_.compliance))

      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      )
    }

  }

   // Test the component part - with NotApplicable
  "A component, with two keys and NotApplicable reports" should {
    val executionTimestamp = new DateTime()
    val reports = Seq[Reports](
        new ResultNotApplicableReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "/var/cfengine", executionTimestamp, "message"),
        new ResultSuccessReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "bar", executionTimestamp, "message")
              )

    val expectedComponent = new ReportComponent(
        "component"
      , 2
      , Seq("/var/cfengine", "bar")
      , Seq("/var/cfengine", "bar")
    )

    val getComponentStatus = (r:Seq[Reports]) => ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, r, NoAnswerReportType)

    "return a component globally success " in {
      getComponentStatus(reports).reportType == SuccessReportType
    }
    "return a component with two key values " in {
      getComponentStatus(reports).componentValues.size == 2
    }
    "return a component with the /var/cfengine in NotApplicable " in {
      getComponentStatus(reports).componentValues.filter(x => x.componentValue == "/var/cfengine").size == 1 and
       getComponentStatus(reports).componentValues.filter(x => x.componentValue == "/var/cfengine").forall(x => x.reportType == NotApplicableReportType)
    }
    "return a component with the bar key success " in {
      getComponentStatus(reports).componentValues.filter(x => x.componentValue == "bar").size == 1 and
       getComponentStatus(reports).componentValues.filter(x => x.componentValue == "bar").forall(x => x.reportType == SuccessReportType)
    }
  }

}