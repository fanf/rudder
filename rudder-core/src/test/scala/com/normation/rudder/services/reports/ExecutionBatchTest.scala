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
  ): Seq[RuleNodeStatusReport] = {
    (for {
      directiveOnNode   <- ruleExpectedReports.directivesOnNodes
      (nodeId, version) <- directiveOnNode.nodeConfigurationIds
    } yield {
      ExecutionBatch.getNodeStatusReports(nodeId, None, version, Seq(ruleExpectedReports), reportsParam, agentExecutionInterval, complianceMode)
    }).flatten
  }

  val getNodeStatusByRule = (getNodeStatusReportsByRule _).tupled



   //Test the component part
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

    val expectedComponent = new ComponentExpectedReport(
        "component"
      , 2
      , Seq("foo", "bar")
      , Seq("foo", "bar")
    )

    val getComponentStatus = (r:Seq[Reports]) => ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, r, NoAnswerReportType)

    "return a component globally repaired " in {
      getComponentStatus(reports).compliance === ComplianceLevel(success = 1, repaired = 1)
    }
    "return a component with two key values " in {
      getComponentStatus(reports).componentValues.size === 2
    }
    "return a component with the key values foo which is repaired " in {
      getComponentStatus(reports).componentValues("foo").messages.size === 1 and
      getComponentStatus(reports).componentValues("foo").messages.head.reportType ===  RepairedReportType
    }
    "return a component with the key values bar which is a success " in {
      getComponentStatus(reports).componentValues("bar").messages.size === 1 and
      getComponentStatus(reports).componentValues("bar").messages.head.reportType ===  SuccessReportType
    }

    " with bad reports return a component globally unknwon" in {
      getComponentStatus(badReports).compliance === ComplianceLevel(success = 1, unknown = 2)
    }
    "with bad reports return a component with two key values " in {
      getComponentStatus(badReports).componentValues.size === 2
    }
    "with bad reports return a component with the key values foo which is unknwon " in {
      getComponentStatus(badReports).componentValues("foo").messages.size === 2 and
      getComponentStatus(badReports).componentValues("foo").messages.head.reportType ===  UnknownReportType
    }
    "with bad reports return a component with the key values bar which is a success " in {
      getComponentStatus(badReports).componentValues("bar").messages.size === 1 and
      getComponentStatus(badReports).componentValues("bar").messages.head.reportType ===  SuccessReportType
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

    val expectedComponent = new ComponentExpectedReport(
        "component"
      , 2
      , Seq("None", "None")
      , Seq("None", "None")
    )
    val withGood = ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, reports, NoAnswerReportType)
    val witBad   = ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, badReports, NoAnswerReportType)

    "return a component globally repaired " in {
      withGood.compliance === ComplianceLevel(success = 1, repaired = 1)
    }
    "return a component with two key values " in {
      withGood.componentValues.size === 1
    }
    "return a component with both None key repaired " in {
      withGood.componentValues("None").messages.size === 2 and
      withGood.componentValues("None").compliance === ComplianceLevel(success = 1, repaired = 1)
    }

    "with bad reports return a component globally unknown " in {
      witBad.compliance === ComplianceLevel(unknown = 3)
    }
    "with bad reports return a component with two key values " in {
      witBad.componentValues.size === 1
    }
    "with bad reports return a component with both None key unknown " in {
      witBad.componentValues("None").messages.size === 3 and
      witBad.componentValues("None").messages.forall(x => x.reportType === UnknownReportType)
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

    val expectedComponent = new ComponentExpectedReport("component", 2
      , Seq("${sys.bla}", "${sys.foo}")
      , Seq("${sys.bla}", "${sys.foo}")
    )

    val withGood = ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, reports, NoAnswerReportType)
    val witBad   = ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, badReports, NoAnswerReportType)

    "return a component globally repaired " in {
      withGood.compliance === ComplianceLevel(success = 1, repaired = 1)
    }
    "return a component with two key values " in {
      withGood.componentValues.size === 2
    }
    "return a component with both cfengine keys repaired " in {
      withGood.componentValues("${sys.bla}").messages.size === 1
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

    val expectedComponent = new ComponentExpectedReport("component", 2
      , Seq("${sys.bla}", "bar")
      , Seq("${sys.bla}", "bar")
    )

    val getComponentStatus = (r:Seq[Reports]) => ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, r, NoAnswerReportType)

    "return a component globally repaired " in {
      getComponentStatus(reports).compliance === ComplianceLevel(success = 1, repaired = 1)
    }
    "return a component with two key values " in {
      getComponentStatus(reports).componentValues.size === 2
    }
    "return a component with the cfengine keys repaired " in {
      getComponentStatus(reports).componentValues("${sys.bla}").messages.size === 1 and
       getComponentStatus(reports).componentValues("${sys.bla}").messages.forall(x => x.reportType === RepairedReportType)
    }
    "return a component with the bar key success " in {
      getComponentStatus(reports).componentValues("bar").messages.size === 1 and
       getComponentStatus(reports).componentValues("bar").messages.forall(x => x.reportType === SuccessReportType)
    }
    "with bad reports return a component globally unknown " in {
      getComponentStatus(badReports).compliance ===  ComplianceLevel(unknown = 2)
    }
    "with bad reports return a component with two key values " in {
      getComponentStatus(badReports).componentValues.size === 2
    }
    "with bad reports return a component with bar as a success " in {
      getComponentStatus(badReports).componentValues("bar").messages.size === 1 and
       getComponentStatus(badReports).componentValues("bar").messages.forall(x => x.reportType === SuccessReportType)
    }
    "with bad reports return a component with the cfengine key as unknown " in {
      getComponentStatus(badReports).componentValues("${sys.bla}").messages.size === 1 and
       getComponentStatus(badReports).componentValues("${sys.bla}").messages.forall(x => x.reportType === UnknownReportType)
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
                    , Seq(new ComponentExpectedReport("component", 1, Seq("value"), Seq() ))
                  )
                )
            ))
        )
      , Seq[Reports](new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value",DateTime.now(), "message"))
      , 5
      , FullCompliance
    )



    val nodeStatus = (getNodeStatusReportsByRule _).tupled(param)


    "have one detailed reports when we create it with one report" in {
      nodeStatus.size ==1
    }

    "have one detailed success node when we create it with one success report" in {
      nodeStatus.head.nodeId === str2nodeId("one")
    }


    "have one detailed rule success directive when we create it with one success report" in {
      nodeStatus.head.directives.head._1 === DirectiveId("policy")
    }

    "have no detailed rule non-success directive when we create it with one success report" in {
      val c = AggregatedStatusReport(nodeStatus.toSet).compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 100)
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
                    , Seq(new ComponentExpectedReport("component", 1, Seq("value"), Seq() ))
                  )
                )
            ))
        )
      , Seq[Reports](new ResultSuccessReport(DateTime.now(), "rule", "policy", "two", 12, "component", "value",DateTime.now(), "message"))
      , 5
      , FullCompliance
    )

    val nodeStatus = getNodeStatusByRule(param)

    "have one detailed reports when we create it with one report" in {
      nodeStatus.size ==1
    }

    "have a pending node when we create it with one wrong success report right now" in {
      val c = AggregatedStatusReport(nodeStatus.toSet).compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 100)
      and (c.pc_unknown === 0)
      and (c.pc_success === 0)
      ) and nodeStatus.head.nodeId === str2nodeId("one")
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
                    , Seq(new ComponentExpectedReport("component", 1, Seq("value"), Seq() ))
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

    "have one detailed reports when we create it" in {
      nodeStatus.size ==1
    }

    "have one unknown node when we create it with one success report" in {
      val c = AggregatedStatusReport(nodeStatus.toSet).compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 100)
      ) and  nodeStatus.head.nodeId === str2nodeId("one")
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
                    , Seq(new ComponentExpectedReport("component", 1, Seq("value"), Seq() ))
                  )
                )
           ))
        )
      , Seq[Reports](new ResultSuccessReport(DateTime.now(), "rule", "policy", "one", 12, "component", "value",DateTime.now(), "message"))
      , 5
      , FullCompliance
    )
    val nodeStatus = getNodeStatusByRule(param)

    "have two detailed reports when we create it" in {
      nodeStatus.size == 2
    }

    "have one success, and one pending node, in the component detail of the rule" in {
      AggregatedStatusReport(nodeStatus.toSet).compliance === ComplianceLevel(success = 1, repaired = 1)
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
                    , Seq(new ComponentExpectedReport("component", 1, Seq("value"), Seq() ))
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

    "have one detailed rule report" in {
      nodeStatus.size == 1
    }
    "have one detailed rule report with a 67% compliance" in {
      val c = AggregatedStatusReport(nodeStatus.toSet).compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 33)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 67)
      )

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
                         new ComponentExpectedReport("component", 1, Seq("value"), Seq() )
                       , new ComponentExpectedReport("component2", 1, Seq("value"), Seq() )
                     ))
                   , DirectiveExpectedReports("policy2", Seq(
                         new ComponentExpectedReport("component", 1, Seq("value"), Seq() )
                       , new ComponentExpectedReport("component2", 1, Seq("value"), Seq() )
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
    val aggregated = AggregatedStatusReport(nodeStatus.toSet)

    "have two detailed rule report" in {
      nodeStatus.size must beEqualTo(2)
    }
    "have detailed rule report for policy of 67%" in {
      val c = aggregated.directives("policy").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 33)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 67)
      )
    }
    "have detailed rule report for policy2 of 33%" in {
      val c = aggregated.directives("policy2").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 33)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 67)
      )
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
                         new ComponentExpectedReport("component", 1, Seq("value"), Seq() )
                       , new ComponentExpectedReport("component2", 1, Seq("value"), Seq() )
                     ))
                   , DirectiveExpectedReports("policy2",Seq(
                         new ComponentExpectedReport("component", 1, Seq("value"), Seq() )
                       , new ComponentExpectedReport("component2", 1, Seq("value"), Seq() )
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
    val aggregated = AggregatedStatusReport(nodeStatus.toSet)

    "have two detailed rule report" in {
      nodeStatus.size must beEqualTo(2)
    }
    "have detailed rule report for policy of 67%" in {
      val c = aggregated.directives("policy").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 33)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 67)
      )
    }
    "have detailed rule report for policy2 of 33%" in {
      val c = aggregated.directives("policy2").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 33)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 67)
      )
    }
    "have detailed rule report for policy-component of 100%" in {
      val c = aggregated.directives("policy").components("component").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 100)
      )
    }
    "have detailed rule report for policy-component2 of 67%" in {
      val c = aggregated.directives("policy").components("component").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 67)
      )
    }
    "have detailed rule report for policy2-component2 of 33%" in {
      val c = aggregated.directives("policy2").components("component2").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 33)
      )
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
                       new ComponentExpectedReport("component", 1, Seq("value", "value2", "value3"), Seq() )
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
    val aggregated = AggregatedStatusReport(nodeStatus.toSet)

    "have two detailed rule report" in {
      nodeStatus.size must beEqualTo(2)
    }
    "have detailed rule report for policy of 67%" in {
      val c = aggregated.directives("policy").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 33)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 67)
      )
    }
    "have detailed rule report for policy2 of 33%" in {
      val c = aggregated.directives("policy2").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 33)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 67)
      )
    }
    "have detailed rule report for policy-component of 100%" in {
      val c = aggregated.directives("policy").components("component").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 100)
      )
    }
    "have detailed rule report for policy-component2 of 67%" in {
      val c = aggregated.directives("policy").components("component").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 67)
      )
    }
    "have detailed rule report for policy2-component2 of 33%" in {
      val c = aggregated.directives("policy2").components("component2").compliance
      (   (c.pc_error === 0)
      and (c.pc_noAnswer === 0)
      and (c.pc_notApplicable === 0)
      and (c.pc_repaired === 0)
      and (c.pc_pending === 0)
      and (c.pc_unknown === 0)
      and (c.pc_success === 33)
      )
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
                      new ComponentExpectedReport("component", 1, Seq("""some\"text"""), Seq("""some\text""") )
                  ))
                )
            ))
        )
      , Seq[Reports](new ResultSuccessReport(new DateTime(), "rule", "policy", "one", 12, "component", """some\"text""",new DateTime(), "message"))
      , 5
      , FullCompliance
    )
    val nodeStatus = getNodeStatusByRule(param)

    "have one detailed reports when we create it with one report" in {
      nodeStatus.size ===1
    }

    "have one detailed success node when we create it with one success report" in {
      nodeStatus.head.nodeId === str2nodeId("one") and
      nodeStatus.head.compliance.pc_success === 100
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
                      new ComponentExpectedReport("component", 1, Seq("""${sys.workdir}/inputs/\"test"""), Seq() )
                  ))
                )
            ))
        )
      , Seq[Reports](new ResultSuccessReport(new DateTime(), "rule", "policy", "nodeId", 12, "component", """/var/cfengine/inputs/\"test""", new DateTime(), "message"))
      , 5
      , FullCompliance
    )

    val nodeStatus = getNodeStatusByRule(param)

    "have one detailed reports when we create it with one report" in {
     nodeStatus.size ===1
    }

    "have one detailed success node when we create it with one success report" in {
     nodeStatus.head.nodeId === str2nodeId("nodeId") and
     nodeStatus.head.compliance.pc_success === 100
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
                    new ComponentExpectedReport("component", 1, Seq("""${sys.workdir}/inputs/"test"""), Seq("""${sys.workdir}/inputs/"test""") )
                  ))
                )
           ))
        )
      , Seq[Reports](new ResultSuccessReport(new DateTime(), "rule", "policy", "nodeId", 12, "component", """/var/cfengine/inputs/"test""", new DateTime(), "message"))
      , 5
      , FullCompliance
    )
    val nodeStatus = getNodeStatusByRule(param)

    "have one detailed reports when we create it with one report" in {
     nodeStatus.size === 1
    }

    "have one detailed success node when we create it with one success report" in {
     nodeStatus.head.nodeId === str2nodeId("nodeId") and
     nodeStatus.head.compliance.pc_success === 100
    }
  }

   // Test the component part - with NotApplicable
  "A component, with two keys and NotApplicable reports" should {
    val executionTimestamp = new DateTime()
    val reports = Seq[Reports](
        new ResultNotApplicableReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "/var/cfengine", executionTimestamp, "message"),
        new ResultSuccessReport(executionTimestamp, "cr", "policy", "nodeId", 12, "component", "bar", executionTimestamp, "message")
              )

    val expectedComponent = new ComponentExpectedReport(
        "component"
      , 2
      , Seq("/var/cfengine", "bar")
      , Seq("/var/cfengine", "bar")
    )

    val getComponentStatus = (r:Seq[Reports]) => ExecutionBatch.checkExpectedComponentWithReports(expectedComponent, r, NoAnswerReportType)

    "return a component globally success " in {
      getComponentStatus(reports).compliance.pc_success === 100
    }
    "return a component with two key values " in {
      getComponentStatus(reports).componentValues.size === 2
    }
    "return a component with the /var/cfengine in NotApplicable " in {
      getComponentStatus(reports).componentValues("/var/cfengine").componentValue.size === 1 and
      getComponentStatus(reports).componentValues("/var/cfengine").compliance.pc_notApplicable === 100
    }
    "return a component with the bar key success " in {
      getComponentStatus(reports).componentValues("bar").componentValue.size == 1 and
      getComponentStatus(reports).componentValues("bar").compliance.pc_success === 100
    }
  }

}