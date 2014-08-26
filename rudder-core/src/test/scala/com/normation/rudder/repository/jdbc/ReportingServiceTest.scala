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

package com.normation.rudder.repository.jdbc
import scala.slick.driver.PostgresDriver.simple._
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable._
import org.springframework.jdbc.datasource.DataSourceTransactionManager
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.reports.ComponentStatusReport
import com.normation.rudder.domain.reports.ComponentValueStatusReport
import com.normation.rudder.domain.reports.DirectiveStatusReport
import com.normation.rudder.domain.reports.NodeConfigVersion
import com.normation.rudder.domain.reports.NodeStatusReport
import com.normation.rudder.domain.reports.ReportType
import com.normation.rudder.domain.reports.Reports
import com.normation.rudder.domain.reports.SuccessReportType
import com.normation.rudder.migration.DBCommon
import com.normation.rudder.reports.ErrorOnly
import com.normation.rudder.reports.execution.AgentRunId
import com.normation.rudder.reports.execution.ReportExecution
import com.normation.rudder.reports.execution.ReportsExecutionService
import com.normation.rudder.reports.execution.RoReportsExecutionSquerylRepository
import com.normation.rudder.reports.execution.WoReportsExecutionSquerylRepository
import com.normation.rudder.reports.status.StatusUpdateSquerylRepository
import com.normation.rudder.services.reports.ExpectedReportsUpdateImpl
import com.normation.rudder.services.reports.ReportingServiceImpl
import net.liftweb.common.Full
import com.normation.rudder.domain.reports.NodeConfigId
import com.normation.rudder.domain.reports.NodeConfigId
import com.normation.rudder.domain.reports.NodeConfigVersion

/**
 *
 * Test reporting service:
 *
 */
@RunWith(classOf[JUnitRunner])
class ReportingServiceTest extends DBCommon {
System.setProperty("test.postgres", "true")


  //clean data base
  def cleanTables() = {
    jdbcTemplate.execute("DELETE FROM ReportsExecution; DELETE FROM RudderSysEvents;")
  }

  val reportsRepo = new ReportsJdbcRepository(jdbcTemplate)
  val slick = new SlickSchema(dataSource)
  val findExpected = new FindExpectedReportsJdbcRepository(jdbcTemplate, 2)
  val updateExpected = new UpdateExpectedReportsJdbcRepository(jdbcTemplate, new DataSourceTransactionManager(dataSource), findExpected)
  val updateExpectedService = new ExpectedReportsUpdateImpl(updateExpected)

  val roAgentRun = new RoReportsExecutionSquerylRepository(squerylConnectionProvider)
  val woAgentRun = new WoReportsExecutionSquerylRepository(roAgentRun)

  val reportingService = new ReportingServiceImpl(findExpected, reportsRepo, roAgentRun, () => 5, () => Full(ErrorOnly))

  val updateRuns = new ReportsExecutionService(reportsRepo, woAgentRun, new StatusUpdateSquerylRepository(squerylConnectionProvider), 1)

  import slick._

  sequential


  val run1 = DateTime.now.minusMinutes(5*2).withMillisOfSecond(123) //check that millis are actually used
  val run2 = DateTime.now.minusMinutes(5*1)
  //now
  val run3 = DateTime.now.minusMinutes(5*0)

/*
 * We need 5 nodes
 * - n0: no run
 * - n1: a run older than agent frequency, no config version
 * - n2: a run older than agent frequency, a config version n2_t1
 * - n3: both a run newer and older than agent frequency, no config version
 * - n4: both a run newer and older than agent frequency, a config version n4_t2
 *
 * Two expected reports, one for t0, one for t1
 * - at t0:
 * - r0 / d0 / c0 on all nodes
 * - r1 / d1 / c1 on all nodes
 *
 * - at t1:
 * - r0 / d0 / c0 on all nodes
 * - r2 / d2 / c2 on all nodes
 *
 *
 *
 * TODO: need to test for "overridden" unique directive on a node
 */


  val allNodes_t1 = Seq("n0", "n1", "n2", "n3", "n4").map(n => (n, List(n+"_t1")))
  val allNodes_t2 = Seq("n0", "n1", "n2", "n3", "n4").map(n => (n, List(n+"_t2")))

  val expecteds = (
    Map[SlickExpectedReports, Seq[SlickExpectedReportsNodes]]()
    ++ expect("r0", 1)( //r0 @ t1
        (1, "r0_d0", "r0_d0_c0", 1, """["r0_d0_c0_v0"]""", run1, Some(run2), allNodes_t1 )
      , (1, "r0_d0", "r0_d0_c1", 1, """["r0_d0_c1_v0"]""", run1, Some(run2), allNodes_t1 )
    )
    ++ expect("r1", 1)( //r1 @ t1
        (2, "r1_d1", "r1_d1_c0", 1, """["r1_d1_c0_v0"]""", run1, Some(run2), allNodes_t1 )
    )
    ++ expect("r0", 2)( //r0 @ t2
        (3, "r0_d0", "r0_d0_c0", 1, """["r0_d0_c0_v1"]""", run2, None, allNodes_t2 )
      , (3, "r0_d0", "r0_d0_c1", 1, """["r0_d0_c1_v1"]""", run2, None, allNodes_t2 )
    )
    ++ expect("r2", 1)( //r2 @ t2
        (4, "r2_d2", "r2_d2_c0", 1, """["r2_d2_c0_v0"]""", run2, None, allNodes_t2 )
    )
  )

  val reports = (
    Map[String, Seq[Reports]]()
    + node("n0")(
        // no run
    )
    + node("n1")( //run 1: no config version ; no run 2
          ("hasPolicyServer-root", 1, "common", "common", "StartRun", run1, "result_success", "Start execution")
        , ("r0", 1, "r0_d0", "r0_d0_c0", "r0_d0_c0_v0", run1, "result_success", "msg")
        , ("r0", 1, "r0_d0", "r0_d0_c1", "r0_d0_c1_v0", run1, "result_success", "msg")
        , ("r1", 1, "r1_d1", "r1_d1_c0", "r1_d1_c0_v0", run1, "result_success", "msg")
        , ("hasPolicyServer-root", 1, "common", "common", "EndRun", run1, "result_success", "End execution")
    )
    + node("n2")( //run 1: config version ; no run 2
          ("hasPolicyServer-root", 1, "common", "common", "StartRun", run1, "result_success", "Start execution [n2_t1]")
        , ("r0", 1, "r0_d0", "r0_d0_c0", "r0_d0_c0_v0", run1, "result_success", "msg")
        , ("r0", 1, "r0_d0", "r0_d0_c1", "r0_d0_c1_v0", run1, "result_success", "msg")
        , ("r1", 1, "r1_d1", "r1_d1_c0", "r1_d1_c0_v0", run1, "result_success", "msg")
        , ("hasPolicyServer-root", 1, "common", "common", "EndRun", run1, "result_success", "End execution [n2_t1]")
    )
    + node("n3")( //run 1 and run 2 : no config version
          ("hasPolicyServer-root", 1, "common", "common", "StartRun", run1, "result_success", "Start execution")
        , ("r0", 1, "r0_d0", "r0_d0_c0", "r0_d0_c0_v0", run1, "result_success", "msg")
        , ("r0", 1, "r0_d0", "r0_d0_c1", "r0_d0_c1_v0", run1, "result_success", "msg")
        , ("r1", 1, "r1_d1", "r1_d1_c0", "r1_d1_c0_v0", run1, "result_success", "msg")
        , ("hasPolicyServer-root", 1, "common", "common", "EndRun", run1, "result_success", "End execution")
        , ("hasPolicyServer-root", 1, "common", "common", "StartRun", run2, "result_success", "Start execution")
        , ("r0", 2, "r0_d0", "r0_d0_c0", "r0_d0_c0_v1", run2, "result_success", "msg")
        , ("r0", 2, "r0_d0", "r0_d0_c1", "r0_d0_c1_v1", run2, "result_success", "msg")
        , ("r2", 1, "r2_d2", "r2_d2_c0", "r2_d2_c0_v0", run2, "result_success", "msg")
        , ("hasPolicyServer-root", 1, "common", "common", "EndRun", run2, "result_success", "End execution")
    )
    + node("n4")( //run 1 and run 2 : one config version
          ("hasPolicyServer-root", 1, "common", "common", "StartRun", run1, "result_success", "Start execution [n4_t1]")
        , ("r0", 1, "r0_d0", "r0_d0_c0", "r0_d0_c0_v0", run1, "result_success", "msg")
        , ("r0", 1, "r0_d0", "r0_d0_c1", "r0_d0_c1_v0", run1, "result_success", "msg")
        , ("r1", 1, "r1_d1", "r1_d1_c0", "r1_d1_c0_v0", run1, "result_success", "msg")
        , ("hasPolicyServer-root", 1, "common", "common", "EndRun", run1, "result_success", "End execution [n4_t1]")
        , ("hasPolicyServer-root", 1, "common", "common", "StartRun", run2, "result_success", "Start execution [n4_t2]")
        , ("r0", 2, "r0_d0", "r0_d0_c0", "r0_d0_c0_v1", run2, "result_success", "msg")
        , ("r0", 2, "r0_d0", "r0_d0_c1", "r0_d0_c1_v1", run2, "result_success", "msg")
        , ("r2", 1, "r2_d2", "r2_d2_c0", "r2_d2_c0_v0", run2, "result_success", "msg")
        , ("hasPolicyServer-root", 1, "common", "common", "EndRun", run2, "result_success", "End execution [n4_t2]")
    )
  )

  step {
    slick.insertReports(reports.values.toSeq.flatten)
    slickExec { implicit session =>

      expectedReportsTable ++= expecteds.keySet
      expectedReportsNodesTable ++= expecteds.values.toSet.flatten

    }

    updateRuns.findAndSaveExecutions(42)
    updateRuns.findAndSaveExecutions(43)


  }


  "Testing expected reports" should {  //be in ExpectedReportsTest!
    //essentially test the combination of in/in(values clause

    "be correct for in(tuple) clause" in {
      val res = findExpected.getExpectedReports(configs(("n1","n1_t1")), Set()).openOrThrowException("'Test failled'")
      println(res)
      res.size must beEqualTo(2)
    }
    "be correct for in(value(tuple)) clause" in {
      val res = findExpected.getExpectedReports(configs(("n1","n1_t1"),("n2","n2_t1")), Set()).openOrThrowException("'Test failled'")
      res.size must beEqualTo(2)
    }

    "be correct for in('string') clause" in {
      val res = findExpected.getLastExpectedReports(nodes("n1"), Set()).openOrThrowException("'Test failled'")
      res.size must beEqualTo(2)
    }

    "be correct for in(value('string')) clause" in {
      val res = findExpected.getLastExpectedReports(nodes("n1","n2"), Set()).openOrThrowException("'Test failled'")
      res.size must beEqualTo(2)
    }
  }

  "Finding reports status" should {


    "contains runs for node" in {
      val res = roAgentRun.getNodesLastRun(Set("n0", "n1", "n2", "n3", "n4").map(NodeId(_))).openOrThrowException("test failed")

      res must beEqualTo(agentRuns(
          ("n0" -> None                               )
        , ("n1" -> Some(( run1, None         , true )))
        , ("n2" -> Some(( run1, Some("n2_t1"), true )))
        , ("n3" -> Some(( run2, None         , true )))
        , ("n4" -> Some(( run2, Some("n4_t2"), true )))
      ))

    }



    "find the last reports for node 1" in {
      val r = reportingService.findNodeStatusReports(nodes("n2"), Set("r0"))
      val result = r.openOrThrowException("'Test failled'")
      result must contain(exactly(
          nodeStatus("n2", Some(run1), Some("n2_t1"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v0", SuccessReportType, List("msg")))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v0", SuccessReportType, List("msg")))
              )
          ))
      ))
    }
  }

  ////////// utility methods //////////////

  implicit def nodes(ids:String*):Set[NodeId] = ids.map( NodeId(_) ).toSet
  implicit def configs(ids:(String,String)*): Set[NodeConfigId] = {
    ids.map(id => NodeConfigId(NodeId(id._1), NodeConfigVersion(id._2))).toSet
  }

  implicit def toReport(t:(DateTime,String, String, String, Int, String, String, DateTime, String, String)): Reports = {
    implicit def toRuleId(s:String) = RuleId(s)
    implicit def toDirectiveId(s: String) = DirectiveId(s)
    implicit def toNodeId(s: String) = NodeId(s)

    Reports(t._1, t._2, t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10)
  }

  def compStatus(id: String, values: (String, ReportType, List[String])*): ComponentStatusReport = {
    val v = values.map { case(value, tpe, msgs) => ComponentValueStatusReport(value, Some(value), tpe, msgs) }
    ComponentStatusReport(id, v.toSet, v.flatMap(_.message).toList, Set())
  }

  def nodeStatus(id: String, run:Option[DateTime], version: Option[String], ruleId: String
      , directives: (String, Seq[ComponentStatusReport])*
  ): NodeStatusReport = {
    NodeStatusReport(NodeId(id), run, version.map(NodeConfigVersion(_)), RuleId(ruleId)
        , directives.map(d => DirectiveStatusReport(DirectiveId(d._1), d._2.toSet, Set())).toSet
        , Set()
    )
  }

  def expect(ruleId: String, serial: Int)
            //           nodeJoinKey, directiveId  component   cardinality   componentValues  beging     end            , (nodeId, version)
            (expecteds: (Int        , String     , String    , Int         , String         , DateTime, Option[DateTime], Seq[(String,List[String])])*)
  : Map[SlickExpectedReports, Seq[SlickExpectedReportsNodes]] = {
    expecteds.map { exp =>
      SlickExpectedReports(None, exp._1, ruleId, serial, exp._2, exp._3, exp._4, exp._5, exp._5, exp._6, exp._7) ->
      exp._8.map{ case (nodeId, version) =>
        SlickExpectedReportsNodes(exp._1, nodeId, version)
      }
    }.toMap
  }

  //         nodeId               ruleId  serial dirId   comp     keyVal   execTime   severity  msg
  def node(nodeId:String)(lines: (String, Int,   String, String,  String,  DateTime,  String,   String)*): (String, Seq[Reports]) = {
    (nodeId, lines.map(t => toReport((t._6, t._1, t._3, nodeId, t._2,t._4,t._5,t._6,t._7,t._8))))
  }

  implicit def toAgentIds(ids:Set[(String, DateTime)]): Set[AgentRunId] = {
    ids.map(t => AgentRunId(NodeId(t._1), t._2))
  }

  implicit def toRuleId(id: String): RuleId = RuleId(id)

  implicit def agentRuns(runs:(String, Option[(DateTime, Option[String], Boolean)])*): Map[NodeId, Option[ReportExecution]] = {
    runs.map { case (id, opt) =>
      NodeId(id) -> opt.map(e => ReportExecution(AgentRunId(NodeId(id), e._1), e._2.map(NodeConfigVersion(_)), e._3))
    }.toMap
  }




}
