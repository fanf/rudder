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
import org.specs2.matcher.MatchResult
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.springframework.jdbc.datasource.DataSourceTransactionManager
import net.liftweb.common.Full

import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.reports._
import com.normation.rudder.migration.DBCommon
import com.normation.rudder.reports.ChangesOnly
import com.normation.rudder.reports.execution._
import com.normation.rudder.reports.status.StatusUpdateSquerylRepository
import com.normation.rudder.reports.FullCompliance
import com.normation.rudder.services.reports.ExpectedReportsUpdateImpl
import com.normation.rudder.services.reports.ReportingServiceImpl

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

  val updateRuns = new ReportsExecutionService(reportsRepo, woAgentRun, new StatusUpdateSquerylRepository(squerylConnectionProvider), 1)

  import slick._


  //help differentiate run number with the millis
  //perfect case: generation are followe by runs one minute latter

  val gen1 = DateTime.now.minusMinutes(5*2).withMillisOfSecond(1)
  val run1 = gen1.plusMinutes(1)
  val gen2 = gen1.plusMinutes(5).withMillisOfSecond(2)
  val run2 = gen2.plusMinutes(1)
  //now
//  val gen3 = gen2.plusMinutes(5).withMillisOfSecond(3)
//  val run3 = gen3.minusMinutes(1)

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
        (1, "r0_d0", "r0_d0_c0", 1, """["r0_d0_c0_v0"]""", gen1, Some(gen2), allNodes_t1 )
      , (1, "r0_d0", "r0_d0_c1", 1, """["r0_d0_c1_v0"]""", gen1, Some(gen2), allNodes_t1 )
    )
    ++ expect("r1", 1)( //r1 @ t1
        (2, "r1_d1", "r1_d1_c0", 1, """["r1_d1_c0_v0"]""", gen1, Some(gen2), allNodes_t1 )
    )
    ++ expect("r0", 2)( //r0 @ t2
        (3, "r0_d0", "r0_d0_c0", 1, """["r0_d0_c0_v1"]""", gen2, None, allNodes_t2 )
      , (3, "r0_d0", "r0_d0_c1", 1, """["r0_d0_c1_v1"]""", gen2, None, allNodes_t2 )
    )
    ++ expect("r2", 1)( //r2 @ t2
        (4, "r2_d2", "r2_d2_c0", 1, """["r2_d2_c0_v0"]""", gen2, None, allNodes_t2 )
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

  sequential

  step {
    slick.insertReports(reports.values.toSeq.flatten)
    slickExec { implicit session =>
      expectedReportsTable ++= expecteds.keySet
      expectedReportsNodesTable ++= expecteds.values.toSet.flatten
    }
    updateRuns.findAndSaveExecutions(42)
    updateRuns.findAndSaveExecutions(43) //need to be done one time for init, one time for actual work
  }


  "Testing set-up for expected reports and agent run" should {  //be in ExpectedReportsTest!
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

  }

  /////////////////////////////////////////////////////////////////
  /////////////////////// RuleStatusReports ///////////////////////
  /////////////////////////////////////////////////////////////////

  ///////////////////////////////// error only mode /////////////////////////////////

  "Finding rule status reports for the error only mode" should {
    val errorOnlyReportingService = new ReportingServiceImpl(findExpected, reportsRepo, roAgentRun, () => 5, () => Full(ChangesOnly))

    "get r0" in {
      val r = errorOnlyReportingService.findDirectiveRuleStatusReportsByRule(RuleId("r0"))
      val result = r.openOrThrowException("'Test failled'")

      val expected = Seq(
          nodeStatus("n0", None, Some("n0_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List()))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List()))
              )
          ))
        , nodeStatus("n1", Some(run1), Some("n1_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", PendingReportType, List()))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", PendingReportType, List()))
              )
          ))
        , nodeStatus("n2", Some(run1), Some("n2_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", PendingReportType, List()))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", PendingReportType, List()))
              )
          ))
        , nodeStatus("n3", Some(run2), Some("n3_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List("msg")))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List("msg")))
              )
          ))
        , nodeStatus("n4", Some(run2), Some("n4_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List("msg")))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List("msg")))
              )
          ))
      )


      result must beEqualTo(expected)
    }

    "get r1" in {
      val r = errorOnlyReportingService.findDirectiveRuleStatusReportsByRule(RuleId("r1"))
      val result = r.openOrThrowException("'Test failled'")
      result must beEqualTo(List())
    }

    "get r2" in {
      val r = errorOnlyReportingService.findDirectiveRuleStatusReportsByRule(RuleId("r2"))
      val result = r.openOrThrowException("'Test failled'")

      val expected = Seq(
          nodeStatus("n0", None, Some("n0_t2"), "r2",
              ("r2_d2", Seq(
                  compStatus("r2_d2_c0", ("r2_d2_c0_v0", SuccessReportType, List()))
              )
          ))
        , nodeStatus("n1", Some(run1), Some("n1_t2"), "r2",
              ("r2_d2", Seq(
                  compStatus("r2_d2_c0", ("r2_d2_c0_v0", PendingReportType, List()))
              )
          ))

        , nodeStatus("n4", Some(run2), Some("n4_t2"), "r2",
              ("r2_d2", Seq(
                  compStatus("r2_d2_c0", ("r2_d2_c0_v0", SuccessReportType, List("msg")))
              )
          ))

      )
      result must beEqualTo(expected)
    }
  }

  ///////////////////////////////// full compliance mode /////////////////////////////////

  "Finding rule status reports for the error only mode" should {
    val complianceReportingService = new ReportingServiceImpl(findExpected, reportsRepo, roAgentRun, () => 5, () => Full(FullCompliance))


    "get r0" in {
      val r = complianceReportingService.findDirectiveRuleStatusReportsByRule(RuleId("r0"))
      val result = r.openOrThrowException("'Test failled'")

      val expected = Seq(
          nodeStatus("n0", None, Some("n0_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List()))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List()))
              )
          ))
        , nodeStatus("n1", Some(run1), Some("n1_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", PendingReportType, List()))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", PendingReportType, List()))
              )
          ))
        , nodeStatus("n2", Some(run1), Some("n2_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", PendingReportType, List()))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", PendingReportType, List()))
              )
          ))
        , nodeStatus("n3", Some(run2), Some("n3_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List("msg")))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List("msg")))
              )
          ))
        , nodeStatus("n4", Some(run2), Some("n4_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List("msg")))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List("msg")))
              )
          ))
      )
      result must beEqualTo(expected)
    }

    "get r1" in {
      val r = complianceReportingService.findDirectiveRuleStatusReportsByRule(RuleId("r1"))
      val result = r.openOrThrowException("'Test failled'")
      result must beEqualTo(List())
    }

    "get r2" in {
      val r = complianceReportingService.findDirectiveRuleStatusReportsByRule(RuleId("r2"))
      val result = r.openOrThrowException("'Test failled'")

      val expected = Seq(
          nodeStatus("n0", None, Some("n0_t2"), "r2",
              ("r2_d2", Seq(
                  compStatus("r2_d2_c0", ("r2_d2_c0_v0", SuccessReportType, List()))
              )
          ))
        , nodeStatus("n1", Some(run1), Some("n1_t2"), "r2",
              ("r2_d2", Seq(
                  compStatus("r2_d2_c0", ("r2_d2_c0_v0", PendingReportType, List()))
              )
          ))

        , nodeStatus("n4", Some(run2), Some("n4_t2"), "r2",
              ("r2_d2", Seq(
                  compStatus("r2_d2_c0", ("r2_d2_c0_v0", SuccessReportType, List("msg")))
              )
          ))

      )
      result must beEqualTo(expected)
    }
  }

  /////////////////////////////////////////////////////////////////
  /////////////////////// NodeStatusReports ///////////////////////
  /////////////////////////////////////////////////////////////////

  ///////////////////////////////// error only mode /////////////////////////////////

  "Finding node status reports for the error only mode" should {
    val errorOnlyReportingService = new ReportingServiceImpl(findExpected, reportsRepo, roAgentRun, () => 5, () => Full(ChangesOnly))

    "get success for node 0 on gen2 data (without msg) even if it didn't send reports" in {
      val r = errorOnlyReportingService.findNodeStatusReports(nodes("n0"), Set())
      val result = r.openOrThrowException("'Test failled'")
      compareNodeStatus(result, Seq(
          nodeStatus("n0", None, Some("n0_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List()))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List()))
              )
          ))
        , nodeStatus("n0", None, Some("n0_t2"), "r2",
              ("r2_d2", Seq(
                  compStatus("r2_d2_c0", ("r2_d2_c0_v0", SuccessReportType, List()))
              )
          ))
      ))
    }

    "report success for node 1 on gen2 data, without messages, even if no reports exists for run2" in {
      val r = errorOnlyReportingService.findNodeStatusReports(nodes("n1"), Set("r0"))
      val result = r.openOrThrowException("'Test failled'")
      compareNodeStatus(result, Seq(
          nodeStatus("n1", Some(run1), Some("n1_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List()))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List()))
              )
          ))
      ))
    }

    "find the correct, older that agent frequency, last report based on configuration for node 2" in {
      val r = errorOnlyReportingService.findNodeStatusReports(nodes("n2"), Set("r0", "r1", "r2"))
      val result = r.openOrThrowException("'Test failled'")
      compareNodeStatus(result, Seq(
          nodeStatus("n2", Some(run1), Some("n2_t1"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v0", SuccessReportType, List("msg")))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v0", SuccessReportType, List("msg")))
              )
          ))
        , nodeStatus("n2", Some(run1), Some("n2_t1"), "r1",
              ("r1_d1", Seq(
                  compStatus("r1_d1_c0", ("r1_d1_c0_v0", SuccessReportType, List("msg")))
              )
          ))
      ))
    }

    "find the correct last report based on last expectation for node 3" in {
      val r = errorOnlyReportingService.findNodeStatusReports(nodes("n3"), Set("r0","r1"))
      val result = r.openOrThrowException("'Test failled'")
      compareNodeStatus(result, Seq(
          nodeStatus("n3", Some(run2), Some("n3_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List("msg")))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List("msg")))
              )
          ))
      ))
    }

    "find the correct last report based on configuration for node 4" in {
      val r = errorOnlyReportingService.findNodeStatusReports(nodes("n4"), Set("r0", "r2"))
      val result = r.openOrThrowException("'Test failled'")
      compareNodeStatus(result, Seq(
          nodeStatus("n4", Some(run2), Some("n4_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List("msg")))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List("msg")))
              )
          ))
        , nodeStatus("n4", Some(run2), Some("n4_t2"), "r2",
              ("r2_d2", Seq(
                  compStatus("r2_d2_c0", ("r2_d2_c0_v0", SuccessReportType, List("msg")))
              )
          ))
      ))
    }
  }

  ///////////////////////////////// full compliance mode /////////////////////////////////

  "Finding node status reports for the compliance mode" should {
    val complianceReportingService = new ReportingServiceImpl(findExpected, reportsRepo, roAgentRun, () => 5, () => Full(FullCompliance))

    "get pending for node 0 on gen2 data (without msg)" in {
      val r = complianceReportingService.findNodeStatusReports(nodes("n0"), Set("r0"))
      val result = r.openOrThrowException("'Test failled'")
      compareNodeStatus(result, Seq(
          nodeStatus("n0", None, Some("n0_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", PendingReportType, List()))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", PendingReportType, List()))
              )
          ))
      ))
    }

    "report 'pending' for node 1 even if we can find the correct reports for run1 and they are not expired" in {
      val r = complianceReportingService.findNodeStatusReports(nodes("n1"), Set())
      val result = r.openOrThrowException("'Test failled'")
      compareNodeStatus(result, Seq(
          nodeStatus("n1", Some(run1), Some("n1_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", PendingReportType, List()))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", PendingReportType, List()))
              )
          ))
        , nodeStatus("n1", Some(run1), Some("n1_t2"), "r2",
              ("r2_d2", Seq(
                  compStatus("r2_d2_c0", ("r2_d2_c0_v0", PendingReportType, List()))
              )
          ))
      ))
    }


    /*
     * This case is a little touchy because node2 sent reports for gen1/run1,
     * but not for gen2 (current expectation).
     * But reports from run1 don't have expired (they are not too far from now),
     * they are older than gen-time. And we can compare on nodeconfigversion,
     * and on serial
     *
     * So here, we really expect data from gen2, and we get pending because the
     * expiration time is not spent for now.
     */
    "report 'pending' for node 2 even if we can find the correct reports and they are not expired" in {
      val r = complianceReportingService.findNodeStatusReports(nodes("n2"), Set("r0", "r1"))
      val result = r.openOrThrowException("'Test failled'")
      compareNodeStatus(result, Seq(
          nodeStatus("n2", Some(run1), Some("n2_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", PendingReportType, List()))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", PendingReportType, List()))
              )
          ))
      ))
    }

    "find the correct last report based last expectation for node 3" in {
      val r = complianceReportingService.findNodeStatusReports(nodes("n3"), Set("r0"))
      val result = r.openOrThrowException("'Test failled'")
      compareNodeStatus(result, Seq(
          nodeStatus("n3", Some(run2), Some("n3_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List("msg")))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List("msg")))
              )
          ))
      ))
    }

    "find the correct last report based on configuration for node 4" in {
      val r = complianceReportingService.findNodeStatusReports(nodes("n4"), Set())
      val result = r.openOrThrowException("'Test failled'")
      compareNodeStatus(result, Seq(
          nodeStatus("n4", Some(run2), Some("n4_t2"), "r0",
              ("r0_d0", Seq(
                  compStatus("r0_d0_c0", ("r0_d0_c0_v1", SuccessReportType, List("msg")))
                , compStatus("r0_d0_c1", ("r0_d0_c1_v1", SuccessReportType, List("msg")))
              )
          ))
        , nodeStatus("n4", Some(run2), Some("n4_t2"), "r2",
              ("r2_d2", Seq(
                  compStatus("r2_d2_c0", ("r2_d2_c0_v0", SuccessReportType, List("msg")))
              )
          ))
      ))
    }

    /*
     * TODO: a case for a node where the report sent are from the n-1 expectation
     * BUT they where sent AFTER the n expectation time, so they could be OK.
     * The check must happen on NodeConfigVersion for that case if we don't
     * want to have unexpected reports (what we get if we check on serial).
     * The case can even be set so that the reports look OK (same serial, same values)
     * but in fact, they are from a previous nodeConfigVersion (because one other directive
     * changed).
     */

  }

  ////////// utility methods //////////////

  /*
   * A comparator for NodeStatusReport that allows to more
   * quickly understand what is the problem
   */
  def compareNodeStatus(results:Seq[RuleNodeStatusReport], expecteds:Seq[RuleNodeStatusReport]) = {
    //compare nodestatus for each rules
    val resultByNodeAndRule = results.groupBy(x => (x.nodeId, x.ruleId))
    val expectedByNodeAndRule = expecteds.groupBy(x => (x.nodeId, x.ruleId))

    val compareNodeAndRule = resultByNodeAndRule.keySet must contain(exactly(expectedByNodeAndRule.keySet.toSeq:_*))
    val sameNodeAndRules = resultByNodeAndRule.keySet.intersect(expectedByNodeAndRule.keySet)

    def matchOneNodeStatus(k:(NodeId,RuleId)):MatchResult[Any] = {
      (resultByNodeAndRule(k).toList, expectedByNodeAndRule(k).toList) match {
        case (result :: Nil, expected:: Nil) =>
          val resultDirectives = result.directives
          val expectedDirectives = expected.directives

          val sameDirectives = resultDirectives.keySet.intersect(expectedDirectives.keySet)


          val baseProps = (
                (result.agentRunTime === expected.agentRunTime)
            and (result.configId === expected.configId)
            and (resultDirectives.keySet must contain(exactly(expectedDirectives.keySet.toSeq:_*)))
          )

          def matchOneDirectiveStatus(k:DirectiveId): MatchResult[Any] = {
            resultDirectives(k).components must contain(exactly(expectedDirectives(k).components.toSeq:_*))
          }

          baseProps and (matchOneDirectiveStatus _).foreach(sameDirectives)


        case (x::Nil, y) => ko("Expecting several NodeStatusReports for one node and one rule, which is not supported")
        case (x,_)       => ko("We got several NodeStatusReport for one node and one rule: " + x.toString)
      }
    }

    compareNodeAndRule and (matchOneNodeStatus _).foreach(sameNodeAndRules)

  }

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
    val v = values.map { case(value, tpe, msgs) =>
      val messages = msgs.map(m => MessageStatusReport(tpe, m))
      ComponentValueStatusReport(value, Some(value), messages)
    }
    ComponentStatusReport(id, ComponentValueStatusReport.merge(v))
  }

  def nodeStatus(id: String, run:Option[DateTime], version: Option[String], ruleId: String
      , directives: (String, Seq[ComponentStatusReport])*
  ): RuleNodeStatusReport = {
    RuleNodeStatusReport(
          NodeId(id), RuleId(ruleId), 0, run, version.map(NodeConfigVersion(_))
        , DirectiveStatusReport.merge(directives.map(d =>
            DirectiveStatusReport(DirectiveId(d._1), ComponentStatusReport.merge(d._2))
          ))
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

  implicit def toRuleId(id: String): RuleId = RuleId(id)

  implicit def agentRuns(runs:(String, Option[(DateTime, Option[String], Boolean)])*): Map[NodeId, Option[ReportExecution]] = {
    runs.map { case (id, opt) =>
      NodeId(id) -> opt.map(e => ReportExecution(AgentRunId(NodeId(id), e._1), e._2.map(NodeConfigVersion(_)), e._3))
    }.toMap
  }
}
