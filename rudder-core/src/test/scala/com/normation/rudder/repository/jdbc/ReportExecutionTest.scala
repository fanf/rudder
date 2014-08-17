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

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import com.normation.rudder.migration.DBCommon
import scala.io.Source
import com.normation.rudder.reports.execution.RoReportsExecutionSquerylRepository
import com.normation.rudder.reports.execution.WoReportsExecutionSquerylRepository
import com.normation.rudder.reports.execution.ReportExecution
import com.normation.inventory.domain.NodeId
import org.joda.time.DateTime
import net.liftweb.common.Full
import org.springframework.jdbc.core.ResultSetExtractor
import org.springframework.jdbc.core.RowMapper
import org.springframework.jdbc.core.RowCallbackHandler
import java.sql.ResultSet
import org.specs2.matcher.BeEqualTo
import org.specs2.specification.BeforeExample
import com.normation.rudder.reports.execution.AgentRunId
import com.normation.rudder.domain.reports.NodeConfigVersion


/**
 *
 * Test on database.
 *
 */
@RunWith(classOf[JUnitRunner])
class ReportExecutionsTest extends DBCommon {

  System.setProperty("test.postgres", "true")

  //clean data base
  def cleanTables() = {
    jdbcTemplate.execute("DELETE FROM ReportsExecution;")
  }


  val roRunRepo = new RoReportsExecutionSquerylRepository(squerylConnectionProvider)
  val woRunRepo = new WoReportsExecutionSquerylRepository(roRunRepo)


  val (n1, n2) = (NodeId("n1"), NodeId("n2"))
  val runMinus2 = DateTime.now.minusMinutes(7)
  val runMinus1 = DateTime.now.minusMinutes(2)

  sequential


  "Execution repo" should {
    val runs = Seq(
        ReportExecution(AgentRunId(n1, runMinus2), Some(NodeConfigVersion("nodeConfig_n1_v1")), true)
      , ReportExecution(AgentRunId(n1, runMinus1), Some(NodeConfigVersion("nodeConfig_n1_v1")), false)
    )

    "correctly insert" in {
      woRunRepo.updateExecutions(Seq(runs(0))) must beEqualTo( Full(Seq(runs(0)) ))
    }

    "correctly find back" in {
      roRunRepo.getNodeLastExecution(n1) must beEqualTo(Full(Some(runs(0))))
    }

    "correctly update last" in {
      (woRunRepo.updateExecutions(runs) must beEqualTo( Full(Seq(runs(1))) )) and
      (roRunRepo.getNodeLastExecution(n1) must beEqualTo(Full(Some(runs(1)))))
    }

    "don't find report when none was added" in {
      roRunRepo.getNodeLastExecution(n2) must beEqualTo(Full(None))
    }
  }


  val initRuns = Seq(
      ReportExecution(AgentRunId(n1, runMinus2.minusMinutes(5)), Some(NodeConfigVersion("nodeConfig_n1_v1")), true)
    , ReportExecution(AgentRunId(n1, runMinus2), Some(NodeConfigVersion("nodeConfig_n1_v1")), true)
    , ReportExecution(AgentRunId(n1, runMinus1), Some(NodeConfigVersion("nodeConfig_n1_v1")), false)
  )

  /*
   * BE VERY CAREFULL: code just above the "should" is not
   * executed when one's thinking. So if you have logic
   * to exec before fragment, it MUST go in a step...
   */
  step {
    cleanTables
    woRunRepo.updateExecutions(initRuns)
  }

  "Updating execution" should {

    "correctly close and let closed existing execution" in {
      val all = Seq(
          initRuns(0).copy(isCompleted = false) //not updated
        , initRuns(1).copy(nodeConfigVersion = Some(NodeConfigVersion("nodeConfig_n1_v2")))
        , initRuns(2).copy(isCompleted = true)
        , ReportExecution(AgentRunId(n1, runMinus2.minusMinutes(10)), Some(NodeConfigVersion("nodeConfig_n1_v1")), true)
        , ReportExecution(AgentRunId(n1, runMinus1.plusMinutes(5)), Some(NodeConfigVersion("nodeConfig_n1_v1")), false)
      )

      //only the first one should not be modified
      woRunRepo.updateExecutions(all).openOrThrowException("Failed test") must contain(exactly(all.tail:_*))

    }
  }

}