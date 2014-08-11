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

import java.sql.Timestamp
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import scala.slick.driver.PostgresDriver.simple._
import org.joda.time.DateTime
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.reports.Reports
import com.normation.rudder.migration.DBCommon
import com.normation.rudder.reports.execution.AgentRunId
import javax.sql.DataSource
import net.liftweb.common.Loggable
import java.sql.BatchUpdateException
import com.normation.rudder.reports.execution.AgentRunId
import com.normation.rudder.reports.execution.AgentRunId

/**
 *
 * Test on database.
 *
 */
@RunWith(classOf[JUnitRunner])
class ReportsTest extends DBCommon {
System.setProperty("test.postgres", "true")


  //clean data base
  def cleanTables() = {
    jdbcTemplate.execute("DELETE FROM ReportsExecution;")
  }


  val repostsRepo = new ReportsJdbcRepository(jdbcTemplate)
  val slick = new ReportsSlick(dataSource)

  sequential

  implicit def toReport(t:(DateTime,String, String, String, Int, String, String, DateTime, String, String)) = {
    implicit def toRuleId(s:String) = RuleId(s)
    implicit def toDirectiveId(s: String) = DirectiveId(s)
    implicit def toNodeId(s: String) = NodeId(s)

    Reports(t._1, t._2, t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10)
  }

  def node(nodeId:String)(lines: (String, String, Int, String, String, DateTime, String, String)*): (String, Seq[Reports]) = {
    (nodeId, lines.map(t => toReport((t._6, t._1, t._2, nodeId, t._3,t._4,t._5,t._6,t._7,t._8))))
  }

  val run1 = DateTime.now.minusMinutes(5*5).withMillisOfSecond(123) //check that millis are actually used
  val run2 = DateTime.now.minusMinutes(5*4)
  val reports = (
    Map[String, Seq[Reports]]() +
    node("n0")(
        ("r0", "d1", 1, "c1", "cv1", run1, "result_success", "End execution")
    ) +
    node("n1")(
        ("r0", "d1", 1, "c1", "cv1", run1, "result_success", "End execution")
      , ("r1", "d1", 1, "c1", "cv1", run1, "result_success", "msg1")
      , ("r1", "d1", 1, "c2", "cv2", run1, "result_success", "msg1")
      //haha! run2!
      , ("r1", "d1", 1, "c2", "cv2", run2, "result_success", "msg1")
    ) +
    node("n2")(
        ("r0", "d1", 1, "c1", "cv1", run1, "result_success", "End execution")
      , ("r1", "d1", 1, "c1", "cv1", run1, "result_success", "msg1")
      , ("r1", "d1", 1, "c2", "cv2", run1, "result_success", "msg1")
    )
  )


  step {
    slick.insertReports(reports.values.toSeq.flatten)
  }

  implicit def toAgentIds(ids:Set[(String, DateTime)]):Set[AgentRunId] = {
    ids.map(t => AgentRunId(NodeId(t._1), t._2))
  }

  "Execution repo" should {

    "find the last reports for node0" in {
      val result = repostsRepo.findReportByAgentExecution(Set(AgentRunId(NodeId("n0"), run1))).openOrThrowException("Test failed with exception")
      result must contain(exactly(reports("n0")(0)))
    }

    "find reports for node 0,1,2" in {
      val runs = Set(("n0", run1), ("n1", run1), ("n2", run1) )
      val result = repostsRepo.findReportByAgentExecution(runs).openOrThrowException("Test failed with exception")
      result must contain(exactly(reports("n0")++reports("n1").reverse.tail++reports("n2"):_*))
    }

    "not find report for none existing agent run id" in {
      val runs = Set( ("n2", run2), ("n3", run1))
      val result = repostsRepo.findReportByAgentExecution(runs).openOrThrowException("Test failed with exception")
      result must beEmpty
    }
  }

}

class ReportsSlick(datasource: DataSource) extends Loggable {

  final case class SlickReports(
      id                 : Option[Long]
    , executionDate      : DateTime
    , nodeId             : String
    , directiveId        : String
    , ruleId             : String
    , serial             : Int
    , component          : String
    , keyValue           : String
    , executionTimestamp : DateTime
    , eventType          : String
    , policy             : String
    , msg                : String
  )

  implicit def date2dateTime = MappedColumnType.base[DateTime, Timestamp](
      dt => new Timestamp(dt.getMillis)
    , ts => new DateTime(ts.getTime)
  )

  class ReportsTable(tag: Tag) extends Table[SlickReports](tag, "ruddersysevents") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc) // This is the primary key column
    def executionDate = column[DateTime]("executiondate")
    def nodeId = column[String]("nodeid")
    def directiveId = column[String]("directiveid")
    def ruleId = column[String]("ruleid")
    def serial = column[Int]("serial")
    def component = column[String]("component")
    def keyValue = column[String]("keyvalue")
    def executionTimeStamp = column[DateTime]("executiontimestamp")
    def eventType = column[String]("eventtype")
    def policy = column[String]("policy")
    def msg = column[String]("msg")

    // Every table needs a * projection with the same type as the table's type parameter
    def * = (
        id.?, executionDate, nodeId, directiveId, ruleId, serial,
        component, keyValue, executionTimeStamp, eventType, policy, msg
    ) <> (SlickReports.tupled, SlickReports.unapply)
  }

  val reportsQuery = TableQuery[ReportsTable]

  val slickDB = Database.forDataSource(datasource)

  def toSlickReport(r:Reports): SlickReports = {
    SlickReports(None, r.executionDate, r.nodeId.value, r.directiveId.value, r.ruleId.value, r.serial
        , r.component, r.keyValue, r.executionTimestamp, r.severity, "policy", r.message)
  }

  def insertReports(reports: Seq[Reports]) = {
    val slickReports = reports.map(toSlickReport(_))

    try {
      slickDB.withSession { implicit s =>
        reportsQuery ++= slickReports
      }
    } catch {
      case e: BatchUpdateException =>
        logger.error("Error when inserting reports: " + e.getMessage)
        logger.error(e.getNextException)
        throw e
    }
  }
}
