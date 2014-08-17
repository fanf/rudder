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
import com.normation.rudder.reports.execution.ReportExecution
import com.normation.rudder.reports.execution.AgentRunId
import com.normation.rudder.domain.reports.RuleExpectedReports
import org.springframework.jdbc.datasource.DataSourceTransactionManager
import com.normation.rudder.domain.reports.NodeConfigVersions
import com.normation.rudder.domain.reports.NodeConfigVersion

/**
 *
 * Test on database.
 *
 */
@RunWith(classOf[JUnitRunner])
class ExpectedReportsTest extends DBCommon {
System.setProperty("test.postgres", "true")


  //clean data base
  def cleanTables() = {
    jdbcTemplate.execute("DELETE FROM expectedReports; DELETE FROM expectedReportsNodes;")
  }

  val expectedRepostsRepo = new RuleExpectedReportsJdbcRepository(jdbcTemplate, new DataSourceTransactionManager(dataSource))
  val slick = new ExpectedReportsSchema(dataSource)
  import slick._

  sequential

  implicit def toReport(t:(DateTime,String, String, String, Int, String, String, DateTime, String, String)) = {
    implicit def toRuleId(s:String) = RuleId(s)
    implicit def toDirectiveId(s: String) = DirectiveId(s)
    implicit def toNodeId(s: String) = NodeId(s)

    Reports(t._1, t._2, t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10)
  }

  val run1 = DateTime.now.minusMinutes(5*5).withMillisOfSecond(123) //check that millis are actually used
  val run2 = DateTime.now.minusMinutes(5*4)
  val run3 = DateTime.now.minusMinutes(5*3)

  implicit def toSlickExpectedReportsNodes(nodeJoin: Int, nodeId: String, nodeConfigVersions: String): SlickExpectedReportsNodes = {
    SlickExpectedReportsNodes(nodeJoin, nodeId, nodeConfigVersions)
  }

  "Finding nodes" should {
    val expectedReportsNodes: Seq[SlickExpectedReportsNodes] = Seq(
        SlickExpectedReportsNodes(1, "n0", "")
      , SlickExpectedReportsNodes(1, "n1", "abc , def , ghi")
      , SlickExpectedReportsNodes(2, "n0", "cba")
      , SlickExpectedReportsNodes(3, "n2", "xz")
    )
    step {
      slickExec { implicit s =>
        expectedReportsNodesTable ++= expectedReportsNodes
      }
    }

    "get back what was inserted" in {
      slickExec { implicit s =>
        expectedReportsNodesTable.list must contain(exactly(expectedReportsNodes:_*))
      }
    }

    "get in the same way" in {
      import scala.slick.driver.JdbcDriver.backend.Database
      import scala.slick.jdbc.{GetResult, StaticQuery => Q}
      import Q.interpolation
      slickExec { implicit s =>
        val i = 1
        val res = sql"select nodeid, nodeconfigversions from expectedreportsnodes where nodeJoinKey = ${i}".as[(String, String)].list

        res must contain(exactly( ("n0", ""), ("n1", "abc , def , ghi")  ))
      }
    }

    "find the last reports for node0" in {
      val result = expectedRepostsRepo.getNodes(Set(1)).openOrThrowException("Test failed with exception")
      result.values.toSeq must contain(exactly(
          NodeConfigVersions(NodeId("n0"), List())
        , NodeConfigVersions(NodeId("n1"), List("abc", "def", "ghi").map(NodeConfigVersion(_)))
      ))
    }
  }

}

class ExpectedReportsSchema(datasource: DataSource) extends Loggable {

  final case class SlickExpectedReports(
      pkId                      : Option[Int]
    , nodeJoinKey               : Int
    , ruleId                    : String
    , serial                    : Int
    , directiveId               : String
    , component                 : String
    , cardinality               : Int
    , componentsValues          : String
    , unexpandedComponentsValues: String
    , beginDate                 : DateTime
    , endDate                   : DateTime
  )

  final case class SlickExpectedReportsNodes(
      nodeJoinKey        : Int
    , nodeId             : String
    , nodeConfigVersions : String
  )

  implicit def date2dateTime = MappedColumnType.base[DateTime, Timestamp](
      dt => new Timestamp(dt.getMillis)
    , ts => new DateTime(ts.getTime)
  )

  class ExpectedReportsTable(tag: Tag) extends Table[SlickExpectedReports](tag, "expectedreports") {
    def pkId = column[Int]("pkid", O.PrimaryKey, O.AutoInc) // This is the primary key column
    def nodeJoinKey = column[Int]("nodejoinkey")
    def ruleId = column[String]("ruleid")
    def serial = column[Int]("serial")
    def directiveId = column[String]("directiveid")
    def component = column[String]("component")
    def cardinality = column[Int]("cardinality")
    def componentsValues = column[String]("componentsvalues")
    def unexpandedComponentsValues = column[String]("unexpandedcomponentsvalues")
    def beginDate = column[DateTime]("begindate")
    def endDate = column[DateTime]("enddate")

    // Every table needs a * projection with the same type as the table's type parameter
    def * = (
        pkId.?, nodeJoinKey, ruleId, serial, directiveId, component,
        cardinality, componentsValues, unexpandedComponentsValues, beginDate, endDate
    ) <> (SlickExpectedReports.tupled, SlickExpectedReports.unapply)
  }

  class ExpectedReportsNodesTable(tag: Tag) extends Table[SlickExpectedReportsNodes](tag, "expectedreportsnodes") {
    def nodeJoinKey = column[Int]("nodejoinkey")
    def nodeId = column[String]("nodeid")
    def nodeConfigVersions = column[String]("nodeconfigversions")

    // Every table needs a * projection with the same type as the table's type parameter
    def * = (
        nodeJoinKey, nodeId, nodeConfigVersions
        ) <> (SlickExpectedReportsNodes.tupled, SlickExpectedReportsNodes.unapply)
    def pk = primaryKey("pk_expectedreportsnodes", (nodeJoinKey, nodeId))
  }

  val expectedReportsTable = TableQuery[ExpectedReportsTable]
  val expectedReportsNodesTable = TableQuery[ExpectedReportsNodesTable]

  val slickDB = Database.forDataSource(datasource)

  def slickExec[A](body: Session => A): A = {
    try {
      slickDB.withSession { s => body(s) }
    } catch {
      case e: BatchUpdateException =>
        logger.error("Error when inserting reports: " + e.getMessage)
        logger.error(e.getNextException)
        throw e
    }
  }
}
