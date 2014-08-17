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
import com.normation.rudder.domain.reports.NodeConfigId
import com.normation.rudder.domain.reports.NodeConfigVersion
import com.normation.rudder.domain.reports.NodeConfigVersion
import com.normation.rudder.domain.reports.DirectiveExpectedReports
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.reports.ReportComponent
import com.normation.rudder.domain.reports.RuleExpectedReports
import com.normation.rudder.domain.reports.RuleExpectedReports
import com.normation.rudder.domain.reports.RuleExpectedReports
import com.normation.rudder.domain.reports.DirectivesOnNodes
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

  val expectedReportsRepo = new RuleExpectedReportsJdbcRepository(jdbcTemplate, new DataSourceTransactionManager(dataSource))
  val slick = new ExpectedReportsSchema(dataSource)
  import slick._

  sequential

  implicit def toReport(t:(DateTime,String, String, String, Int, String, String, DateTime, String, String)) = {
    implicit def toRuleId(s:String) = RuleId(s)
    implicit def toDirectiveId(s: String) = DirectiveId(s)
    implicit def toNodeId(s: String) = NodeId(s)

    Reports(t._1, t._2, t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10)
  }

  implicit def toNodeConfigIds(seq:Seq[(String, String)]) = seq.map(x => NodeConfigId(NodeId(x._1), NodeConfigVersion(x._2)))
  implicit def toMapNodeConfig(seq:Seq[(String, String)]) = seq.map(x => (NodeId(x._1), Some(NodeConfigVersion(x._2)))).toMap

  val run1 = DateTime.now.minusMinutes(5*5).withMillisOfSecond(123) //check that millis are actually used
  val run2 = DateTime.now.minusMinutes(5*4)
  val run3 = DateTime.now.minusMinutes(5*3)

  implicit def toSlickExpectedReportsNodes(nodeJoin: Int, nodeId: String, nodeConfigVersions: String): SlickExpectedReportsNodes = {
    SlickExpectedReportsNodes(nodeJoin, nodeId, nodeConfigVersions)
  }


  def compareSlickER(report: SlickExpectedReports, expected: SlickExpectedReports) = {
    report.pkId === expected.pkId and
    report.nodeJoinKey === expected.nodeJoinKey and
    report.serial === expected.serial and
    report.directiveId === expected.directiveId and
    report.component === expected.component and
    report.cardinality === expected.cardinality and
    report.componentsValues === expected.componentsValues and
    report.unexpandedComponentsValues === expected.unexpandedComponentsValues and
    report.endDate === expected.endDate
  }

  def compareER(report: RuleExpectedReports, expected: RuleExpectedReports) = {
    report.ruleId === expected.ruleId and
    report.serial === expected.serial and
    (report.directivesOnNodes must contain(exactly( expected.directivesOnNodes:_*)))

  }

  "Finding nodes" should {

    //note: in version, [a,b,c] means "c" is the most recent versions
    //in the unzserialized object, the most recent version is the HEAD of the list.
    //note: spaces are trimmed in version
    val expectedReportsNodes: Seq[SlickExpectedReportsNodes] = Seq(
        SlickExpectedReportsNodes(1, "n0", "")
      , SlickExpectedReportsNodes(1, "n1", """[" abc" , "def " , "\nghi\t"]""")
      , SlickExpectedReportsNodes(2, "n0", """["cba"]""")
      , SlickExpectedReportsNodes(3, "n2", """["xz"]""")
      , SlickExpectedReportsNodes(4, "n1", """["mno" , "pqr"]""")
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

        res must contain(exactly( ("n0", ""), ("n1", """[" abc" , "def " , "\nghi\t"]""")  ))
      }
    }

    "find the last reports for nodejoinkey" in {
      val result = expectedReportsRepo.getNodes(Set(1)).openOrThrowException("Test failed with exception")
      result.values.toSeq must contain(exactly(
          NodeConfigVersions(NodeId("n0"), List())
          //the order of values is important, as head is most recent
        , NodeConfigVersions(NodeId("n1"), List("ghi", "def", "abc").map(NodeConfigVersion(_)))
      ))
    }

    "correctly sort version for a node and several nodejoinkey" in {
      val result = expectedReportsRepo.getNodes(Set(1,4)).openOrThrowException("Test failed with exception")
      result.values.toSeq must contain(exactly(
          NodeConfigVersions(NodeId("n0"), List())
          //the order of values is important, as head is most recent
        , NodeConfigVersions(NodeId("n1"), List("pqr","mno","ghi", "def", "abc").map(NodeConfigVersion(_)))
      ))
    }
  }


  /*
   * Testing updates
   */
  "Updating from a clean expected reports table" should {
    step {
      cleanTables
      //reset nodeJoinKey sequence to 100
      jdbcTemplate.execute("ALTER SEQUENCE ruleVersionId RESTART WITH 100;")
      jdbcTemplate.execute("ALTER SEQUENCE ruleSerialId RESTART WITH 100;")

    }
    val r1 = RuleId("r1")
    val serial = 42
    val nodeConfigIds = Seq( ("n1", "n1_v1"), ("n2", "n2_v1") )
    val c1 = ReportComponent("c1", 1, Seq("c1_v1"), Seq())
    val d1 = DirectiveExpectedReports(DirectiveId("d1"),Seq(c1))
    val directiveExpectedReports = Seq(d1)

    val expected = SlickExpectedReports(Some(100), 100, r1.value, serial, d1.directiveId.value
      , c1.componentName, c1.cardinality, ComponentsValuesSerialiser.serializeComponents(c1.componentsValues)
      , "[]", DateTime.now, None
    )


    "the first time, just insert" in {
      val inserted = expectedReportsRepo.saveExpectedReports(r1, serial, directiveExpectedReports, nodeConfigIds)

      slickExec { implicit s =>
        val reports =  expectedReportsTable.list
        val nodes = expectedReportsNodesTable.list
        val directiveOnNodes = Seq(DirectivesOnNodes(100, nodeConfigIds, directiveExpectedReports))

        compareER(inserted.openOrThrowException("Test failed"), RuleExpectedReports(r1, serial, directiveOnNodes, DateTime.now, None))
        reports.size === 1 and compareSlickER(reports(0), expected) and
        nodes.size === 2 and (nodes must contain(exactly(
            SlickExpectedReportsNodes(100, "n1", """["n1_v1"]""")
          , SlickExpectedReportsNodes(100, "n2", """["n2_v1"]""")
        )))
      }
    }

    "saving the same exactly, nothing change" in {
      expectedReportsRepo.saveExpectedReports(r1, serial, directiveExpectedReports, nodeConfigIds)

      slickExec { implicit s =>
        val reports =  expectedReportsTable.list
        val nodes = expectedReportsNodesTable.list

        reports.size === 1 and compareSlickER(reports(0), expected) and
        nodes.size === 2 and (nodes must contain(exactly(
            SlickExpectedReportsNodes(100, "n1", """["n1_v1"]""")
          , SlickExpectedReportsNodes(100, "n2", """["n2_v1"]""")
        )))
      }
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
    , endDate                   : Option[DateTime]
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
    def endDate = column[DateTime]("enddate", O.Nullable)

    // Every table needs a * projection with the same type as the table's type parameter
    def * = (
        pkId.?, nodeJoinKey, ruleId, serial, directiveId, component,
        cardinality, componentsValues, unexpandedComponentsValues, beginDate, endDate.?
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
