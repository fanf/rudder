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

import com.normation.rudder.domain.policies.DirectiveId
import com.normation.inventory.domain.NodeId
import net.liftweb.common._
import com.normation.rudder.domain.policies._
import com.normation.rudder.repository.RuleExpectedReportsRepository
import com.normation.rudder.domain.reports.RuleExpectedReports
import com.normation.rudder.domain.reports._
import org.joda.time._
import org.slf4j.{Logger,LoggerFactory}
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.core._
import java.sql.ResultSet
import java.sql.Timestamp
import scala.collection.JavaConversions._
import net.liftweb.json._
import com.normation.utils.HashcodeCaching
import net.liftweb.util.Helpers.tryo
import com.normation.utils.Control._
import org.springframework.transaction.TransactionStatus
import org.springframework.transaction.support.TransactionCallback
import org.springframework.transaction.PlatformTransactionManager
import com.normation.rudder.reports.execution.AgentRunId
import com.normation.rudder.reports.execution.AgentRunId

class RuleExpectedReportsJdbcRepository(
    jdbcTemplate      : JdbcTemplate
  , transactionManager: PlatformTransactionManager
  ) extends RuleExpectedReportsRepository {

  val logger = LoggerFactory.getLogger(classOf[RuleExpectedReportsJdbcRepository])

  /**
   * We need to create transaction for the insertion of expected reports
   * otherwise race conditions may occur
   * We are clearly pushing the complexity of jdbcTemplate, and will need to move to
   * higher lever abstraction for this
   */
  val transactionTemplate = new org.springframework.transaction.support.TransactionTemplate(transactionManager)

  val baseQuery = "select pkid, nodejoinkey, ruleid, serial, directiveid, component, cardinality, componentsvalues, unexpandedComponentsValues, begindate, enddate from expectedreports where 1=1 ";


  override def findExpectedReportsByNodeConfigId(nodeConfigId: NodeConfigurationId): Box[Seq[RuleExpectedReports]] = {
    /*
     * Select agentRun join expectedReportsNodes join expectedreports
     */

    val query = """select pkid, nodejoinkey, ruleid, serial, directiveid, component, cardinality, componentsvalues, unexpandedComponentsValues, begindate, enddate
      from expectedreports as A
      join (
        select nodejoinkey from expectedreportsnodes as B
        where B.nodeId = ? and B.nodeconfigversions like ?
      ) as J on A.nodejoinkey = J.nodejoinkey;"""

    try {
      toRuleExpectedReports(jdbcTemplate.query(query, Array[AnyRef](nodeConfigId.nodeId.value, "%"+nodeConfigId.version+"%"), RuleExpectedReportsMapper).toSeq)
    } catch {
      case e:Exception => Failure(s"Error when getting expected report for node '${nodeConfigId.nodeId.value}' and configuration version '${nodeConfigId.version}'", Full(e), Empty)
    }
  }

  override def findAllCurrentExpectedReportsWithNodesAndSerial(): Map[RuleId, (Int, Set[NodeConfigurationId])] = {
    val composite = jdbcTemplate.query("select distinct ruleid, serial, nodejoinkey from expectedreports where enddate is null", RuleIdSerialNodeJoinKeyMapper)

    (for {
      (ruleId, serial, nodeJoin) <- composite
      nodeList <- getNodes(nodeJoin)
    } yield {
      //only return the last node configuration for each node id
      val nodeConfigIds = nodeList.map{case (nodeId, versions) => NodeConfigurationId(nodeId, versions.reverse.headOption.getOrElse("")) }

      (ruleId, (serial, nodeConfigIds.toSet))
    }).toMap

  }

  /**
   * Return current expectedreports (the one still pending) for this Rule
   * @param rule
   * @return
   */
  override def findCurrentExpectedReports(ruleId : RuleId) : Box[Option[RuleExpectedReports]] = {
    try {
      toRuleExpectedReport(jdbcTemplate.query(baseQuery + " and enddate is null and ruleid = ?", Array[AnyRef](ruleId.value), RuleExpectedReportsMapper).toSeq)
    } catch {
      case e:Exception => Failure("Error when getting expected report for rule " + ruleId.value, Full(e), Empty)
    }
  }
  private[this] def findCurrentExpectedReportsForRules(ruleIds : Set[RuleId]) : Box[Seq[RuleExpectedReports]] = {
    val in = ruleIds.map( _.value ).mkString(",")
    try {
      toRuleExpectedReports(jdbcTemplate.query(baseQuery + " and enddate is null and ruleid in (?)", Array[AnyRef](in), RuleExpectedReportsMapper).toSeq)
    } catch {
      case e:Exception => Failure("Error when getting expected report for rules " + in, Full(e), Empty)
    }
  }

  /**
   * Simply set the endDate for the expected report for this conf rule
   * @param ruleId
   */
  override def closeExpectedReport(ruleId : RuleId) : Box[Unit] = {
    logger.debug(s"Closing expected report for rules '${ruleId.value}'")
    findCurrentExpectedReports(ruleId) match {
      case e:EmptyBox => e
      case Full(None) =>
            logger.warn(s"Cannot close a non existing entry '${ruleId.value}'")
            Full(Unit)
      case Full(Some(entry)) =>
        jdbcTemplate.update("update expectedreports  set enddate = ? where serial = ? and ruleId = ?",
          new Timestamp(DateTime.now().getMillis), new java.lang.Integer(entry.serial), entry.ruleId.value
        )
        Full(Unit) // unit is expected
    }
  }


  /**
   * This utilitary class is used only to compare what is already saved in the
   * DB and compare it with what is to be saved
   */
  private[this] final case class Comparator(
      nodeConfigId : NodeConfigurationId
    , directiveId  : DirectiveId
    , componentName: String
  )

  /**
   * TODO: change this API !
   * Save an expected reports.
   *
   */
  override def saveExpectedReports(
      ruleId                  : RuleId
    , serial                  : Int
    , directiveExpectedReports: Seq[DirectiveExpectedReports]
    , nodeConfigIds           : Seq[NodeConfigurationId]
  ) : Box[RuleExpectedReports] = {
     logger.debug("Saving expected report for rule {}", ruleId.value)
// TODO : store also the unexpanded
     findCurrentExpectedReports(ruleId) match {
       case e: EmptyBox => e
       case Full(Some(x)) =>
         // I need to check I'm not having duplicates
         // easiest way : unfold all, and check intersect
         val toInsert = directiveExpectedReports.flatMap { case DirectiveExpectedReports(dir, comp) =>
           comp.map(x => (dir, x.componentName))
         }.flatMap { case (dir, compName) =>
           nodeConfigIds.map(id => Comparator(id, dir, compName))
         }

         val comparator = x.directivesOnNodes.flatMap { case DirectivesOnNodes(_, configs, dirExp) =>
           dirExp.flatMap { case DirectiveExpectedReports(dir, comp) =>
             comp.map(x => (dir, x.componentName))
           }.flatMap { case (dir, compName) =>
             configs.map(id => Comparator(id, dir, compName))
           }
         }

         toInsert.intersect(comparator) match {
           case seq if seq.size > 0 =>
             val msg = s"Inconsistency in the database : cannot save an already existing expected report for rule '${ruleId.value}'"
             logger.error(msg)
             logger.debug("Intersecting values are " + seq)
             Failure(msg)

           case _ => // Ok
             createExpectedReports(ruleId, serial, directiveExpectedReports, nodeConfigIds)
         }

      case Full(None) =>
          createExpectedReports(ruleId, serial, directiveExpectedReports, nodeConfigIds)
     }
  }

  private[this] def createExpectedReports(
      ruleId                  : RuleId
    , serial                  : Int
    , directiveExpectedReports: Seq[DirectiveExpectedReports]
    , nodeConfigIds           : Seq[NodeConfigurationId]
  ) : Box[RuleExpectedReports] = {

    transactionTemplate.execute(new TransactionCallback[Box[RuleExpectedReports]]() {
      def doInTransaction(status: TransactionStatus): Box[RuleExpectedReports] = {
        // Compute first the version id
        val nodeJoinKey = getNextVersionId

        // Create the lines for the mapping
        val list = for {
                policy <- directiveExpectedReports
                component <- policy.components
        } yield {
                new ExpectedConfRuleMapping(0, nodeJoinKey, ruleId, serial,
                      policy.directiveId, component.componentName, component.cardinality, component.componentsValues, component.unexpandedComponentsValues, DateTime.now(), None)
        }
        list.foreach(entry =>
          jdbcTemplate.update("""insert into expectedreports (
              nodejoinkey, ruleid, serial, directiveid, component, cardinality, componentsValues, unexpandedComponentsValues, begindate
            ) values (?,?,?,?,?,?,?,?,?)"""
          , new java.lang.Integer(entry.nodeJoinKey), ruleId.value, new java.lang.Integer(entry.serial), entry.policyExpectedReport.value
          , entry.component,  new java.lang.Integer(entry.cardinality), ComponentsValuesSerialiser.serializeComponents(entry.componentsValues)
          , ComponentsValuesSerialiser.serializeComponents(entry.unexpandedComponentsValues), new Timestamp(entry.beginDate.getMillis)
          )
        )

        saveNode(nodeConfigIds, nodeJoinKey )

        findCurrentExpectedReports(ruleId) match {
          case Full(Some(x)) => Full(x)
          case Full(None) => Failure("Could not fetch the freshly saved expected report for rule %s".format(ruleId.value))
          case e:EmptyBox => e
        }
      }
    })

  }


  /**
   * Return all the expected reports between the two dates
   * @return
   */
  override def findExpectedReports(beginDate : DateTime, endDate : DateTime) : Box[Seq[RuleExpectedReports]] = {
    var query = baseQuery + " and beginDate < ? and coalesce(endDate, ?) >= ? "
    var array = scala.collection.mutable.Buffer[AnyRef](new Timestamp(endDate.getMillis), new Timestamp(beginDate.getMillis), new Timestamp(beginDate.getMillis))

    toRuleExpectedReports(jdbcTemplate.query(query,
          array.toArray[AnyRef],
          RuleExpectedReportsMapper).toSeq)
  }

  def updateNodeConfigVersion(toUpdate: Map[RuleId, Map[NodeId, Seq[String]]]): Box[Map[RuleId, Map[NodeId, Seq[String]]]] = {
    for {
      //get back the existing (nodeJoinKey, nodeId, configVersions) for rules
      expectedReports    <- findCurrentExpectedReportsForRules(toUpdate.keySet)
      nodeJoinKeyByRules =  expectedReports.flatMap(report => report.directivesOnNodes.map( x => (report.ruleId, x.nodeJoinKey))).toMap
      existing           <- sequence(nodeJoinKeyByRules.toSeq) { k => getNodes(k._2).map( ( k, _) ) }
      currentNodeConfig  =  existing.groupBy { case ((ruleId, k), _) => ruleId }
      updatedVersions    =  toUpdate.flatMap { case (ruleId, byNodeVersions) =>
                              currentNodeConfig.getOrElse(ruleId, Seq()).flatMap { case ( (_, nodeJoin), nodes) =>
                                byNodeVersions.map { case (nodeId, versions) =>
                                  (nodeJoin, nodeId, versions ++ nodes.getOrElse(nodeId, Seq()))
                                }

                              }
                            }
      saved              <- updateNodes(updatedVersions.toSeq)
    } yield {
      toUpdate
    }
  }

  /**
   * Save the server list in the database
   */
  private[this] def updateNodes(configs: Seq[(Int,NodeId,Seq[String])]): Box[Seq[(Int,NodeId,Seq[String])]] = {
    tryo {
      for ((nodeJoinKey, nodeId, versions) <- configs) {
        val versionsString = versions.map(_.trim).mkString(",")
        jdbcTemplate.update(
            "update expectedreportsnodes set nodeconfigversions = ? where nodejoinkey = ? and nodeid = ?"
          ,  versionsString, new java.lang.Integer(nodeJoinKey), nodeId.value
        )
      }
      configs
    }
  }

  /**
   * Return currents expectedreports (the one still pending) for this server
   * @param directiveId
   * @return
   */
  override def findCurrentExpectedReportsByNode(nodeId : NodeId) : Box[Seq[RuleExpectedReports]] = {
    val joinQuery =
      s"""select pkid, expectedreports.nodejoinkey, ruleid,directiveid, serial, component, componentsvalues, unexpandedComponentsValues, cardinality, begindate, enddate
          from expectedreports
          join expectedreportsnodes on expectedreportsnodes.nodejoinkey = expectedreports.nodejoinkey
          where enddate is null and  expectedreportsnodes.nodeId = ?"""

    toRuleExpectedReports(jdbcTemplate.query(joinQuery,
          Array[AnyRef](nodeId.value),
          RuleExpectedReportsMapper).toSeq)

  }

  private[jdbc] def getNodes(nodeJoinKey : Int) : Box[Map[NodeId, Seq[String]]] = {
    tryo {
      jdbcTemplate.query("select nodeid, nodeconfigversions from expectedreportsnodes where nodeJoinKey = ?",
        Array[AnyRef](new java.lang.Integer(nodeJoinKey)),
        NodeJoinKeyConfigMapper).toMap
    }
  }

  private[this] def getNodesLastVersion(nodeJoinKey : Int) : Box[Seq[NodeConfigurationId]] = {
    getNodes(nodeJoinKey).map { m =>
      m.map {case (nodeId, versions) => NodeConfigurationId(nodeId, versions.reverse.headOption.getOrElse(""))}.toSeq
    }
  }

  /**
   * Save the server list in the database
   */
  private def saveNode(nodeConfigurationIds : Seq[NodeConfigurationId], nodeJoinKey : Int) = {
    for (config <- nodeConfigurationIds) {
      jdbcTemplate.update("insert into expectedreportsnodes ( nodejoinkey, nodeid, nodeconfigversions) values (?,?,?)",
        new java.lang.Integer(nodeJoinKey), config.nodeId, config.version
      )
    }
  }

  private def getNextVersionId() : Int = {
    jdbcTemplate.queryForInt("SELECT nextval('ruleVersionId')")
  }


  /**
   * Effectively convert lines from the DB to RuleExpectedReports (and does also fill the nodes, opposite to what
   * was previously done)
   */
  private[this] def toRuleExpectedReports(entries : Seq[ExpectedConfRuleMapping]) : Box[Seq[RuleExpectedReports]] = {
    // first, we fetch all the nodes, so that it's done once and for all
    val nodes = entries.map(_.nodeJoinKey).distinct.map { nodeJoinKey => (nodeJoinKey -> getNodesLastVersion(nodeJoinKey)) }.toMap

    nodes.values.filter (x => !x.isDefined).headOption match {
      case Some(e:Failure) => Failure("Some nodes could not be fetched for expected reports, cause " + e.messageChain)
      case Some(_) => Failure("Some nodes could not be fetched for expected reports")
      case _ => // we don't have illegal values, we will be able to open the box later
        // group entries by Rule/serial
        Full(entries.groupBy( entry=> SerialedRuleId(entry.ruleId, entry.serial)).map { case (key, seq) =>
          // now we need to group elements of the seq together,  based on nodeJoinKey
          val directivesOnNode = seq.groupBy(x => x.nodeJoinKey).map { case (nodeJoinKey, mappedEntries) =>
            // need to convert to group everything by directiveId, the convert to DirectiveExpectedReports
            val directiveExpectedReports = mappedEntries.groupBy(x=>x.policyExpectedReport).map { case (directiveId, lines) =>
              // here I am on the directiveId level, all lines that have the same RuleId, Serial, NodeJoinKey, DirectiveId are
              // for the same directive, and must be put together
              DirectiveExpectedReports(directiveId, lines.map( x => ReportComponent(x.component, x.cardinality, x.componentsValues, x.unexpandedComponentsValues)))
            }
            // I can open the box, for it is checked earlier that it is safe
            DirectivesOnNodes(nodeJoinKey, nodes(nodeJoinKey).openTheBox, directiveExpectedReports.toSeq)
          }
          RuleExpectedReports(
              key.ruleId
            , key.serial
            , directivesOnNode.toSeq
            , seq.head.beginDate
            , seq.head.endDate
          )
        }.toSeq)
    }
  }

  private[this] def toRuleExpectedReport(entries : Seq[ExpectedConfRuleMapping]) : Box[Option[RuleExpectedReports]] = {
      toRuleExpectedReports(entries)  match {
        case Empty => Empty
        case f:Failure =>
          logger.error(s"Error when getting expected report: ${f.messageChain}")
          f
        case Full(seq) =>
          seq.size match {
            case 0 => Full(None)
            case 1 => Full(Some(seq.head))
            case _ => Failure(s"Inconsistency in the database, too many expected reports for a rule")
          }
      }
  }
}

object RuleExpectedReportsMapper extends RowMapper[ExpectedConfRuleMapping] {
  def mapRow(rs : ResultSet, rowNum: Int) : ExpectedConfRuleMapping = {
    // unexpandedcomponentsvalues may be null, as it was not defined before 2.6
    val unexpandedcomponentsvalues = rs.getString("unexpandedcomponentsvalues") match {
      case null => ""
      case value => value
    }
    new ExpectedConfRuleMapping(
      rs.getInt("pkid"),
      rs.getInt("nodejoinkey"),
      new RuleId(rs.getString("ruleid")),
      rs.getInt("serial"),
      DirectiveId(rs.getString("directiveid")),
      rs.getString("component"),
      rs.getInt("cardinality"),
      ComponentsValuesSerialiser.unserializeComponents(rs.getString("componentsvalues")),
      ComponentsValuesSerialiser.unserializeComponents(unexpandedcomponentsvalues),
      new DateTime(rs.getTimestamp("begindate")),
      if(rs.getTimestamp("enddate")!=null) {
        Some(new DateTime(rs.getTimestamp("endDate")))
      } else None
    )
  }
}

object RuleIdMapper extends RowMapper[RuleId] {
  def mapRow(rs : ResultSet, rowNum: Int) : RuleId = {
    new RuleId(rs.getString("ruleid"))
  }
}
object RuleIdAndSerialMapper extends RowMapper[(RuleId, Int)] {
  def mapRow(rs : ResultSet, rowNum: Int) : (RuleId, Int) = {
    (new RuleId(rs.getString("ruleid")) , rs.getInt("serial"))
  }
}

object RuleIdSerialNodeJoinKeyMapper extends RowMapper[(RuleId, Int, Int)] {
  def mapRow(rs : ResultSet, rowNum: Int) : (RuleId, Int, Int) = {
    (new RuleId(rs.getString("ruleid")) , rs.getInt("serial"), rs.getInt("nodejoinkey"))
  }
}

object NodeJoinKeyConfigMapper extends RowMapper[(NodeId, Seq[String])] {
  def mapRow(rs : ResultSet, rowNum: Int) : (NodeId, Seq[String]) = {
    (
        new NodeId(rs.getString("nodeid"))
      , rs.getString("nodeconfigversions") match {
          case null | "" => Seq()
          case x => x.split(",").map(_.trim).toSeq
        }

    )
  }
}

/**
 * Just a plain mapping of the database
 */
case class ExpectedConfRuleMapping(
    val pkId : Int,
    val nodeJoinKey : Int,
    val ruleId : RuleId,
    val serial : Int,
    val policyExpectedReport : DirectiveId,
    val component : String,
    val cardinality : Int,
    val componentsValues : Seq[String],
    val unexpandedComponentsValues : Seq[String],
    // the period where the configuration is applied to the servers
    val beginDate : DateTime = DateTime.now(),
    val endDate : Option[DateTime] = None
) extends HashcodeCaching


 object ComponentsValuesSerialiser {

  def serializeComponents(ids:Seq[String]) : String = {
    implicit val formats = Serialization.formats(NoTypeHints)
    Serialization.write(ids)
  }
  /*
   * from a JSON array: [ "id1", "id2", ...], get the list of
   * components values Ids.
   * Never fails, but returned an empty list.
   */
  def unserializeComponents(ids:String) : Seq[String] = {
    implicit val formats = DefaultFormats
    parse(ids).extract[List[String]]
 }

}
