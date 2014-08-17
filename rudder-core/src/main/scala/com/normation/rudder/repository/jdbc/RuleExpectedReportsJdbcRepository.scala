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
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource

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


  override def findExpectedReportsByNodeConfigId(nodeConfigId: NodeConfigId): Box[Seq[RuleExpectedReports]] = {
    /*
     * Select agentRun join expectedReportsNodes join expectedreports
     */

    val query = """select pkid, nodejoinkey, ruleid, serial, directiveid, component, cardinality, componentsvalues, unexpandedComponentsValues, begindate, enddate, J.nodeid, J.nodeconfigversion
      from expectedreports as A
      join (
        select nodejoinkey, nodeid, nodeconfigversion from expectedreportsnodes as B
        where B.nodeId = ? and B.nodeconfigversions like ?
      ) as J on A.nodejoinkey = J.nodejoinkey;"""

    try {
      toRuleExpectedReports(jdbcTemplate.query(query, Array[AnyRef](nodeConfigId.nodeId.value, "%"+nodeConfigId.version+"%"), RuleExpectedReportsMapper).toSeq)
    } catch {
      case e:Exception => Failure(s"Error when getting expected report for node '${nodeConfigId.nodeId.value}' and configuration version '${nodeConfigId.version}'", Full(e), Empty)
    }
  }

  override def findAllCurrentExpectedReportsWithNodesAndSerial(): Map[RuleId, (Int, Map[NodeId, NodeConfigVersions])] = {
    val composite = jdbcTemplate.query("select distinct ruleid, serial, nodejoinkey from expectedreports where enddate is null", RuleIdSerialNodeJoinKeyMapper)

    (for {
      (ruleId, serial, nodeJoin) <- composite
      nodeList <- getNodes(Set(nodeJoin))
    } yield {
      (ruleId, (serial, nodeList))
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
            Full({})
      case Full(Some(entry)) =>
        jdbcTemplate.update("update expectedreports  set enddate = ? where serial = ? and ruleId = ?",
          new Timestamp(DateTime.now().getMillis), new java.lang.Integer(entry.serial), entry.ruleId.value
        )
        Full({}) // unit is expected
    }
  }


  /**
   * This utilitary class is used only to compare what is already saved in the
   * DB and compare it with what is to be saved
   */
  private[this] final case class Comparator(
      nodeConfigId : (NodeId, Option[NodeConfigVersion])
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
    , nodeConfigIds           : Seq[NodeConfigId]
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
           nodeConfigIds.map(id => Comparator((id.nodeId, Some(id.version)), dir, compName))
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
    , nodeConfigIds           : Seq[NodeConfigId]
  ) : Box[RuleExpectedReports] = {

    transactionTemplate.execute(new TransactionCallback[Box[RuleExpectedReports]]() {
      def doInTransaction(status: TransactionStatus): Box[RuleExpectedReports] = {
        // Compute first the version id
        val nodeJoinKey = jdbcTemplate.queryForInt("SELECT nextval('ruleVersionId')")


        // Create the lines for the mapping
        val list = for {
                policy <- directiveExpectedReports
                component <- policy.components
        } yield {
          jdbcTemplate.update("""insert into expectedreports (
              nodejoinkey, ruleid, serial, directiveid, component, cardinality, componentsValues, unexpandedComponentsValues, begindate
            ) values (?,?,?,?,?,?,?,?,?)"""
          , new java.lang.Integer(nodeJoinKey), ruleId.value, new java.lang.Integer(serial), policy.directiveId.value
          , component.componentName,  new java.lang.Integer(component.cardinality), ComponentsValuesSerialiser.serializeComponents(component.componentsValues)
          , ComponentsValuesSerialiser.serializeComponents(component.unexpandedComponentsValues), new Timestamp(System.currentTimeMillis)
          )
        }

        // save new nodeconfiguration - no need to check for existing version for them
        for (config <- nodeConfigIds) {
          jdbcTemplate.update("insert into expectedreportsnodes ( nodejoinkey, nodeid, nodeconfigversions) values (?,?,?)",
            new java.lang.Integer(nodeJoinKey), config.nodeId, config.version
          )
        }

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

  /**
   * Update the set of nodes to have the given node ConfigVersion.
   * As we don't have other information, we will update "last"
   * (i.e row with the biggest nodeJoin key).
   */
  override def updateNodeConfigVersion(toUpdate: Map[NodeId, NodeConfigVersion]): Box[Seq[(Int,NodeConfigVersions)]] = {

    object NodeConfigVersionsMapper extends RowMapper[(Int,NodeConfigVersions)] {
      def mapRow(rs : ResultSet, rowNum: Int) : (Int,NodeConfigVersions) = {
        val k = rs.getInt("nodejoinkey")
        val n = NodeConfigVersions(NodeId(rs.getString("ruleid")), NodeConfigVersionsSerializer.unserialize(rs.getString("nodeconfigversion")))
        (k,n)
      }
    }

    val select = """select nodejoinkey, nodeid, nodeconfigversions
          from expectedreportsnodes as exp
          inner join (
            select max(nodejoinkey) as maxKey, nodeid
            from expectedreportsnodes
            group by nodeid
            where nodeid in ?
          ) grouped on exp.nodejoinkey = grouped.nodejoinkey and grouped.nodeid = exp.nodeid"""

    val insert = """insert into expectedreportsnodes ( nodejoinkey, nodeid, nodeconfigversions) values (?,?,?)"""

    for {
      configs <- tryo(jdbcTemplate.query(select, Array[AnyRef](toUpdate.keys.map(_.value).mkString("(", "," , ")")), NodeConfigVersionsMapper).toSeq)
      updates <- sequence(configs) { c =>
                   tryo(jdbcTemplate.update(insert
                       , new java.lang.Integer(c._1)
                       , c._2.nodeId
                         //no need to getOrElse, we have at least all the node id returned by the query in the map
                       , NodeConfigVersionsSerializer.serialize(toUpdate(c._2.nodeId)::c._2.versions)
                   ))
                 }
    } yield {
      configs
    }
  }

  /**
   * Save the server list in the database
   */
  private[this] def updateNodes(configs: Seq[(Int,NodeConfigVersions)]): Box[Seq[(Int,NodeConfigVersions)]] = {
    tryo {
      for ((nodeJoinKey, NodeConfigVersions(nodeId, versions)) <- configs) {
        val versionsString = NodeConfigVersionsSerializer.serialize(versions)
        jdbcTemplate.update(
            "update expectedreportsnodes set nodeconfigversions = ? where nodejoinkey = ? and nodeid = ?"
          ,  versionsString, new java.lang.Integer(nodeJoinKey), nodeId.value
        )
      }
      configs
    }
  }

  /**
   * Return currents expectedreports (the one still pending) for this server, but in the
   * case where we don't know the node config version
   */
  override def findLatestExpectedReportsByNode(nodeId : NodeId) : Box[Seq[RuleExpectedReports]] = {
    val joinQuery =
      s"""select pkid, expectedreports.nodejoinkey, ruleid,directiveid, serial, component, componentsvalues, unexpandedComponentsValues, cardinality, begindate, enddate
          from expectedreports
          join expectedreportsnodes on expectedreportsnodes.nodejoinkey = expectedreports.nodejoinkey
          where enddate is null and  expectedreportsnodes.nodeId = ?"""

    toRuleExpectedReports(jdbcTemplate.query(joinQuery,
          Array[AnyRef](nodeId.value),
          RuleExpectedReportsMapper).toSeq)

  }

  /**
   * Retrieve the node config version for a set of expected reports key.
   * If a node appears is bound to several expected reports key,
   * then its config version will be sorted accordingly to the key: a
   * bigger key mean more recent versions.
   */
  private[jdbc] def getNodes(nodeJoinKeys : Set[Int]) : Box[Map[NodeId, NodeConfigVersions]] = {
    tryo {
      jdbcTemplate.query(
          s"select nodejoinkey, nodeid, nodeconfigversions from expectedreportsnodes where nodejoinkey in ${nodeJoinKeys.mkString("(", ",", ")")}"
        , NodeJoinKeyConfigMapper
      ).groupBy(_._2.nodeId).mapValues { seq => //seq cannot be empty due to groupBy
        //merge version together based on nodejoin values
        (seq.reduce[(Int, NodeConfigVersions)] { case ( (maxK, versions), (newK, newConfigVersions) ) =>
          if(maxK >= newK) {
            (maxK, versions.copy(versions = versions.versions ::: newConfigVersions.versions))
          } else {
            (newK, versions.copy(versions = newConfigVersions.versions ++ versions.versions))
          }
        })._2 //only keep the nodeConfigpart
      }
    }
  }

  object NodeJoinKeyConfigMapper extends RowMapper[(Int, NodeConfigVersions)] {
    def mapRow(rs : ResultSet, rowNum: Int) : (Int, NodeConfigVersions) = {
      val nodeId = new NodeId(rs.getString("nodeid"))
      val versions = NodeConfigVersionsSerializer.unserialize(rs.getString("nodeconfigversions"))
      (rs.getInt("nodejoinkey"), NodeConfigVersions(nodeId, versions))
    }
  }

  /**
   * Effectively convert lines from the DB to RuleExpectedReports
   */
  private[this] def toRuleExpectedReports(entries : Seq[ExpectedConfRuleMapping]) : Box[Seq[RuleExpectedReports]] = {
    //find nodeConfigId by nodeJoinKey
    val nodeConfigIds = entries.groupBy( _.nodeJoinKey ).mapValues { _.map(x => (x.nodeId, x.configVersions.headOption)).toMap }

    Full(entries.groupBy( entry => SerialedRuleId(entry.ruleId, entry.serial)).map { case (key, seq) =>
      // now we need to group elements of the seq together,  based on nodeJoinKey
      val directivesOnNode = seq.groupBy(x => x.nodeJoinKey).map { case (nodeJoinKey, mappedEntries) =>
        // need to convert to group everything by directiveId, the convert to DirectiveExpectedReports
        val directiveExpectedReports = mappedEntries.groupBy(x => x.directiveId).map { case (directiveId, lines) =>
          // here I am on the directiveId level, all lines that have the same RuleId, Serial, NodeJoinKey, DirectiveId are
          // for the same directive, and must be put together
          DirectiveExpectedReports(directiveId, lines.map( x => ReportComponent(x.component, x.cardinality, x.componentsValues, x.unexpandedComponentsValues)))
        }

        DirectivesOnNodes(nodeJoinKey, nodeConfigIds(nodeJoinKey), directiveExpectedReports.toSeq)
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
        rs.getInt("pkid")
      , rs.getInt("nodejoinkey")
      , new RuleId(rs.getString("ruleid"))
      , rs.getInt("serial")
      , DirectiveId(rs.getString("directiveid"))
      , rs.getString("component")
      , rs.getInt("cardinality")
      , ComponentsValuesSerialiser.unserializeComponents(rs.getString("componentsvalues"))
      , ComponentsValuesSerialiser.unserializeComponents(unexpandedcomponentsvalues)
      , new DateTime(rs.getTimestamp("begindate"))
      , if(rs.getTimestamp("enddate")!=null) {
          Some(new DateTime(rs.getTimestamp("endDate")))
        } else None
      , NodeId(rs.getString("nodeid"))
      , if(rs.getTimestamp("date") == null) None else Some(new DateTime(rs.getTimestamp("date")))
      , rs.getBoolean("complete")
      , NodeConfigVersionsSerializer.unserialize(rs.getString("nodeConfigVersion"))
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


/**
 * Map a row of the database with a node
 */
case class ExpectedConfRuleMapping(
    val pkId : Int
  , val nodeJoinKey : Int
  , val ruleId : RuleId
  , val serial : Int
  , val directiveId : DirectiveId
  , val component : String
  , val cardinality : Int
  , val componentsValues : Seq[String]
  , val unexpandedComponentsValues : Seq[String]
    // the period where the configuration is applied to the servers
  , val beginDate : DateTime = DateTime.now()
  , val endDate : Option[DateTime] = None
  , val nodeId: NodeId
  , val agentRunTime: Option[DateTime]
  , val isCompleted: Boolean
  , val configVersions: List[NodeConfigVersion]
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


object NodeConfigVersionsSerializer {

  def serialize(versions: List[NodeConfigVersion]): String = {
    implicit val formats = Serialization.formats(NoTypeHints)
    Serialization.write(versions.reverse.map(_.value.trim))
  }

  def unserialize(versions: String): List[NodeConfigVersion] = {
    if(null == versions || versions == "") Nil
    else {
      implicit val formats = DefaultFormats
      parse(versions).extract[List[String]].reverse.map(_.trim).filterNot( _.isEmpty).map(NodeConfigVersion(_))
    }
  }
}