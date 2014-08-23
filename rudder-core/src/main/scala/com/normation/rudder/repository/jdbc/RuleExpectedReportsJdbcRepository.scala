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

import scala.collection.JavaConverters.asScalaBufferConverter
import java.sql.Timestamp
import java.sql.ResultSet
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies._
import com.normation.rudder.domain.reports._
import com.normation.rudder.repository.UpdateExpectedReportsRepository
import com.normation.rudder.repository.FindExpectedReportRepository
import com.normation.utils.HashcodeCaching
import com.normation.utils.Control.sequence
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.core.RowMapper
import org.springframework.transaction.PlatformTransactionManager
import org.springframework.transaction.TransactionStatus
import org.springframework.transaction.support.TransactionCallback
import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.util.Helpers.tryo
import org.joda.time.DateTime

class FindExpectedReportsJdbcRepository(
    jdbcTemplate      : JdbcTemplate
) extends FindExpectedReportRepository with Loggable {

  /*
   * Build a clause to matche if the given attribute is in the given list of values.
   *
   * Try to be as efficient as possible for postgres. TODO: check VALUES; ARRAY
   *
   * http://postgres.cz/wiki/PostgreSQL_SQL_Tricks_I#Predicate_IN_optimalization
   *
   * Does not build anything is the list of values is empty
   *
   */
  private[this] val IN_CLAUSE_MAX_NB_ELT = 2 //should be ~70
  private[this] def in(attribute: String, values: Iterable[String]): String = {
    if(values.isEmpty) ""
    else if(values.size < IN_CLAUSE_MAX_NB_ELT) s"${attribute} IN (${values.mkString("'", "','", "'")})"
    //use IN ( VALUES (), (), ... )
    else s"${attribute} IN(VALUES ${values.map(x => if(x.trim.startsWith("(")) x else s"('$x')").mkString(",")})"
  }

  /*
   * Retrieve the last expected reports for the nodes.
   */
  def getLastExpectedReports(nodeIds: Set[NodeId], filterByRules: Set[RuleId]): Box[Set[RuleExpectedReports]] = {
    if(nodeIds.isEmpty) Full(Set())
    else {
      val rulePredicate = if(filterByRules.isEmpty) "" else " and " + in("ruleid", filterByRules.map(_.value))
      val where = s"""where E.enddate is null and ${in("N.nodeid", nodeIds.map(_.value))} ${rulePredicate}"""

      getRuleExpectedReports(where, Array()).map(_.toSet)
    }
  }

  /*
   * Retrieve the expected reports by config version of the nodes
   */
  def getExpectedReports(nodeConfigIds: Set[NodeConfigId], filterByRules: Set[RuleId]): Box[Set[RuleExpectedReports]] = {
    if(nodeConfigIds.isEmpty) Full(Set())
    else {
      val rulePredicate = if(filterByRules.isEmpty) "" else " and " + in("ruleid", filterByRules.map(_.value))
      val where = s"""where ${in("(N.nodeid, N.nodeconfigversion)", nodeConfigIds.map(x => s"('${x.nodeId.value}','${x.version.value}')"))} ${rulePredicate}"""

      getRuleExpectedReports(where, Array()).map(_.toSet)
    }
  }

  /**
   * Return current expected reports (the one still pending) for this Rule
   */
  override def findCurrentExpectedReports(ruleId : RuleId) : Box[Option[RuleExpectedReports]] = {
    getRuleExpectedReports("where enddate is null and ruleid = ?", Array[AnyRef](ruleId.value))  match {
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


  /**
   * Return all the expected reports between the two dates
   */
  override def findExpectedReports(beginDate : DateTime, endDate : DateTime) : Box[Seq[RuleExpectedReports]] = {
    val params = Array[AnyRef](new Timestamp(endDate.getMillis), new Timestamp(beginDate.getMillis), new Timestamp(beginDate.getMillis))

    getRuleExpectedReports("where beginDate < ? and coalesce(endDate, ?) >= ? ", params)
  }

  private[this] def getRuleExpectedReports(whereClause: String, params: Array[AnyRef]) : Box[Seq[RuleExpectedReports]] = {
    val expectedReportsQuery ="""select
          E.pkid, E.nodejoinkey, E.ruleid, E.directiveid, E.serial, E.component, E.componentsvalues
        , E.unexpandedComponentsValues, E.cardinality, E.begindate, E.enddate
        , N.nodeid, N.nodeconfigversions
      from expectedreports E
      inner join expectedreportsnodes N
      on E.nodejoinkey = N.nodejoinkey """ + whereClause

    for {
      entries <- tryo { if(params.isEmpty) {
                   jdbcTemplate.query(expectedReportsQuery, ReportAndNodeMapper).asScala
                 } else {
                   jdbcTemplate.query(expectedReportsQuery, params, ReportAndNodeMapper).asScala
                 } }
    } yield {
      toExpectedReports(entries)
    }
  }

  private[this] def toExpectedReports(entries:Seq[ReportAndNodeMapping]) : Seq[RuleExpectedReports] = {
    entries.groupBy( entry => SerialedRuleId(entry.ruleId, entry.serial)).map { case (key, seq) =>
      // now we need to group elements of the seq together,  based on nodeJoinKey
      val directivesOnNode = seq.groupBy(x => x.nodeJoinKey).map { case (nodeJoinKey, mappedEntries) =>
        // need to convert to group everything by directiveId, the convert to DirectiveExpectedReports
        val directiveExpectedReports = mappedEntries.groupBy(x => x.directiveId).map { case (directiveId, lines) =>
          // here I am on the directiveId level, all lines that have the same RuleId, Serial, NodeJoinKey, DirectiveId are
          // for the same directive, and must be put together
          DirectiveExpectedReports(directiveId, lines.map( x =>
            ReportComponent(x.component, x.cardinality, x.componentsValues, x.unexpandedCptsValues)
          ).distinct /* because we have the cardinality for that */ )
        }
        val nodeConfigurationIds = mappedEntries.groupBy( _.nodeId).mapValues { lines =>
          //we should have only one line at that level, but else, merger versions
          lines.reduce { (current, next) =>
            if(current.nodeJoinKey >= next.nodeJoinKey) {
              current.copy(nodeConfigVersions = current.nodeConfigVersions ::: next.nodeConfigVersions)
            } else {
              current.copy(nodeConfigVersions = next.nodeConfigVersions ::: current.nodeConfigVersions)
            }
          }.nodeConfigVersions.headOption
        }

        DirectivesOnNodes(nodeJoinKey, nodeConfigurationIds, directiveExpectedReports.toSeq)
      }
      RuleExpectedReports(
          key.ruleId
        , key.serial
        , directivesOnNode.toSeq
        , seq.head.beginDate
        , seq.head.endDate
      )
    }.toSeq
  }

}


class UpdateExpectedReportsJdbcRepository(
    jdbcTemplate      : JdbcTemplate
  , transactionManager: PlatformTransactionManager
  , findReports       : FindExpectedReportsJdbcRepository
) extends UpdateExpectedReportsRepository with Loggable {

  /**
   * We need to create transaction for the insertion of expected reports
   * otherwise race conditions may occur
   * We are clearly pushing the complexity of jdbcTemplate, and will need to move to
   * higher lever abstraction for this
   */
  val transactionTemplate = new org.springframework.transaction.support.TransactionTemplate(transactionManager)



  private[jdbc] def getNodes(nodeJoinKeys : Set[Int]) : Box[Map[Int, Map[NodeId, List[NodeConfigVersion]]]] = {

    object NodeJoinKeyConfigMapper extends RowMapper[(Int, NodeConfigVersions)] {
      def mapRow(rs : ResultSet, rowNum: Int) : (Int, NodeConfigVersions) = {
        val nodeId = new NodeId(rs.getString("nodeid"))
        val versions = NodeConfigVersionsSerializer.unserialize(rs.getString("nodeconfigversions"))
        (rs.getInt("nodejoinkey"), NodeConfigVersions(nodeId, versions))
      }
    }

    if(nodeJoinKeys.isEmpty) Full(Map())
    else tryo {
      val x = jdbcTemplate.query(
          s"select nodejoinkey, nodeid, nodeconfigversions from expectedreportsnodes where nodejoinkey in ${nodeJoinKeys.mkString("(", ",", ")")}"
        , NodeJoinKeyConfigMapper
      ).asScala.groupBy(_._2.nodeId).mapValues { seq => //seq cannot be empty due to groupBy
        //merge version together based on nodejoin values
        (seq.reduce[(Int, NodeConfigVersions)] { case ( (maxK, versions), (newK, newConfigVersions) ) =>
          if(maxK >= newK) {
            (maxK, versions.copy(versions = versions.versions ::: newConfigVersions.versions))
          } else {
            (newK, versions.copy(versions = newConfigVersions.versions ::: versions.versions))
          }
        })
      }

      x.values.groupBy(_._1).mapValues(_.map{case(_, NodeConfigVersions(id,v)) => (id,v)}.toMap)
    }
  }

  override def findAllCurrentExpectedReportsWithNodesAndSerial(): Map[RuleId, (Int, Int, Map[NodeId, NodeConfigVersions])] = {
    val composite = jdbcTemplate.query("select distinct ruleid, serial, nodejoinkey from expectedreports where enddate is null", RuleIdSerialNodeJoinKeyMapper)

    (for {
      (ruleId, serial, nodeJoin) <- composite.asScala
      nodeList                   <- getNodes(Set(nodeJoin))
    } yield {
      (ruleId, (serial, nodeJoin, nodeList(nodeJoin).map{case(nodeId, versions) => (nodeId, NodeConfigVersions(nodeId, versions))}.toMap ))
    }).toMap

  }


  /**
   * Simply set the endDate for the expected report for this conf rule
   * @param ruleId
   */
  override def closeExpectedReport(ruleId : RuleId) : Box[Unit] = {
    logger.debug(s"Closing expected report for rules '${ruleId.value}'")
    findReports.findCurrentExpectedReports(ruleId) match {
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
   * Insert new expectedReports in base.
   *
   */
  override def saveExpectedReports(
      ruleId                  : RuleId
    , serial                  : Int
    , directiveExpectedReports: Seq[DirectiveExpectedReports]
    , nodeConfigIds           : Seq[NodeConfigId]
  ) : Box[RuleExpectedReports] = {
     logger.debug(s"Saving expected report for rule '${ruleId.value}'")
// TODO : store also the unexpanded
     findReports.findCurrentExpectedReports(ruleId) match {
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
          policy    <- directiveExpectedReports
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
            new java.lang.Integer(nodeJoinKey), config.nodeId.value, NodeConfigVersionsSerializer.serialize(List(config.version))
          )
        }

        findReports.findCurrentExpectedReports(ruleId) match {
          case Full(Some(x)) => Full(x)
          case Full(None)    => Failure("Could not fetch the freshly saved expected report for rule %s".format(ruleId.value))
          case eb: EmptyBox  => eb
        }
      }
    })

  }

  /**
   * Update the set of nodes to have the given node ConfigVersion.
   * As we don't have other information, we will update "last"
   * (i.e row with the biggest nodeJoin key).
   */
  override def updateNodeConfigVersion(toUpdate: Seq[(Int, NodeConfigVersions)]): Box[Seq[(Int,NodeConfigVersions)]] = {

    object NodeConfigVersionsMapper extends RowMapper[(Int,NodeConfigVersions)] {
      def mapRow(rs : ResultSet, rowNum: Int) : (Int,NodeConfigVersions) = {
        val k = rs.getInt("nodejoinkey")
        val n = NodeConfigVersions(NodeId(rs.getString("nodeid")), NodeConfigVersionsSerializer.unserialize(rs.getString("nodeconfigversions")))
        (k,n)
      }
    }

    if(toUpdate.isEmpty) Full(Seq())
    else {
      val insert = """update expectedreportsnodes set nodeconfigversions = ? where nodejoinkey = ? and nodeid = ?"""

      for {
        updates <- sequence(toUpdate) { case(nodeJoinKey, NodeConfigVersions(nodeId, versions)) =>
                     tryo(jdbcTemplate.update(insert
                         , NodeConfigVersionsSerializer.serialize(versions)
                         , new java.lang.Integer(nodeJoinKey)
                         , nodeId.value
                           //no need to getOrElse, we have at least all the node id returned by the query in the map
                     ))
                   }
      } yield {
        toUpdate
      }
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

}

case class ReportAndNodeMapping(
    val pkId                : Int
  , val nodeJoinKey         : Int
  , val ruleId              : RuleId
  , val serial              : Int
  , val directiveId         : DirectiveId
  , val component           : String
  , val cardinality         : Int
  , val componentsValues    : Seq[String]
  , val unexpandedCptsValues: Seq[String]
  , val beginDate           : DateTime = DateTime.now()
  , val endDate             : Option[DateTime] = None
  , val nodeId              : NodeId
  , val nodeConfigVersions  : List[NodeConfigVersion]
) extends HashcodeCaching

object ReportAndNodeMapper extends RowMapper[ReportAndNodeMapping] {
  def mapRow(rs : ResultSet, rowNum: Int) : ReportAndNodeMapping = {
    // unexpandedcomponentsvalues may be null, as it was not defined before 2.6
    val unexpandedcomponentsvalues = rs.getString("unexpandedcomponentsvalues") match {
      case null => ""
      case value => value
    }
    new ReportAndNodeMapping(
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
      , NodeConfigVersionsSerializer.unserialize(rs.getString("nodeconfigversions"))
    )
  }
}

object RuleIdSerialNodeJoinKeyMapper extends RowMapper[(RuleId, Int, Int)] {
  def mapRow(rs : ResultSet, rowNum: Int) : (RuleId, Int, Int) = {
    (new RuleId(rs.getString("ruleid")) , rs.getInt("serial"), rs.getInt("nodejoinkey"))
  }
}

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