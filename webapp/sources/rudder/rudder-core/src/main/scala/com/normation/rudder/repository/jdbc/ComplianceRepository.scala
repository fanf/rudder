/*
 *************************************************************************************
 * Copyright 2016 Normation SAS
 *************************************************************************************
 *
 * This file is part of Rudder.
 *
 * Rudder is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In accordance with the terms of section 7 (7. Additional Terms.) of
 * the GNU General Public License version 3, the copyright holders add
 * the following Additional permissions:
 * Notwithstanding to the terms of section 5 (5. Conveying Modified Source
 * Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU General
 * Public License version 3, when you create a Related Module, this
 * Related Module is not considered as a part of the work and may be
 * distributed under the license agreement of your choice.
 * A "Related Module" means a set of sources files including their
 * documentation that, without modification of the Source Code, enables
 * supplementary functions or services in addition to those offered by
 * the Software.
 *
 * Rudder is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Rudder.  If not, see <http://www.gnu.org/licenses/>.

 *
 *************************************************************************************
 */

package com.normation.rudder.repository.jdbc

import cats.implicits.*
import com.normation.errors.*
import com.normation.errors.IOResult
import com.normation.inventory.domain.NodeId
import com.normation.rudder.db.Doobie
import com.normation.rudder.db.Doobie.*
import com.normation.rudder.domain.logger.ReportLogger
import com.normation.rudder.domain.reports.ComplianceLevel
import com.normation.rudder.domain.reports.CompliancePercent
import com.normation.rudder.domain.reports.NodeStatusReport
import com.normation.rudder.domain.reports.RuleNodeStatusReport
import com.normation.rudder.domain.reports.RunAnalysis
import com.normation.rudder.domain.reports.RunAnalysisKind
import com.normation.rudder.domain.reports.RunComplianceInfo
import com.normation.rudder.repository.ComplianceRepository
import doobie.*
import doobie.implicits.*
import net.liftweb.common.Box
import org.joda.time.DateTime
import zio.interop.catz.*
import zio.syntax.*

final case class RunCompliance(
    nodeId:       NodeId,
    runTimestamp: DateTime,
    endOfLife:    DateTime,
    runInfo:      (RunAnalysis, RunComplianceInfo),
    summary:      CompliancePercent,
    details:      Set[RuleNodeStatusReport]
)

object RunCompliance {

  def from(runTimestamp: DateTime, endOfLife: DateTime, report: NodeStatusReport): RunCompliance = {
    RunCompliance(
      report.nodeId,
      runTimestamp,
      endOfLife,
      (report.runInfo, report.statusInfo),
      report.compliance.computePercent(),
      report.reports.flatMap(_._2.reports).toSet
    )
  }
}

class ComplianceJdbcRepository(
    doobie:                   Doobie,
    getSaveComplianceDetails: () => Box[Boolean],
    getSaveComplianceLevels:  () => Box[Boolean]
) extends ComplianceRepository {
  import doobie.*

  val logger = ReportLogger

  val nodeComplianceLevelcolumns: List[String] = List(
    "nodeid",
    "runtimestamp",
    "ruleid",
    "directiveid",
    "pending",
    "success",
    "repaired",
    "error",
    "unexpected",
    "missing",
    "noanswer",
    "notapplicable",
    "reportsdisabled",
    "compliant",
    "auditnotapplicable",
    "noncompliant",
    "auditerror",
    "badpolicymode"
  )

  /*
   * Save a list of node compliance reports
   */
  override def saveRunCompliance(reports: List[NodeStatusReport]): IOResult[Unit] = {

    // Only compute compliance if we need to save complianceDetails or complianceLevels
    val runCompliances = if (getSaveComplianceDetails().getOrElse(false) || getSaveComplianceLevels().getOrElse(true)) {
      /*
       * some sorting of things. We must only store information about node status reports with
       * a run
       */
      reports.flatMap { r =>
        r.runInfo.kind match {
          // ignore case with no runs or when compliance should be kept
          case RunAnalysisKind.KeepLastCompliance | RunAnalysisKind.NoReportInInterval | RunAnalysisKind.NoRunNoExpectedReport |
              RunAnalysisKind.ReportsDisabledInInterval | RunAnalysisKind.NoUserRulesDefined =>
            None
          case _ =>
            (r.runInfo.lastRunDateTime, r.runInfo.expirationDateTime) match {
              case (Some(a), Some(b)) =>
                Some(RunCompliance.from(a, b, r))
              case (Some(a), None)    =>
                r.runInfo.lastRunExpiration match {
                  case Some(b) => Some(RunCompliance.from(a, b, r))
                  // here, the expiration date has not much meaning, since we don't have
                  // information on that node configuration (and so the node has most likely no
                  // idea whatsoever of any config, even global). Take default values,
                  // ie 5min for run + 5min for grace
                  case None    => Some(RunCompliance.from(a, a.plusMinutes(10), r))
                }
              case _                  => None
            }
        }
      }
    } else {
      Nil
    }
    type LEVELS = (String, DateTime, String, String, ComplianceLevel)

    val nodeComplianceLevels: List[LEVELS] = if (getSaveComplianceLevels().getOrElse(true)) {
      runCompliances.flatMap { run =>
        // one aggregatestatus reports can hold several RuleNodeStatusReports with the
        // same node/rule/run but different serial. Here, we already know the nodeid and run,
        // so group by ruleId, get directives, merge.

        run.details.groupBy(_.ruleId).flatMap {
          case (ruleId, aggregats) =>
            // get a map of all (directiveId -> seq(directives)
            // be carefull to "toList", because we don't want to deduplicate if
            // two directive are actually equal
            aggregats.toList.flatMap(_.directives.values).groupBy(_.directiveId).map {
              case (directiveId, seq) =>
                (
                  run.nodeId.value,
                  run.runTimestamp,
                  ruleId.serialize,
                  directiveId.serialize,
                  ComplianceLevel.sum(seq.map(_.compliance))
                )
            }
        }
      }
    } else {
      Nil
    }

    val saveComplianceDetails: ConnectionIO[Unit] = if (getSaveComplianceDetails().getOrElse(false)) {
      val queryCompliance = """insert into nodecompliance (nodeid, runtimestamp, endoflife, runanalysis, summary, details)
                              | values (?, ?, ?, ?, ?, ?)""".stripMargin
      Update[RunCompliance](queryCompliance).updateMany(runCompliances).void
    } else {
      logger
        .trace(
          s"Not persisting compliance details in table 'nodecompliance' because settings 'rudder_save_db_compliance_details' is undefined or false"
        )
        .pure[ConnectionIO]
    }

    val saveComplianceLevels = if (getSaveComplianceLevels().getOrElse(true)) {
      val queryComplianceLevel = s"""insert into nodecompliancelevels (${nodeComplianceLevelcolumns.mkString(",")})
                                    | values ( ${nodeComplianceLevelcolumns.map(_ => "?").mkString(",")} )""".stripMargin
      Update[LEVELS](queryComplianceLevel).updateMany(nodeComplianceLevels)
    } else {
      logger
        .trace(
          s"Not persisting compliance levels in table 'nodecompliancelevels' because settings 'rudder_save_db_compliance_level' is false"
        )
        .pure[ConnectionIO]
    }

    transactIOResult("Error when saving node compliances:")(xa => {
      (for {
        _ <- saveComplianceDetails
        _ <- saveComplianceLevels
      } yield ()).transact(xa)
    }).foldZIO(
      err => {
        // we need to filter out `ERROR: duplicate key value violates unique constraint "nodecompliancelevels_pkey"`
        // see https://issues.rudder.io/issues/18188 for details
        val fullMsg = err.fullMsg
        if (fullMsg.contains("""nodecompliancelevels_pkey""")) {
          effectUioUnit {
            logger.debug(s"Ignored duplicate key violation, see: https://issues.rudder.io/issues/18188. Message is: " + fullMsg)
          }
        } else {
          // that message can be huge because it may contains whole nodecompliance json. Truncate it in error, and
          // display whole in debub
          effectUioUnit {
            val msg = if (fullMsg.size > 200) { fullMsg.substring(0, 196) ++ "..." }
            else { fullMsg }
            logger.error(" " + msg)
            if (msg.endsWith("...")) {
              logger.debug("Full error message was: " + fullMsg)
            }
          } *> err.fail
        }
      },
      res => res.succeed
    )
  }

}
