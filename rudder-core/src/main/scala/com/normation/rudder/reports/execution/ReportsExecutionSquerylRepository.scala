/*
*************************************************************************************
* Copyright 2013 Normation SAS
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

package com.normation.rudder.reports.execution

import java.sql.Timestamp

import org.joda.time.DateTime
import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
import org.squeryl.dsl.CompositeKey2

import com.normation.inventory.domain.NodeId
import com.normation.rudder.repository.jdbc.SquerylConnectionProvider

import net.liftweb.common._
import ExecutionRepositoryUtils._

case class RoReportsExecutionSquerylRepository (
    sessionProvider : SquerylConnectionProvider
) extends RoReportsExecutionRepository with Loggable {

  def getNodeLastExecution (nodeId : NodeId) : Box[Option[ReportExecution]] = {
    try {  sessionProvider.ourTransaction {
      val queryResult = from(Executions.executions)(entry =>
        where(
          entry.nodeId === nodeId.value
        )
        select(entry)
        orderBy(entry.date desc)
      ).page(0,1).headOption.map(fromDB)
      Full(queryResult)
    } } catch {
          case e:Exception  =>
            val msg = s"Error when trying to get report executions for node with Id '${nodeId.value}', reason is ${e.getMessage()}"
            logger.error(msg, e)
            Failure(msg)
    }
  }
}

case class WoReportsExecutionSquerylRepository (
  readExecutions  : RoReportsExecutionSquerylRepository
) extends WoReportsExecutionRepository with Loggable {

  import readExecutions._


  def updateExecutions(executions : Seq[ReportExecution]) : Box[Seq[ReportExecution]] =  {
    //"contains" is only defined regarding the primary key
    def same(x:ReportExecution, y:ReportExecution) = {
      x.nodeId == y.nodeId && x.date == y.date
    }

    def find(r:ReportExecution, seq: Seq[ReportExecution]) = {
      seq.find(x => same(x,r))
    }

    //
    // Question: do we want to save an updated nodeConfigurationVersion ?
    // for now, say we update all
    //

    /*
     * Three cases:
     * - already saved, completed: update them but keeping the "completed" state
     * - already saved, not completed: update them with whatever we found
     * - not already saved: insert them
     */
    for {
      existingExec <- getExecutionsByNodeAndDate(executions) ?~! "Could not fetch the already save executions date for node"

      //insert all execution in parameter not already existing
      toInsert     =  executions.filterNot( x => find(x, existingExec).isDefined )

      //find the list of execution to update, with their actual values
      toUpdate     =  executions.flatMap( x =>
                        find(x, existingExec)
                        //for the one toUpdate, always keep the most recent
                        // nodeConfigurationVersion and the most completed status
                        .flatMap { y =>
                          val completed = x.isCompleted || y.isCompleted
                          val version = x.nodeConfigVersion
                          val toSave = x.copy( isCompleted = completed, nodeConfigVersion = version)
                          if(toSave == y) None else Some(toSave)
                        }
                      )
      updated         <- update(toUpdate) ?~! s"Could not close or update the ${toUpdate.size} execution that has been completed during the interval"
      inserted        <- insert(toInsert) ?~! s"Could not create the ${toInsert.size} execution that has been completed during the interval"
    } yield {
      updated ++ inserted
    }
  }

  private[this] def exec[A, B](body: => B)(res: B => A)(errorMessage: Exception => String): Box[A] = {
    try {
      val a = sessionProvider.ourTransaction {
        body
      }
      Full(res(a))
    } catch {
      case e:Exception  =>
        val msg = errorMessage(e)
        logger.error(msg, e)
        Failure(msg)
    }
  }

  /**
   * From a seq of found executions in RudderSysEvents, find in the existing executions matching
   */
  private[this] def getExecutionsByNodeAndDate (executions: Seq[ReportExecution]) : Box[Seq[ReportExecution]] = {
    exec {
      val result = executions.flatMap { execution =>
        from(Executions.executions)(entry =>
          where(
                entry.nodeId === execution.runId.nodeId.value
            and entry.date   === toTimeStamp(execution.runId.date)
          )
          select(entry)
        )
      }

      result.toSeq.map(fromDB)
    } { a => a
    } {
      e: Exception => s"Error when trying to get nodes report executions, reason is ${e.getMessage()}"
    }
  }


  private[this] def insert(executions : Seq[ReportExecution]) : Box[Seq[ReportExecution]] =  {
    exec {
      Executions.executions.insert(executions.map( toDB(_) ))
    } { saveResult =>
      executions
    } {
      e:Exception =>
        s"Could not save the ${executions.size} nodes executions, reason is ${e.getMessage()}"
    }
  }

  /**
   * Save the EXISTING execution
   */
  private[this] def update(executions : Seq[ReportExecution]) : Box[Seq[ReportExecution]] =  {
    exec {
      Executions.executions.update(executions.map( toDB(_) ))
    } { closeResult =>
      executions
    } {
      e:Exception =>
        s"Could not close the ${executions.size} nodes executions, reason is ${e.getMessage()}"
    }
  }
}

object ExecutionRepositoryUtils {
  implicit def toTimeStamp(d:DateTime) : Timestamp = {
    new Timestamp(d.getMillis)
  }

  implicit def toDB (execution : ReportExecution)  : DBReportExecution = {
    DBReportExecution(execution.runId.nodeId.value, execution.runId.date, execution.nodeConfigVersion, execution.isCompleted)
  }

  implicit def fromDB (execution : DBReportExecution)  : ReportExecution = {
    ReportExecution(AgentRunId(NodeId(execution.nodeId), new DateTime(execution.date)), execution.nodeConfigVersion, execution.isCompleted)
  }
}


object Executions extends Schema {
  val executions = table[DBReportExecution]("reportsexecution")
}

case class DBReportExecution (
    @Column("nodeid")   nodeId     : String
  , @Column("date")     date       : Timestamp
  , @Column("nodeconfigversion") nodeConfigVersion : Option[String]
  , @Column("complete") isCompleted: Boolean
) extends KeyedEntity[CompositeKey2[String,Timestamp]] {

  def id = compositeKey(nodeId,date)
}
