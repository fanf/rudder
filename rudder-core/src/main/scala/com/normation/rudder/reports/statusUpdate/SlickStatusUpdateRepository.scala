/*
*************************************************************************************
* Copyright 2013 Normation SAS
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

package com.normation.rudder.reports.statusUpdate

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{ Failure => TFailure }
import scala.util.{ Success => TSuccess }

import com.normation.rudder.db.DB
import com.normation.rudder.db.SlickSchema

import org.joda.time.DateTime



import net.liftweb.common._

class SlickStatusUpdateRepository (
    slickSchema: SlickSchema
) extends StatusUpdateRepository with Loggable {
  import slickSchema.api._

  val PROP_EXECUTION_STATUS = "executionStatus"


  private[this] val queryGet = Compiled(slickSchema.statusUpdates
                                            .filter( _.key === PROP_EXECUTION_STATUS)
                                            .map(x => (x.lastId, x.date))
                                          )
  private[this] val actionGet = queryGet.result.headOption

  def getExecutionStatus : Future[Box[Option[(Long,DateTime)]]] = {

    slickSchema.db.run(actionGet.asTry).map {
      case TSuccess(x)  => Full(x)
      case TFailure(ex) => Failure(s"Error when retrieving '${PROP_EXECUTION_STATUS}' from db: ${ex.getMessage}", Full(ex), Empty)
    }
  }

  def setExecutionStatus(newId : Long, reportsDate : DateTime) : Future[Box[DB.StatusUpdate]] = {
    def add = DB.StatusUpdate(PROP_EXECUTION_STATUS, newId, reportsDate)

    val action = for {
      entry  <- actionGet.asTry
      result <- entry match {
                  case TSuccess(None) =>
                    (slickSchema.statusUpdates += add).asTry.map( _ => add)
                  case TSuccess(Some(e)) =>
                    queryGet.update((newId, reportsDate)).asTry.map( _ => add)
                  case TFailure(ex) =>
                    DBIO.failed(ex)
                }
    } yield {
      result
    }

    slickSchema.db.run(action.asTry).map {
      case TSuccess(x)  => Full(x)
      case TFailure(ex) => Failure(s"Error when retrieving '${PROP_EXECUTION_STATUS}' from db: ${ex.getMessage}", Full(ex), Empty)
    }
  }

}
