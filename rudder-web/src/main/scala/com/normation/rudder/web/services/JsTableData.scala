/*
*************************************************************************************
* Copyright 2014 Normation SAS
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

package com.normation.rudder.web.services

import net.liftweb.http.js.JsExp
import net.liftweb.http.js.JsObj
import scala.collection.TraversableLike
import net.liftweb.http.js.JE.JsArray
import com.normation.rudder.domain.reports.ComplianceLevel
import net.liftweb.http.js.JE


/*
 * That class represent a Line in a DataTable.
 */
trait JsTableLine {

  def json : JsObj

  //transform the compliance percent to a list with a given order:
  //pc_pending, pc_success, pc_repaired, pc_error, pc_noAnswer, pc_notApplicable
  def jsCompliance(compliance: ComplianceLevel) = JsArray(
      JE.Num(compliance.pc_pending)
    , JE.Num(compliance.pc_success)
    , JE.Num(compliance.pc_repaired)
    , JE.Num(compliance.pc_error)
    , JE.Num(compliance.pc_noAnswer)
    , JE.Num(compliance.pc_notApplicable)
  )
}

/*
 * That class a set of Data to use in datatable
 * It should be used as data parameter when creating datatable in javascript
 */
case class JsTableData[T <: JsTableLine] ( lines : List[T] ) {

  def json = JsArray(lines.map(_.json))

}

