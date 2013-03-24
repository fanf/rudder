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
package com.normation.rudder.web.snippet.configuration

import net.liftweb.common._
import net.liftweb.http.DispatchSnippet
import net.liftweb.util._
import net.liftweb.util.Helpers._
import scala.xml._
import net.liftweb.http._
import net.liftweb.http.SHtml._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import bootstrap.liftweb.RudderConfig
import com.normation.rudder.domain.parameters.GlobalParameter
import com.normation.rudder.domain.parameters.ParameterName
import com.normation.eventlog.ModificationId
import org.joda.time.DateTime
import com.normation.rudder.web.model.CurrentUser
import com.normation.rudder.web.model.{
  WBTextField, FormTracker, WBTextAreaField, WBRadioField
}
import java.util.regex.Pattern
import net.liftweb.http.js.JE.JsRaw
import com.normation.rudder.web.components.GlobalParameterForm

class ParameterManagement extends DispatchSnippet with Loggable {

  private[this] val roParameterService = RudderConfig.roParameterService
 
  private[this] val gridName      = "globalParametersGrid"
  private[this] val gridContainer = "ParamGrid"
  private[this] val htmlId_form   = "globalParameterForm"
  
  //the current GlobalParameterForm component
  private[this] val parameterForm = new LocalSnippet[GlobalParameterForm]
    
  def dispatch = {
    case "display" => { _ =>  display }
  }

  def display() : NodeSeq = {
    roParameterService.getAllGlobalParameters match {
      case Full(seq) => displayGridParameters(seq, gridName)
      case Empty     => displayGridParameters(Seq(), gridName)
      case f:Failure =>
        <div class="error">Error when trying to get global Parameters</div>
    }
  }
  
  def displayGridParameters(params:Seq[GlobalParameter], gridName:String) : NodeSeq  = {
    (
      "tbody *" #> ("tr" #> params.map { param =>
        val lineHtmlId = Helpers.nextFuncName
        "tr[id]" #> lineHtmlId &
        ".name *" #> param.name.value &
        ".value *" #> param.value &
        ".description *" #> param.description &
        ".overridable *" #> param.overridable &
        ".change *" #> <div>{
                       ajaxButton("Update", () => setAndShowParameterForm(Some(param)), ("class", "smallButton")) ++
                       ajaxButton("Delete", () => showRemovePopupForm(param), ("class", "smallButton"))
                       }</div>        
      }) &
      ".createParameter *" #> ajaxButton("Create", () => setAndShowParameterForm(None))
     ).apply(dataTableXml(gridName)) ++ Script(initJs)
  }
  

  
  private[this] def dataTableXml(gridName:String) = {
    <div id={gridContainer}>
      <div id="actions_zone">
        <div class="createParameter"/>
      </div>
      <table id={gridName} class="display" cellspacing="0">
        <thead>
          <tr class="head">
            <th class="titleId">Name</th>
            <th>Value</th>
            <th>Description</th>
            <th>Change</th>
          </tr>
        </thead>

        <tbody>
          <tr class="parameterLine" id="lineId">
            <td class="name">[name of parameter]</td>
            <td class="value">[value of parameter]</td>
            <td class="description">[description]</td>
            <td class="change">[change / delete]</td>
          </tr>
        </tbody>
      </table>

      <div id="parametersGrid_paginate_area" class="paginate"></div>

    </div>

  }
  
  private[this] def jsVarNameForId(tableId:String) = "oTable" + tableId
  
  private[this] def initJs() : JsCmd = {
    OnLoad(
        JsRaw("""
          /* Event handler function */
          #table_var# = $('#%s').dataTable({
            "asStripeClasses": [ 'color1', 'color2' ],
            "bAutoWidth"   : false,
            "bFilter"      : true,
            "bPaginate"    : true,
            "bLengthChange": true,
            "sPaginationType": "full_numbers",
            "oLanguage": {
              "sZeroRecords": "No parameters!",
              "sSearch": ""
            },
            "bJQueryUI"    : true,
            "aaSorting"    : [[ 0, "asc" ]],
            "aoColumns": [
              { "sWidth": "180px" },
              { "sWidth": "180px" },
              { "sWidth": "300px" },
              { "sWidth": "150px" }
            ],
            "sDom": '<"dataTables_wrapper_top"fl>rt<"dataTables_wrapper_bottom"ip>'
          });
          $('.dataTables_filter input').attr("placeholder", "Search");
          """.format(gridName).replaceAll("#table_var#",jsVarNameForId(gridName))
        )
    )
  }

  private[this] def setAndShowParameterForm(parameter : Option[GlobalParameter]) : JsCmd = {
    val form = new GlobalParameterForm(htmlId_form, parameter, (String) => updateGrid() )
    parameterForm.set(Full(form))
    
    SetHtml(htmlId_form, form.dispatch("showForm")(NodeSeq.Empty)) & 
    JsRaw("""scrollToElement("%s");""".format(htmlId_form)) & 
    OnLoad(JsRaw("""correctButtons();"""))
  }

  /**
   * Display a popup to confirm  deletion of parameter
   */
  private[this] def showRemovePopupForm(parameter : GlobalParameter) : JsCmd = {
    val popupContent = (
       "#removeActionDialog *" #> { (n:NodeSeq) => SHtml.ajaxForm(n) } andThen
       "#dialogRemoveButton" #> { removeButton % ("id", "removeButton") } &
       ".reasonsFieldsetPopup" #> { crReasonsRemovePopup.map { f =>
         "#explanationMessage" #> <div>{userPropertyService.reasonsFieldExplanation}</div> &
         "#reasonsField" #> f.toForm_!
       } } &
        "#errorDisplay *" #> { updateAndDisplayNotifications(formTrackerRemovePopup) }
    )(popupRemoveForm)
    
    SetHtml("deletionPopup", popupContent)
  }
  
  private[this] def updateGrid() : JsCmd = {
    Replace(gridContainer, display()) & OnLoad(JsRaw("""correctButtons();"""))
  }
  
  
  

  private[this] def deletePopupXml(paramName : String) : NodeSeq = {
    <div id="removeActionDialog" class="nodisplay">
     <div class="simplemodal-title">
       <h1>Delete global Parameter {paramName}</h1>
       <hr/>
     </div>
     <div class="simplemodal-content">
       <div>
         <img src="/images/icWarn.png" alt="Warning!" height="32" width="32" class="warnicon"/>
         <h2>Are you sure that you want to delete this item?</h2>
       </div>
       <br />
        <div class="reasonsFieldsetPopup">
        <div id="explanationMessage">
          Here comes the explanation to reasons field
        </div>
        <div id="reasonsField">
          Here comes the reasons field
        </div>
      </div>
       <br />
       <hr class="spacer" />
     </div>
     <div class="simplemodal-bottom">
       <hr/>
       <div class="popupButton">
         <span>
           <button class="simplemodal-close" onClick="$.modal.close();">Cancel</button>
           <button id="dialogRemoveButton">Delete</button>
         </span>
       </div>
     </div>
   </div>
  }

}