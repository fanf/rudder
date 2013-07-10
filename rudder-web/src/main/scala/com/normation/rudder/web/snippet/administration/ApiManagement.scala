/*
*************************************************************************************
* Copyright 2011-2013 Normation SAS
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

package com.normation.rudder.web.snippet.administration


import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._
import JE._
import JsCmds._
import net.liftweb.util._
import Helpers._
import scala.xml.Text
import scala.xml.NodeSeq
import bootstrap.liftweb.RudderConfig
//import com.normation.rudder.api.ApiAccount


class ApiManagement extends DispatchSnippet with Loggable {


//  private[this] def roAccountRepos = RudderConfig.roApiAccountRepository
//  private[this] def woAccountRepos = RudderConfig.woApiAccountRepository


  override def dispatch = {
    // manage accounts
    case "accounts" => displayAccounts()
  }


  private[this] def displayAccounts() : CssSel = {

    ???

//    roAccountRepos.getAll match {
//      case eb:EmptyBox =>
//        val e = eb ?~! "Can not access to API Accounts"
//        logger.debug(e.messageChain)
//        e.rootExceptionCause.foreach(logger.debug(_))
//
//        <div class="error">{e.messageChain}</div>
//
//      case Full(accounts) =>
//        //the ajax form, n times
//        "name=displayAccount" #> ( (xml:NodeSeq) => accounts.map { case(id, a) => {
//
//           (
//             "name=displayAccount [id]" #> s"display${account.id.value}"
//           & "name=accountId" #> s"${account.id.value} ${if(account.isEnabled) "" else "[DISABLED]"}"
//           & "name=notification" #> NodeSeq.Empty
//           & "name=description" #> account.description, (x => account = account.copy(description = x)))
//           & "name=update" #> SHtml.ajaxSubmit("Update", save(account) _)
//           & "name=delete" #>
//           & "name=revoke" #>
//           & "name=regenerate" #>
//           )
//           (SHtml.ajaxForm(xml))
//        }})
//        )
//        &
//
//    }
//
//
//    "#foo" #> "plop"
//
//  }
//
//  private[this] def save(account: ApiAccount)() : JsCmd = {
//    this.woAccountRepos.save(account)
//
//    Replace
  }

}
