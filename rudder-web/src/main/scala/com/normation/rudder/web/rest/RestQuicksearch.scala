package com.normation.rudder.web.rest

import net.liftweb.http.rest.RestHelper
import net.liftweb.common.Loggable
import com.normation.rudder.api.WoApiAccountRepository
import com.normation.rudder.api.RoApiAccountRepository
import com.normation.rudder.web.rest.RestUtils._
import net.liftweb.json.JsonDSL._
import net.liftweb.common._
import net.liftweb.json.JArray
import org.joda.time.DateTime
import com.normation.rudder.web.components.DateFormaterService
import com.normation.rudder.api._
import net.liftweb.http.LiftResponse
import com.normation.utils.StringUuidGenerator
import com.normation.eventlog.ModificationId
import com.normation.rudder.web.model.CurrentUser

class RestQuicksearch (

) extends RestHelper with Loggable {


  serve {
    case Get("secure" :: "api" :: "quicksearch" :: token :: Nil, req) =>
      readApi.getAll() match {
        case Full(accountSeq) =>
          val accounts = ("accounts" -> JArray(accountSeq.toList.map(toJson(_))))
          toJsonResponse(None,accounts)("getAllAccounts",true)
        case eb : EmptyBox =>
          logger.error(s"Could not get accounts cause : ${(eb ?~ "could not get account").msg}")
          toJsonError(None,s"Could not get accounts cause : ${(eb ?~ "could not get account").msg}")("getAllAccounts",true)

      }

    case Delete("secure" :: "apiaccounts" :: token :: Nil, req) =>
      val apiToken = ApiToken(token)
      readApi.getByToken(apiToken) match {
        case Full(Some(account)) =>
          writeApi.delete(account.id, ModificationId(uuidGen.newUuid), CurrentUser.getActor) match {
            case Full(_) =>
              val accounts = ("accounts" -> JArray(List(toJson(account))))
              toJsonResponse(None,accounts)("deleteAccount",true)

            case eb : EmptyBox =>
              toJsonError(None,s"Could not delete account with token $token cause : ${(eb ?~ "could not delete account").msg}")("deleteAccount",true)
          }

        case Full(None) =>
          toJsonError(None,s"Could not delete account with token $token cause : could not get account")("deleteAccount",true)
        case eb : EmptyBox =>
          toJsonError(None,s"Could not delete account with token $token cause : ${(eb ?~ "could not get account").msg}")("deleteAccount",true)
      }

  }

  def save(account:ApiAccount) : LiftResponse = {
    writeApi.save(account, ModificationId(uuidGen.newUuid), CurrentUser.getActor) match {
      case Full(res) =>
        val accounts = ("accounts" -> JArray(List(toJson(res))))
        toJsonResponse(None,accounts)("updateAccount",true)

      case eb : EmptyBox =>
        toJsonError(None, s"Could not update account '${account.name.value}' cause : ${(eb ?~ "could not save account").msg}")("updateAccount",true)
    }
  }

  def toJson(account : ApiAccount) = {
    ("id" -> account.id.value) ~
    ("name" -> account.name.value) ~
    ("token" -> account.token.value) ~
    ("tokenGenerationDate" -> DateFormaterService.getFormatedDate(account.tokenGenerationDate)) ~
    ("description" -> account.description) ~
    ("creationDate" -> DateFormaterService.getFormatedDate(account.creationDate)) ~
    ("enabled" -> account.isEnabled)
  }

}



