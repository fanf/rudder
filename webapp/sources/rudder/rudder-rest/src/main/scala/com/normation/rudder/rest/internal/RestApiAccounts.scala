package com.normation.rudder.rest

import com.normation.eventlog.ModificationId
import com.normation.rudder.api.*
import com.normation.rudder.api.ApiAuthorization as ApiAuthz
import com.normation.rudder.api.RoApiAccountRepository
import com.normation.rudder.api.WoApiAccountRepository
import com.normation.rudder.apidata.ApiAccountSerialisation.*
import com.normation.rudder.rest.RestUtils.*
import com.normation.rudder.users.UserService
import com.normation.utils.StringUuidGenerator
import com.normation.zio.*
import net.liftweb.common.*
import net.liftweb.common.Loggable
import net.liftweb.http.LiftResponse
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JArray
import net.liftweb.json.JsonDSL.*
import org.joda.time.DateTime

class RestApiAccounts(
    readApi:        RoApiAccountRepository,
    writeApi:       WoApiAccountRepository,
    restExtractor:  RestExtractorService,
    tokenGenerator: TokenGenerator,
    uuidGen:        StringUuidGenerator,
    userService:    UserService,
    apiAuthService: ApiAuthorizationLevelService
) extends RestHelper with Loggable {

  val tokenSize = 32

  // used in ApiAccounts snippet to get the context path
  // of that service
  val relativePath: List[String] = "secure" :: "apiaccounts" :: Nil

  serve {
    case Get("secure" :: "apiaccounts" :: Nil, req) =>
      implicit val prettify = restExtractor
        .extractBoolean("prettify")(req)(identity)
        .getOrElse(Some(false))
        .getOrElse(
          false
        )
      implicit val action: String = "getAllAccounts"

      // here, 'write' and not 'read' because some tokens may have admin write access,
      // and we want to avoid escalation
      OldInternalApiAuthz.withWriteAdmin(readApi.getAllStandardAccounts.either.runNow match {
        case Right(accountSeq) =>
          val accounts = {
            (
              ("aclPluginEnabled" -> apiAuthService.aclEnabled) ~
              ("accounts"         -> JArray(accountSeq.toList.map(_.toJson)))
            )
          }
          toJsonResponse(None, accounts)
        case Left(err)         =>
          val msg = s"Could not get accounts cause : ${err.fullMsg}"
          logger.error(msg)
          toJsonError(None, msg)

      })

    case "secure" :: "apiaccounts" :: Nil JsonPut body -> req =>
      implicit val prettify = restExtractor
        .extractBoolean("prettify")(req)(identity)
        .getOrElse(Some(false))
        .getOrElse(
          false
        )
      implicit val action: String = "updateAccount"

      OldInternalApiAuthz.withWriteAdmin(req.json match {
        case Full(json) =>
          restExtractor.extractApiAccountFromJSON(json) match {
            case Full(restApiAccount) =>
              if (restApiAccount.name.isDefined) {
                // generate the id for creation
                val id         = ApiAccountId(uuidGen.newUuid)
                val now        = DateTime.now
                // by default, token expires after one month
                val expiration = restApiAccount.expiration.getOrElse(Some(now.plusMonths(1)))
                val acl        = restApiAccount.authz.getOrElse(ApiAuthz.None)

                val account = ApiAccount(
                  id,
                  ApiAccountKind.PublicApi(acl, expiration),
                  restApiAccount.name.get,
                  ApiToken(tokenGenerator.newToken(tokenSize)),
                  restApiAccount.description.getOrElse(""),
                  restApiAccount.enabled.getOrElse(true),
                  now,
                  now
                )
                writeApi.save(account, ModificationId(uuidGen.newUuid), userService.getCurrentUser.actor).either.runNow match {
                  case Right(_) =>
                    val accounts = ("accounts" -> JArray(List(account.toJson)))
                    toJsonResponse(None, accounts)

                  case Left(err) =>
                    val msg = s"Could not create account cause : ${err.fullMsg}"
                    logger.error(msg)
                    toJsonError(None, msg)
                }
              } else {
                val msg = s"Could not create account cause : could not get account"
                logger.error(msg)
                toJsonError(None, msg)
              }

            case eb: EmptyBox =>
              val msg = s"Could not create account cause : ${(eb ?~ "could not extract data from JSON").msg}"
              logger.error(msg)
              toJsonError(None, msg)
          }
        case eb: EmptyBox =>
          logger.error("No Json data sent")
          toJsonError(None, "No Json data sent")
      })

    case "secure" :: "apiaccounts" :: tokenId :: Nil JsonPost body -> req =>
      val apiTokenId        = ApiAccountId(tokenId)
      implicit val prettify = restExtractor
        .extractBoolean("prettify")(req)(identity)
        .getOrElse(Some(false))
        .getOrElse(
          false
        )
      implicit val action: String = "updateAccount"

      OldInternalApiAuthz.withWriteAdmin(req.json match {
        case Full(json) =>
          restExtractor.extractApiAccountFromJSON(json) match {
            case Full(restApiAccount) =>
              readApi.getById(apiTokenId).either.runNow match {
                case Right(Some(account)) =>
                  val updateAccount = restApiAccount.update(account)
                  save(updateAccount)

                case Right(None) =>
                  val msg = s"Could not update account ${tokenId} cause : could not get account"
                  logger.error(msg)
                  toJsonError(None, msg)
                case Left(err)   =>
                  val msg = s"Could not update account ${tokenId} cause : ${err.fullMsg}"
                  logger.error(msg)
                  toJsonError(None, msg)
              }
            case eb: EmptyBox =>
              val msg = s"Could not update account ${tokenId} cause : ${(eb ?~ "could not extract data from JSON").msg}"
              logger.error(msg)
              toJsonError(None, msg)
          }
        case eb: EmptyBox =>
          toJsonError(None, "No Json data sent")
      })

    case Delete("secure" :: "apiaccounts" :: tokenId :: Nil, req) =>
      val apiTokenId        = ApiAccountId(tokenId)
      implicit val prettify = restExtractor
        .extractBoolean("prettify")(req)(identity)
        .getOrElse(Some(false))
        .getOrElse(
          false
        )
      implicit val action: String = "deleteAccount"

      OldInternalApiAuthz.withWriteAdmin(readApi.getById(apiTokenId).either.runNow match {
        case Right(Some(account)) =>
          writeApi.delete(account.id, ModificationId(uuidGen.newUuid), userService.getCurrentUser.actor).either.runNow match {
            case Right(_) =>
              val accounts = ("accounts" -> JArray(List(account.toJson)))
              toJsonResponse(None, accounts)

            case Left(err) =>
              toJsonError(None, s"Could not delete account ${tokenId} cause : ${err.fullMsg}")
          }

        case Right(None) =>
          toJsonError(None, s"Could not delete account ${tokenId} cause : could not get account")
        case Left(err)   =>
          toJsonError(None, s"Could not delete account ${tokenId} cause : ${err.fullMsg}")
      })

    case Post("secure" :: "apiaccounts" :: tokenId :: "regenerate" :: Nil, req) =>
      val apiTokenId        = ApiAccountId(tokenId)
      implicit val prettify = restExtractor
        .extractBoolean("prettify")(req)(identity)
        .getOrElse(Some(false))
        .getOrElse(
          false
        )
      implicit val action: String = "regenerateAccount"

      OldInternalApiAuthz.withWriteAdmin(readApi.getById(apiTokenId).either.runNow match {
        case Right(Some(account)) =>
          val newToken       = ApiToken(tokenGenerator.newToken(tokenSize))
          val generationDate = DateTime.now
          writeApi
            .save(
              account.copy(token = newToken, tokenGenerationDate = generationDate),
              ModificationId(uuidGen.newUuid),
              userService.getCurrentUser.actor
            )
            .either
            .runNow match {
            case Right(account) =>
              val accounts = ("accounts" -> JArray(List(account.toJson)))
              toJsonResponse(None, accounts)

            case Left(err) =>
              val msg = s"Could not regenerate account ${tokenId} cause : ${err.fullMsg}"
              logger.error(msg)
              toJsonError(None, s"Could not regenerate account ${tokenId} cause : ${err.fullMsg}")(
                "regenerateAccount",
                true
              )
          }

        case Right(None) =>
          val msg = s"Could not regenerate account ${tokenId} cause could not get account"
          logger.error(msg)
          toJsonError(None, msg)
        case Left(err)   =>
          val msg = s"Could not regenerate account ${tokenId} cause : ${err.fullMsg}"
          logger.error(msg)
          toJsonError(None, msg)
      })

  }

  def save(account: ApiAccount)(implicit action: String, prettify: Boolean): LiftResponse = {
    writeApi.save(account, ModificationId(uuidGen.newUuid), userService.getCurrentUser.actor).either.runNow match {
      case Right(res) =>
        val accounts = ("accounts" -> JArray(List(res.toJson)))
        toJsonResponse(None, accounts)

      case Left(err) =>
        toJsonError(None, s"Could not update account '${account.name.value}' cause : ${err.fullMsg}")
    }
  }

}

final case class RestApiAccount(
    id:          Option[ApiAccountId],
    name:        Option[ApiAccountName],
    description: Option[String],
    enabled:     Option[Boolean],
    oldId:       Option[ApiAccountId],
    expiration:  Option[Option[DateTime]],
    authz:       Option[ApiAuthz]
) {

  // Id cannot change if already defined
  def update(account: ApiAccount): ApiAccount = {
    val nameUpdate   = name.getOrElse(account.name)
    val enableUpdate = enabled.getOrElse(account.isEnabled)
    val descUpdate   = description.getOrElse(account.description)
    val kind         = account.kind match {
      case ApiAccountKind.PublicApi(a, e) =>
        ApiAccountKind.PublicApi(authz.getOrElse(a), expiration.getOrElse(e))
      case x                              => x
    }

    account.copy(name = nameUpdate, isEnabled = enableUpdate, description = descUpdate, kind = kind)
  }
}
