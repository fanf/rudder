package com.normation.rudder.web.rest

import com.normation.rudder.services.QuicksearchService
import com.normation.rudder.services.QuicksearchService._
import com.normation.rudder.web.rest.RestUtils._

import net.liftweb.common._
import net.liftweb.http.JsonResponse
import net.liftweb.http.js._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsExp

import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JArray
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._

class RestQuicksearch (
  quicksearch: QuicksearchService
) extends RestHelper with Loggable {


  serve {
    case Get("secure" :: "api" :: "quicksearch" :: token :: Nil, req) => {

      val error: JsExp = "error"
      def toJs(results: List[QuicksearchResult]): JValue = JArray(results.map(_.toJson))

      quicksearch.search(token) match {
        case eb: EmptyBox  => JsonResponse(error               , Nil, Nil, RestError.code)
        case Full(results) => toJsonResponse(None, toJs(results.toList))("quicksearch", false)
      }

    }

  }

  private implicit class JsonSearchResult(r: QuicksearchResult) {

    def toJson(): JObject = (
        ( "name" -> r.name        )
      ~ ( "desc" -> r.description )
      ~ ( "id  " -> r.id.value    )
      ~ ( "type" -> ( r.id match {
                      case _: QRNodeId      => "node"
                      case _: QRRuleId      => "rule"
                      case _: QRDirectiveId => "directive"
                      case _: QRGroupId     => "group"
                      case _: QRParameterId => "parameter"
                    })
        )
    )
  }


}



