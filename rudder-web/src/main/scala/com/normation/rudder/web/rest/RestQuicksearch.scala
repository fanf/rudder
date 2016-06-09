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


  final val MAX_RES_BY_KINd = 10

  serve {
    case Get("secure" :: "api" :: "quicksearch" :: token :: Nil, req) => {

      quicksearch.search(token) match {

        case eb: EmptyBox  =>
          val e = eb ?~! s"Error when looking for object containing ${token}"
          toJsonError(None, e.messageChain)("quicksearch", false)

        case Full(results) =>
          toJsonResponse(None, prepare(results, MAX_RES_BY_KINd))("quicksearch", false)
      }

    }

  }

  /**
   * A function that will prepare results to be transfered to the browser:
   * - split them by kind, so that we can add a summary by number for each
   * - in each kind, sort by natural order on names,
   * - for each kind, limit the number of element sent to the browser.
   *   The user will be able to make more precises search if needed, and we avoid
   *   crunching the browser with thousands of answers
   */
  private[this] def prepare(results: Seq[QuicksearchResult], maxByKind: Int): JValue = {

    // group by kind, and build the summary for each
    val map = results.groupBy( _.id.tpe ).map { case (tpe, seq) =>
      //distinct by id:
      val unique   = seq.map(x => (x.id, x) ).toMap.values.toSeq
      val returned = unique.take(maxByKind)

      val summary = ResultTypeSummary(tpe, unique.size, returned.size)

      (tpe, (summary, returned))
    }

    // now, transformed to the wanted results: an array.
    // hard coded order for elements:
    // - nodes
    // - groups
    // - directives
    // - parameters
    // - rules

    val jsonList = QuicksearchResultId.allTypes.flatMap { tpe =>
      val (summary, res) = map.getOrElse(tpe, (ResultTypeSummary(tpe, 0,0), Seq()) )
      summary.toJson :: res.toList.map( _.toJson )
    }

    JArray(jsonList)
  }

  private[this] final case class ResultTypeSummary(
      tpe            : String
    , originalNumber : Int
    , returnedNumber : Int
  )

  private[this] implicit class JsonResultTypeSummary(t: ResultTypeSummary) {

    val desc = if(t.originalNumber <= t.returnedNumber) {
       s"${t.originalNumber} found"
    } else { // we elided some results
       s"${t.originalNumber} found, only displaying the ${t.returnedNumber} firsts. You should try a more precise query"
    }

    def toJson(): JObject = {
      (
          ("type"    -> t.tpe.capitalize)
        ~ ("summary" -> desc            )
      )
    }
  }

  private[this] implicit class JsonSearchResult(r: QuicksearchResult) {
    import com.normation.inventory.domain.NodeId
    import com.normation.rudder.domain.policies.DirectiveId
    import com.normation.rudder.domain.policies.RuleId
    import com.normation.rudder.domain.nodes.NodeGroupId
    import com.normation.rudder.domain.parameters.ParameterName
    import com.normation.rudder.web.model.JsInitContextLinkUtil._
    import net.liftweb.http.S
    import com.normation.rudder.domain.RudderLDAPConstants._
    import com.normation.inventory.ldap.core.LDAPConstants._

    def toJson(): JObject = {
      def enc(s: String) = S.encodeURL(s).encJs

      val url = r.id match {
        case QRNodeId(v)      => nodeLink(NodeId(v))
        case QRRuleId(v)      => ruleLink(RuleId(v))
        case QRDirectiveId(v) => directiveLink(DirectiveId(v))
        case QRGroupId(v)     => groupLink(NodeGroupId(v))
        case QRParameterId(v) => globalParameterLink(ParameterName(v))
      }

      //some attribute with better name:
      val a = {
        r.attribute match {
          case A_NODE_UUID | A_DIRECTIVE_UUID | A_NODE_GROUP_UUID | A_RULE_UUID => "id"
          case A_NAME | A_PARAMETER_NAME=> "name"
          case A_NODE_PROPERTY => "property"
          case A_HOSTNAME => "hostname"
          case A_LIST_OF_IP => "ip"
          case A_SERVER_ROLE => "rudder role"
          case A_ARCH => "arch"
          case A_DIRECTIVE_VARIABLES => "parameter"
          case A_PARAMETER_VALUE => "value"
          case A_OS_NAME => "os name"
          case A_OS_FULL_NAME => "os"
          case A_OS_VERSION => "os version"
          case A_OS_SERVICE_PACK => "os service pack"
          case A_OS_KERNEL_VERSION => "os kernel version"
          case x => x
        }
      }

      //limit description length to avoid having a whole file printed
      val v = {
        val max = 30
        if(r.value.size > max+3) r.value.take(max) + "..."
        else                     r.value
      }

      val desc = s"${a}: ${v}"

      (
          ( "name" -> r.name     )
        ~ ( "type" -> r.id.tpe   )
        ~ ( "id"   -> r.id.value )
        ~ ( "desc" -> desc       )
        ~ ( "url"  -> url        )
      )
    }
  }


}



