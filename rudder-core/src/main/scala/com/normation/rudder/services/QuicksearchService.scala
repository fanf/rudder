/*
*************************************************************************************
* Copyright 2011 Normation SAS
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

package com.normation.rudder.services

import com.normation.inventory.domain._
import org.joda.time.DateTime
import com.normation.ldap.sdk.LDAPConnectionProvider
import com.normation.inventory.ldap.core.InventoryDit
import com.normation.rudder.domain.RudderDit
import com.normation.rudder.domain.NodeDit
import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.normation.rudder.domain.nodes.{NodeInfo, Node}
import com.normation.rudder.domain.RudderLDAPConstants._
import com.normation.inventory.ldap.core.LDAPConstants._
import com.normation.rudder.domain.Constants._
import com.unboundid.ldap.sdk._
import com.normation.ldap.sdk._
import BuildFilter._
import com.normation.rudder.repository.ldap.LDAPEntityMapper
import com.normation.utils.Control._
import com.normation.inventory.ldap.core.InventoryMapper
import com.normation.inventory.ldap.core.InventoryDitService
import com.normation.rudder.domain.logger.TimingDebugLogger
import com.normation.rudder.repository.CachedRepository
import com.normation.inventory.ldap.core.InventoryDit
import com.normation.inventory.ldap.core.InventoryMapper
import com.normation.inventory.ldap.core.LDAPConstants
import com.normation.rudder.domain.nodes.MachineInfo
import java.util.regex.Pattern
import QuicksearchService.QuicksearchResult
import com.normation.rudder.services.QuicksearchService.QRNodeId
import com.normation.rudder.services.QuicksearchService.QuicksearchResultId
import com.normation.rudder.services.QuicksearchService.QRRuleId
import com.normation.rudder.services.QuicksearchService.QRDirectiveId
import com.normation.rudder.services.QuicksearchService.QRGroupId
import com.normation.rudder.services.QuicksearchService.QRParameterId

/**
 * This class allow to return a list of Rudder object given a string.
 * The string is search in some attribute
 * (node name, hostname, uuid, directive attribute, etc).
 * The service always return both the matching attribute and the
 * object type and UUID
 */
class QuicksearchService(
    ldapConnection : LDAPConnectionProvider[RoLDAPConnection]
  , nodeDit        : NodeDit
  , inventoryDit   : InventoryDit
  , rudderDit      : RudderDit
) {



  /*
   * We need to list actual attributes, because we don't want software ids.
   * Really bad idea, these software ids, in node...
   */
  val attributes: Seq[String] = (Set(
    // node
    //  id, name, description, isBroken, isSystem
    //, isPolicyServer <- this one is special and decided based on objectClasss rudderPolicyServer
    //, creationDate, nodeReportingConfiguration, properties
      A_NODE_UUID, A_DESCRIPTION
    , A_NODE_PROPERTY

    //node inventory, safe machine and node (above)
    //  hostname, ips, inventoryDate, publicKey
    //, osDetails
    //, agentsName, policyServerId, localAdministratorAccountName
    //, serverRoles, archDescription, ram
    , A_HOSTNAME, A_LIST_OF_IP
    , A_OS_NAME, A_OS_FULL_NAME, A_OS_VERSION, A_OS_KERNEL_VERSION, A_OS_SERVICE_PACK, A_WIN_USER_DOMAIN, A_WIN_COMPANY, A_WIN_KEY, A_WIN_ID
    , A_AGENTS_NAME, A_POLICY_SERVER_UUID, A_ROOT_USER
    , A_SERVER_ROLE, A_ARCH

    //directive
    // id, name, description, attribute
    , A_DIRECTIVE_UUID, A_DIRECTIVE_VARIABLES

    // groups
    , A_NODE_GROUP_UUID

    // rules
    , A_RULE_UUID

    // parameters
    , A_PARAMETER_NAME, A_PARAMETER_VALUE

  )).toSeq



  /**
   * Search for all entries that match on one of the wanted attribute the substring given in
   * argument.
   */
  private[this] def getEntries(con: RoLDAPConnection, token: String): Seq[LDAPEntry] = {

    val filter = AND(
        OR(Seq(
          AND(IS(OC_NODE)             , Filter.create(s"entryDN:dnOneLevelMatch:=${ inventoryDit.NODES.dn.toString             }"))
        , AND(IS(OC_RUDDER_NODE)      , Filter.create(s"entryDN:dnOneLevelMatch:=${ nodeDit.NODES.dn.toString                  }"))
        , AND(IS(OC_RULE)             , Filter.create(s"entryDN:dnSubtreeMatch:=${  rudderDit.RULES.dn.toString                }"))
        , AND(IS(OC_DIRECTIVE)        , Filter.create(s"entryDN:dnSubtreeMatch:=${  rudderDit.ACTIVE_TECHNIQUES_LIB.dn.toString}"))
        , AND(IS(OC_RUDDER_NODE_GROUP), Filter.create(s"entryDN:dnSubtreeMatch:=${  rudderDit.GROUP.dn.toString                }"))
        , AND(IS(OC_PARAMETER        ), Filter.create(s"entryDN:dnOneLevelMatch:=${ rudderDit.PARAMETERS.dn.toString           }"))
        ):_*)
    ,   OR(attributes.map(a => MATCH(a, token)):_*)
    )

    con.search(nodeDit.BASE_DN, Sub, filter, (attributes :+ A_OC):_*)
  }


  /**
   * correctly transform entry to a result, putting what is needed in type and description
   */
  private[this] def toSearchResult(pattern: Pattern)(e: LDAPEntry): Option[QuicksearchResult] = {
    def getId(e: LDAPEntry): Option[QuicksearchResultId] = {
      if       (e.isA(OC_NODE             )) { e(A_NODE_UUID).map( QRNodeId )
      } else if(e.isA(OC_RUDDER_NODE      )) { e(A_NODE_UUID).map( QRRuleId )
      } else if(e.isA(OC_RULE             )) { e(A_NODE_UUID).map( QRRuleId )
      } else if(e.isA(OC_DIRECTIVE        )) { e(A_NODE_UUID).map( QRDirectiveId )
      } else if(e.isA(OC_RUDDER_NODE_GROUP)) { e(A_NODE_UUID).map( QRGroupId )
      } else if(e.isA(OC_PARAMETER        )) { e(A_NODE_UUID).map( QRParameterId )
      } else { None
      }
    }


    // get the attribute value matching patters
    // if several, take only one at random. If none, that's strange, reject entry
    // also, don't look in objectClass to find the pattern
    for {
      desc <- e.attributes.flatMap(a => if(a.getName == OC) Seq() else a.getValues).find( v => pattern.matcher(v).matches )
      id   <- getId(e)
    } yield {
      QuicksearchResult(id, e(A_NAME).getOrElse(id.value), desc)
    }
  }

  def search(token: String): Box[Seq[QuicksearchResult]] = {
    for {
      ldap <- ldapConnection
    } yield {
      val matches = toSearchResult(s"""?i.*${token}.*""".r.pattern) _
      val results = getEntries(ldap, token).flatMap(matches(_))
      println(s" ===> Found ${results.size} entries matchings search")
      //we can have duplicates ids, removes them
      results.map(r => (r.id, r)).toMap.values.toSeq
    }
  }
}

object QuicksearchService {

  // we defined a set of id type to be able to process specifically
  // these kinds in result

  sealed trait QuicksearchResultId { def value: String }

  final case class QRNodeId      (value: String) extends QuicksearchResultId
  final case class QRGroupId     (value: String) extends QuicksearchResultId
  final case class QRDirectiveId (value: String) extends QuicksearchResultId
  final case class QRRuleId      (value: String) extends QuicksearchResultId
  final case class QRParameterId (value: String) extends QuicksearchResultId


  final case class QuicksearchResult(
      id         : QuicksearchResultId // the uuid used to build url
    , name       : String              // the user facing name
    , description: String              // the part that matches the search

  )

}

