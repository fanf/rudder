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

package com.normation.rudder.services.servers

import net.liftweb.common.Box
import com.normation.inventory.domain.NodeId
import com.normation.rudder.repository.RoDirectiveRepository
import com.normation.rudder.repository.WoDirectiveRepository
import com.normation.rudder.domain.Constants
import net.liftweb.common.Loggable
import com.normation.eventlog.EventActor
import com.normation.eventlog.ModificationId
import sun.net.util.IPAddressUtil
import ca.mrvisser.sealerate
import com.normation.ldap.sdk.LDAPConnectionProvider
import com.normation.ldap.sdk.RwLDAPConnection
import com.normation.rudder.domain.RudderDit
import com.normation.rudder.domain.logger.ApplicationLogger
import com.unboundid.ldap.sdk.DN
import net.liftweb.common.Failure
import net.liftweb.common.Full
import com.normation.box._
import com.normation.rudder.domain.policies.DirectiveRId

/**
 * This service allows to manage properties linked to the root policy server,
 * like authorized network, policy server hostname, etc.
 *
 * For now, the hypothesis that there is one and only one policy server is made.
 */
trait PolicyServerManagementService {

  /**
   * Get the list of authorized networks, i.e the list of networks such that
   * a node with an IP in it can ask for updated policies.
   */
  def getAuthorizedNetworks(policyServerId:NodeId) : Box[Seq[String]]

  /**
   * Update the list of authorized networks with the given list
   */
  def setAuthorizedNetworks(policyServerId:NodeId, networks:Seq[String], modId: ModificationId, actor:EventActor) : Box[Seq[String]]

  /**
   * Delete things related to a relay (groups, rules, directives)
   */
  def deleteRelaySystemObjects(policyServerId: NodeId): Box[Unit]

}

object PolicyServerManagementService {
  /**
   * Check if the given string is a network address,
   * i.e if it on the form IP(v4 or v6)/mask.
   * A single IP address will be accepted by the test.
   */
  def isValidNetwork(net:String) = {

    val parts = net.split("/")
    if(parts.size == 1) {
       IPAddressUtil.isIPv6LiteralAddress(parts(0)) ||
       IPAddressUtil.isIPv4LiteralAddress(parts(0))
    } else if(parts.size == 2) {
      (
       IPAddressUtil.isIPv6LiteralAddress(parts(0)) ||
       IPAddressUtil.isIPv4LiteralAddress(parts(0))
      ) && (
        try {
          val n = parts(1).toInt
          if(n >= 0 && n < 255) true else false
        } catch {
          case _:NumberFormatException => false
        }
      )
    } else false
  }
}

class PolicyServerManagementServiceImpl(
    roDirectiveRepository: RoDirectiveRepository
  , woDirectiveRepository: WoDirectiveRepository
  , ldap                 : LDAPConnectionProvider[RwLDAPConnection]
  , dit                  : RudderDit
) extends PolicyServerManagementService with Loggable {

  /**
   * The list of authorized network is not directly stored in an
   * entry. We have to look for the DistributePolicy directive for
   * that server and the rudderPolicyVariables: ALLOWEDNETWORK
   */
  override def getAuthorizedNetworks(policyServerId:NodeId) : Box[Seq[String]] = {
    for {
      directive <- roDirectiveRepository.getDirective(Constants.buildCommonDirectiveId(policyServerId))
    } yield {
      val allowedNetworks = directive.toList.flatMap(_.parameters.getOrElse(Constants.V_ALLOWED_NETWORK, List()))
      allowedNetworks.toList
    }
  }.toBox

  override def setAuthorizedNetworks(policyServerId:NodeId, networks:Seq[String], modId: ModificationId, actor:EventActor) : Box[Seq[String]] = {

    val directiveId = Constants.buildCommonDirectiveId(policyServerId)

    //filter out bad networks
    val validNets = networks.flatMap { case net =>
      if(PolicyServerManagementService.isValidNetwork(net)) Some(net)
      else {
        logger.error("Ignoring allowed network '%s' because it does not seem to be a valid network")
        None
      }
    }

    for {
      res <- roDirectiveRepository.getActiveTechniqueAndDirective(DirectiveRId(directiveId)).notOptional(s"Error when retrieving system directive with ID ${directiveId.value}' which is mandatory for allowed networks configuration.")
      (activeTechnique, directive) = res
      newPi = directive.copy(parameters = directive.parameters + (Constants.V_ALLOWED_NETWORK -> validNets.map( _.toString)))
      msg = Some("Automatic update of system directive due to modification of accepted networks ")
      saved <- woDirectiveRepository.saveSystemDirective(activeTechnique.id, newPi, modId, actor, msg).chainError(s"Can not save directive for Active Technique '${activeTechnique.id.value}")
    } yield {
      networks
    }
  }.toBox

  /**
   * Delete things related to a relay:
   * - group: nodeGroupId=hasPolicyServer-${uuid},groupCategoryId=SystemGroups,groupCategoryId=GroupRoot,ou=Rudder,cn=rudder-configuration
   * - rule target:  ruleTarget=policyServer:${RELAY_UUID},groupCategoryId=SystemGroups,groupCategoryId=GroupRoot,ou=Rudder,cn=rudder-configuration
   * - directive:  directiveId=${RELAY_UUID}-distributePolicy,activeTechniqueId=distributePolicy,techniqueCategoryId=Rudder Internal,techniqueCategoryId=Active Techniques,ou=Rudder,cn=rudder-configuration
   * - directive: directiveId=common-${RELAY_UUID},activeTechniqueId=common,techniqueCategoryId=Rudder Internal,techniqueCategoryId=Active Techniques,ou=Rudder,cn=rudder-configuratio
   * - rule: ruleId=${RELAY_UUID}-DP,ou=Rules,ou=Rudder,cn=rudder-configuration
   * - rule: ruleId=hasPolicyServer-${RELAY_UUID},ou=Rules,ou=Rudder,cn=rudder-configuration
   */
  def deleteRelaySystemObjects(policyServerId: NodeId): Box[Unit] = {
    if(policyServerId == Constants.ROOT_POLICY_SERVER_ID) {
      Failure("Root server configuration elements can't be deleted")
    } else { // we don't have specific validation to do: if the node is not a policy server, nothing will be done
      def DN(child: String, parentDN: DN) = new DN(child+","+parentDN.toString)
      val id = policyServerId.value

// nodeGroupId=hasPolicyServer-b887aee5-f191-45cd-bb2d-9e3f5b30d06e,groupCategoryId=SystemGroups,groupCategoryId=GroupRoot,ou=Rudder,cn=rudder-configuration

      for {
        con <- ldap
        _   <- con.delete(DN(s"nodeGroupId=hasPolicyServer-${id}", dit.GROUP.SYSTEM.dn))
        _   <- con.delete(DN(s"ruleTarget=policyServer:${id}", dit.GROUP.SYSTEM.dn))
        _   <- con.delete(DN(s"directiveId=${id}-distributePolicy,activeTechniqueId=distributePolicy,techniqueCategoryId=Rudder Internal", dit.ACTIVE_TECHNIQUES_LIB.dn))
        _   <- con.delete(DN(s"directiveId=common-${id},activeTechniqueId=common,techniqueCategoryId=Rudder Internal", dit.ACTIVE_TECHNIQUES_LIB.dn))
        _   <- con.delete(DN(s"ruleId=${id}-DP", dit.RULES.dn))
        _   <- con.delete(DN(s"ruleId=hasPolicyServer-${id}", dit.RULES.dn))
        _   =  ApplicationLogger.info(s"System configuration object (rules, directives, groups) related to relay '${id}' were successfully deleted.")
      } yield ()
    }.toBox
  }
}

sealed trait RelaySynchronizationMethod { def value: String }
object RelaySynchronizationMethod {

  final case object Classic extends RelaySynchronizationMethod  { val value = "classic"  }

  final case object Rsync extends RelaySynchronizationMethod    { val value = "rsync"    }

  final case object Disabled extends RelaySynchronizationMethod { val value = "disabled" }

  final val all: Set[RelaySynchronizationMethod] = sealerate.values[RelaySynchronizationMethod]

  def parse(value: String): Box[RelaySynchronizationMethod] = {
    value match {
            case null|"" => Failure("An empty or null string can not be parsed as a relay synchronization method")
            case s => s.trim.toLowerCase match {
              case Classic.value  => Full(Classic)
              case Disabled.value => Full(Disabled)
              case Rsync.value    => Full(Rsync)
              case _              => Failure(s"Cannot parse the given value as a valid relay synchronization method: '${value}'. Authorised values are: '${all.map( _.value).mkString(", ")}'")
            }
          }
  }
}
