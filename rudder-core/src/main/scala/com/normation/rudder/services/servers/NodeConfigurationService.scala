/*
*************************************************************************************
* Copyright 2011 Normation SAS
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

package com.normation.rudder.services.servers

import com.normation.inventory.domain.AgentType
import com.normation.rudder.domain.servers._
import net.liftweb.common.Box
import com.normation.cfclerk.domain.{Cf3PolicyDraftId,TechniqueId}
import com.normation.rudder.services.policies.TargetNodeConfiguration
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.{Rule,RuleId}
import com.normation.rudder.services.policies.TargetNodeConfiguration

trait NodeConfigurationService {

  /**
   * Get all NodeConfigurations
   */
  //def getAllNodeConfigurations() : Box[Map[NodeId, NodeConfiguration]]

  /**
   * Update a node configuration using a targetNodeConfiguration :
   * update the directives and the node context, as well as the agentsName
   * (well, every fields actually)
   * @param target
   * @return
   */
  def updateNodeConfiguration(target : TargetNodeConfiguration, allNodeConfiguration: Map[NodeId, TargetNodeConfiguration]) : Box[Map[NodeId, TargetNodeConfiguration]]


  /**
   * Delete a list of node configurations
   * If a NodeConfiguration is not found, ignore it.
   */
  def deleteNodeConfigurations(nodeIds:Set[NodeId]) : Box[Set[NodeId]]

  /**
   * Delete all node configurations
   */
  def deleteAllNodeConfigurations() : Box[Set[NodeId]]

  /**
   * Write the templates of the updated NodeConfigurations.
   * That method will write the promises for updated nodes
   * and save corresponding node configuration *only*.
   * If there is no modification between current and target state,
   * nothing is done for that node configuration.
   *
   * Return the list of updated node configuration (and so nodes
   * for which promises where written).
   */
  def writeTemplateForUpdatedNodeConfigurations(rootNodeId: NodeId,  nodesToUpdate: Set[NodeId], allNodeConfigs: Map[NodeId, TargetNodeConfiguration]) : Box[Seq[TargetNodeConfiguration]] = ???


  /**
   * A method that normalize directives on a node configuration.
   * It should alway been called before writing promises.
   */
  def normalize(nodeConfig: TargetNodeConfiguration): Box[TargetNodeConfiguration] = {
    private def deduplicateUniqueDirectives(directives: Seq[RuleWithCf3PolicyDraft]) : Seq[RuleWithCf3PolicyDraft] = {
      val resultingDirectives = ArrayBuffer[RuleWithCf3PolicyDraft]();

      for (directiveToAdd <- directives.sortBy(x => x.cf3PolicyDraft.priority)) {
        //Prior to add it, must check that it is not unique and not already present
        val technique = directiveToAdd.cf3PolicyDraft.technique
        if (technique.isMultiInstance)
           resultingDirectives += directiveToAdd
        else {
          // if it is unique, add it only a same one is not already there
          if (resultingDirectives.filter(x => x.cf3PolicyDraft.technique.id == directiveToAdd.cf3PolicyDraft.technique.id).size == 0)
             resultingDirectives += directiveToAdd
          else
             logger.warn("Ignoring less prioritized unique directive %s ".format(directiveToAdd))
        }
      }
      resultingDirectives
    }

    /**
     * Adding a directive to a node, without saving anything
     * (this is not hyper sexy)
     */
    private def addDirectives(node:NodeConfiguration, directives :  Seq[RuleWithCf3PolicyDraft]) : Box[NodeConfiguration] = {

      var modifiedNode = node

      for (directive <- directives) {
          // check the legit character of the policy
          if (modifiedNode.targetRulePolicyDrafts.find( _.draftId == directive.draftId) != None) {
            logger.warn(s"Cannot add a directive with the same id than an already existing one ${directive.draftId}")
            return ParamFailure[RuleWithCf3PolicyDraft](
                "Duplicate directive",
                Full(new TechniqueException("Duplicate directive " + directive.draftId)),
                Empty,
                directive)
          }


          val technique = directive.cf3PolicyDraft.technique

          // Check that the directive can be multiinstances
          // to check that, either make sure that it is multiinstance, or that it is not
          // multiinstance and that there are no existing directives based on it
          if (modifiedNode.findDirectiveByTechnique(directive.cf3PolicyDraft.technique.id).filter(x => technique.isMultiInstance==false).size>0) {
            logger.warn(s"Cannot add a directive from the same non duplicable technique ${directive.cf3PolicyDraft.technique.id} than an already existing one")
            return ParamFailure[RuleWithCf3PolicyDraft]("Duplicate unique technique", Full(new TechniqueException("Duplicate unique policy " +directive.cf3PolicyDraft.technique.id)), Empty, directive)
          }
          modifiedNode.addDirective(directive) match {
            case Full(updatedNode : NodeConfiguration) =>
              modifiedNode = updatedNode
            case f:EmptyBox => return f
          }
      }
      Full(modifiedNode)
    }


    val directives = deduplicateUniqueDirectives(nodeConfig.identifiableCFCPIs.values)

  }

  ///// pure methods /////

  /**
   * Find the NodeConfigurations having the policy name listed (it's policy name, not instance).
   * We are looking in TARGET rule policy draft containing technique with given name
   */
  def getNodeConfigurationsMatchingPolicy(techniqueId : TechniqueId, allNodeConfigs:Map[NodeId, TargetNodeConfiguration]) : Seq[TargetNodeConfiguration] = {
    allNodeConfigs.values.toSeq.filterNot( _.findDirectiveByTechnique(techniqueId).isEmpty )
  }

  /**
   * Find the NodeConfigurations having the directive named (it's the directiveId)
   * We are looking for CURRENT rule policy draft
   */
  def getNodeConfigurationsMatchingDirective(cf3PolicyDraftId : Cf3PolicyDraftId, allNodeConfigs: Map[NodeId, TargetNodeConfiguration]) : Seq[TargetNodeConfiguration] = {
    allNodeConfigs.values.toSeq.filter( _.identifiableCFCPIs.exists(x => x.draftId == cf3PolicyDraftId) )
  }

}
