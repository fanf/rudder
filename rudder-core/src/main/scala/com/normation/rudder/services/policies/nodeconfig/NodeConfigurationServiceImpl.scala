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

package com.normation.rudder.services.policies.nodeconfig

import java.io.File
import java.io.PrintWriter

import org.joda.time.DateTime

import com.normation.cfclerk.domain.Cf3PolicyDraft
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.policies.RuleWithCf3PolicyDraft
import com.normation.rudder.exceptions.TechniqueException
import com.normation.rudder.repository.FullActiveTechniqueCategory
import com.normation.rudder.services.policies.TemplateWriter
import com.normation.utils.Control._

import net.liftweb.common._
import net.liftweb.json.NoTypeHints
import net.liftweb.json.Serialization
import net.liftweb.json.Serialization.writePretty



/**
 * A class implementing the logic about node configuration change.
 * Extracted from NodeConfigurationServiceImpl to make it testable.
 */
class DetectChangeInNodeConfiguration extends Loggable {

  override def detectChangeInNode(currentOpt: Option[NodeConfigurationCache], targetConfig: NodeConfiguration, directiveLib: FullActiveTechniqueCategory) : Set[RuleId] = {
    /*
     * Check if a policy draft (Cf3PolicyDraft) has a technique updated more recently
     * than the given date.
     *
     * If no date is passed, we consider that it's "now" and so that
     * nothing was updated.
     */
    def wasUpdatedSince(draft: Cf3PolicyDraft, optDate: Option[DateTime], directiveLib: FullActiveTechniqueCategory): Boolean = {
      //it's updated if any of the technique in the draft acceptation date is more recent than the given one

      optDate match {
        case None => false
        case Some(date) =>

        directiveLib.allTechniques.get(draft.technique.id) match {
          case None => //technique not available: consider it's an update
            true
          case Some((_, None)) => //no acceptation date available: consider it's an update
            true
          case Some((_, Some(d))) =>
            d.isAfter(date)
        }
      }
    }

    logger.debug(s"Checking changes in node '${targetConfig.nodeInfo.id.value}'")
    currentOpt match {
      case None =>
        //what do we do if we don't have a cache for the node ? All the target rules are "changes" ?
        logger.trace("No node configuration cache availabe")
        targetConfig.identifiableCFCPIs.map( _.ruleId ).toSet
      case Some(current) =>

        val target = NodeConfigurationCache(targetConfig)
        val allRuleIds = (current.policyCache.map( _.ruleId ) ++ target.policyCache.map( _.ruleId )).toSet

        // First case : a change in the minimalnodeconfig is a change of all CRs
        if (current.nodeInfoCache != target.nodeInfoCache) {
          logger.trace(s"A change in the minimal configuration of node '${current.id.value}'")
          allRuleIds

        // Second case : a change in the system variable is a change of all CRs
        } else if(current.nodeContextCache != target.nodeContextCache) {
          logger.trace(s"A change in the system variable node '${current.id.value}'")
          allRuleIds

        // Third case : a change in the parameters is a change of all CRs
        } else if(current.parameterCache != target.parameterCache) {
          logger.trace(s"A change in the parameters for node '${current.id.value}'")
          allRuleIds

        } else {

          //check for different policy draft.
          val currentDrafts = current.policyCache.map( x => (x.draftId, x) ).toMap
          val targetDrafts = target.policyCache.map( x => (x.draftId, x) ).toMap

          //draftid in one and not the other are new,
          //for the one in both, check both ruleId and cacheValue

          ((currentDrafts.keySet ++ targetDrafts.keySet).map(id => (currentDrafts.get(id), targetDrafts.get(id))).flatMap {
            case (None, None) => //should not happen
              Set[RuleId]()
            case (Some(PolicyCache(ruleId, _, _)), None) => Set(ruleId)
            case (None, Some(PolicyCache(ruleId, _, _))) => Set(ruleId)
            case (Some(PolicyCache(r0, d0, c0)), Some(PolicyCache(r1, d1, c1))) =>
              //d1 and d2 are equals by construction, but keep them for future-proofing
              if(d0 == d1) {
                if(
                   //check that the rule is the same
                      r0 == r1
                   //and that the policy draft is the same (it's cache value, actually)
                   && c0 == c1

                ) {
                  Set[RuleId]() //no modification
                } else {
                  logger.trace(s"A change in the promise with draft ID '${d0}.value' for rule ID '${r0.value}', '${r1.value}'")
                  Set(r0,r1)
                }
              } else Set[RuleId]()
          }) ++ {
            //we also have to add all Rule ID for a draft whose technique has been accepted since last cache generation
            //(because we need to write template again)
            val ids = (targetConfig.identifiableCFCPIs.collect {
              case RuleWithCf3PolicyDraft(ruleId, draft) if(wasUpdatedSince(draft, current.writtenDate, directiveLib)) => ruleId
            }).toSet

            if(ids.nonEmpty) {
              logger.trace(s"A change in the techniques (technique was updated) for rules ID [${ids.mkString(", ")}]")
            }
            ids
          }
        }
    }
  }

}



/**
 * Implementation of the Node Configuration service
 * It manages the NodeConfiguration content (the cache of the deployed conf)
 *
 * That implementation is not thread safe at all, and all call to its
 * methods should be made in the context of an actor
 * (deployment service and it's actor model is a good example)
 *
 */
class NodeConfigurationServiceImpl(
    policyTranslator    : TemplateWriter
  , repository          : NodeConfigurationCacheRepository
  , logNodeConfig       : NodeConfigurationLogger
) extends NodeConfigurationService with Loggable {

  private[this] val detect = new DetectChangeInNodeConfiguration()

  //delegate to repository for nodeconfig persistence
  def deleteNodeConfigurations(nodeIds:Set[NodeId]) :  Box[Set[NodeId]] = repository.deleteNodeConfigurations(nodeIds)
  def deleteAllNodeConfigurations() : Box[Unit] = repository.deleteAllNodeConfigurations
  def onlyKeepNodeConfiguration(nodeIds:Set[NodeId]) : Box[Set[NodeId]] = repository.onlyKeepNodeConfiguration(nodeIds)
  def cacheNodeConfiguration(nodeConfigurations: Set[NodeConfiguration]): Box[Set[NodeId]] = repository.save(nodeConfigurations.map(x => NodeConfigurationCache(x)))

  def sanitize(targets : Seq[NodeConfiguration]) : Box[Map[NodeId, NodeConfiguration]] = {

    /**
     * Sanitize directive to the node configuration, returning a new node configuration with
     * updated directives.
     *
     * That method check that:
     * - that the directive added is not already in the NodeConfiguration (why ?)
     * - that there is at most one directive for each "unique" technique
     */
    def sanitizeOne(nodeConfig: NodeConfiguration) : Box[NodeConfiguration] = {

      val emptyConfig: Box[Seq[RuleWithCf3PolicyDraft]] = Full(Seq())

      val newConfig = (emptyConfig/:nodeConfig.identifiableCFCPIs) { case (current, toAdd) =>

        current match {
          case eb:EmptyBox => eb
          case Full(seq) =>
            if(seq.exists( _.draftId == toAdd.draftId )) {
              /*
               * Why do we do that and not just keep the last (or first) inserted for a
               * given draft id ?
               */
              ParamFailure[RuleWithCf3PolicyDraft](
                  "Duplicate directive",
                  Full(new TechniqueException("Duplicate directive " + toAdd.draftId)),
                  Empty,
                  toAdd)
            } else if(!toAdd.cf3PolicyDraft.technique.isMultiInstance) {

              val withSameTechnique = (seq :+ toAdd).filter( _.cf3PolicyDraft.technique.id == toAdd.cf3PolicyDraft.technique.id).sortBy( _.cf3PolicyDraft.priority )
              //we know that the size is at least one, so keep the head, and log discard tails

              withSameTechnique.tail.foreach { x =>
                logger.debug(s"Unicity check: discard policy draft with id '${x.cf3PolicyDraft.id.value}' on node '${nodeConfig.nodeInfo.id.value}' (based on Technique with the 'unique' attribute set, and more priorised directive exists)")
              }

              Full(seq :+ withSameTechnique.head)

            } else {
              Full(seq :+ toAdd)
            }
        }
      }
      newConfig.map(x => nodeConfig.copy(identifiableCFCPIs = x))
    }


    for {
      sanitized <- sequence(targets) { sanitizeOne(_) }
    } yield {
      sanitized.map(c => (c.nodeInfo.id, c)).toMap
    }

  }

  def selectUpdatedNodeConfiguration(nodeConfigurations: Map[NodeId, NodeConfiguration]): Box[Map[NodeId, NodeConfiguration]] = {
    repository.getAll.map { oldConfigCache =>
      val newConfigCache = nodeConfigurations.map{ case (_, conf) => NodeConfigurationCache(conf) }
      val oldConfigCache = repository.getAll.openOrThrowException("TODO: change that!!!!!")

      val (updatedConfig, notUpdatedConfig) = newConfigCache.toSeq.partition{ p =>
        oldConfigCache.get(p.id) match {
          case None => true
          case Some(e) => e != p
        }
      }

      if(notUpdatedConfig.size > 0) {
        logger.debug(s"Not updating non-modified node configuration: [${notUpdatedConfig.map( _.id.value).mkString(", ")}]")
      }

      if(updatedConfig.size == 0) {
        logger.info("No node configuration was updated, no promises to write")
        Map()
      } else {
        val nodeToKeep = updatedConfig.map( _.id ).toSet
        logger.info(s"Configuration of following nodes were updated, their promises are going to be written: [${updatedConfig.map(_.id.value).mkString(", ")}]")
        nodeConfigurations.filterKeys(id => nodeToKeep.contains(id))
      }
    }
  }

  /**
   * Write templates for node configuration that changed since the last write.
   *
   */
  def writeTemplate(rootNodeId: NodeId, nodeConfigs: Map[NodeId, NodeConfiguration]) : Box[Seq[NodeConfiguration]] = {
    policyTranslator.writePromisesForMachines(nodeConfigs, rootNodeId, nodeConfigs).map(_ => nodeConfigs.values.toSeq )
  }


  override def detectChangeInNodes(nodes : Seq[NodeConfiguration], directiveLib: FullActiveTechniqueCategory) : Set[RuleId]  = {
    //todo: get the list of existing node configuration to compare to

    val existing : Map[NodeId, NodeConfigurationCache] = repository.getAll.openOrThrowException("TODO CHANGE THAT").toMap

    nodes.flatMap{ x =>
      detectChangeInNode(existing.get(x.nodeInfo.id), x, directiveLib)
    }.toSet
  }


  override def detectChangeInNode(currentOpt: Option[NodeConfigurationCache], targetConfig: NodeConfiguration, directiveLib: FullActiveTechniqueCategory) : Set[RuleId] =
    detect.detectChangeInNode(currentOpt, targetConfig, directiveLib)
}