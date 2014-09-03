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

package com.normation.rudder.services.reports

import com.normation.cfclerk.domain.TrackerVariableSpec
import com.normation.inventory.domain.NodeId
import net.liftweb.common._
import scala.collection.mutable.{Set => MutSet}
import scala.collection.mutable.{Map => MutMap}
import org.joda.time._
import org.slf4j.{Logger,LoggerFactory}
import com.normation.cfclerk.domain.{
  TrackerVariable,Variable,
  Cf3PolicyDraft,Cf3PolicyDraftId
}
import com.normation.rudder.domain._
import com.normation.rudder.domain.reports.RuleExpectedReports
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.domain.reports._
import com.normation.rudder.services.reports._
import com.normation.rudder.repository._
import com.normation.rudder.domain.policies.RuleVal
import com.normation.rudder.domain.reports.DirectiveExpectedReports
import com.normation.rudder.domain.reports.ReportComponent
import com.normation.cfclerk.xmlparsers.CfclerkXmlConstants._
import com.normation.rudder.domain.policies.ExpandedRuleVal
import com.normation.utils.Control._
import scala.collection.mutable.Buffer
import com.normation.cfclerk.domain.PredefinedValuesVariableSpec
import com.normation.cfclerk.domain.PredefinedValuesVariableSpec
import com.normation.rudder.domain.policies.ExpandedDirectiveVal
import com.normation.rudder.domain.policies.RuleId
import com.normation.rudder.reports.execution.RoReportsExecutionRepository
import com.normation.rudder.reports.execution.AgentRunId
import com.normation.rudder.reports.execution.ReportExecution
import com.normation.rudder.reports.execution.ReportExecution
import com.normation.rudder.reports.ComplianceMode

class ReportingServiceImpl(
    confExpectedRepo   : RuleExpectedReportsRepository
  , reportsRepository  : ReportsRepository
  , agentRunRepository : RoReportsExecutionRepository
  , computeCardinality : ComputeCardinalityOfDirectiveVal
  , getAgentRunInterval: () => Int
  , getComplianceMode  : () => Box[ComplianceMode]
) extends ReportingService {

  val logger = LoggerFactory.getLogger(classOf[ReportingServiceImpl])

  /**
   * Update the list of expected reports when we do a deployment
   * For each RuleVal, we check if it was present or it serial changed
   *
   * Note : deleteRules is not really used (maybe it will in the future)
   * @param ruleVal
   * @return
   */
  def updateExpectedReports(expandedRuleVals : Seq[ExpandedRuleVal], deleteRules : Seq[RuleId]) : Box[Seq[RuleExpectedReports]] = {
    // All the rule and serial. Used to know which one are to be removed
    val currentConfigurationsToRemove =  MutMap[RuleId, (Int, Set[NodeConfigurationId])]() ++
      confExpectedRepo.findAllCurrentExpectedReportsWithNodesAndSerial()

    // We can have case of node config version being updated, but node the exepected report
    // (for example, if the version increment is due to a change in an other directive)
    // track them for saving
    var updatedNodeConfigVersion = Set[(RuleId, NodeConfigurationId)]()

    val confToClose = MutSet[RuleId]()
    val confToCreate = Buffer[ExpandedRuleVal]()

    // Then we need to compare each of them with the one stored
    for (conf@ExpandedRuleVal(ruleId, configs, newSerial) <- expandedRuleVals) {
      currentConfigurationsToRemove.get(ruleId) match {
        // non existant, add it
        case None =>
          logger.debug("New rule %s".format(ruleId))
          confToCreate += conf

        case Some((serial, nodeConfigSet)) if ((serial == newSerial)&&(configs.size > 0)) =>
            // no change if same serial and some config appliable, that's ok, trace level
            logger.trace(s"Serial number (${serial}) for expected reports for rule '${ruleId.value}' was not changed and cache up-to-date: nothing to do")
            // must check that their are no differents nodes in the DB than in the new reports
            // it can happen if we delete nodes in some corner case (detectUpdates(nodes) cannot detect it
            // And it's actually nodes, not node version because version may change and still use the
            // same expected report
            if (configs.keySet.map( _.nodeId) != nodeConfigSet.map( _.nodeId)) {
              logger.debug("Same serial %s for ruleId %s, but not same node set, it need to be closed and created".format(serial, ruleId))
              confToCreate += conf
              confToClose += ruleId
            } else {
              //check for node configuration version to update
              val changedVersion = configs.keySet.filterNot( nodeConfigSet.contains )
              updatedNodeConfigVersion ++= changedVersion.map(x => (ruleId, x))
            }
            currentConfigurationsToRemove.remove(ruleId)

        case Some((serial, nodeSet)) if ((serial == newSerial)&&(configs.size == 0)) => // same serial, but no targets
            // if there is not target, then it need to be closed
          logger.debug(s"Serial number (${serial}) for expected reports for rule '${ruleId.value}' was not changed BUT no previous configuration known: update expected reports for that rule")

        case Some(serial) => // not the same serial
          logger.debug(s"Serial number (${serial}) for expected reports for rule '${ruleId.value}' was changed: update expected reports for that rule")
            confToCreate += conf
            confToClose += ruleId

            currentConfigurationsToRemove.remove(ruleId)
      }
    }


    // close the expected reports that don't exist anymore
    for (closable <- currentConfigurationsToRemove.keys) {
      confExpectedRepo.closeExpectedReport(closable)
    }
    // close the expected reports that need to be changed
    for (closable <- confToClose) {
      confExpectedRepo.closeExpectedReport(closable)
    }

    // Now I need to unfold the configuration to create, so that I get for a given
    // set of ruleId, serial, DirectiveExpectedReports we have the list of  corresponding nodes

    val expanded = confToCreate.map { case ExpandedRuleVal(ruleId, configs, serial) =>
      configs.toSeq.map { case (nodeConfigId, directives) =>
        // each directive is converted into Seq[DirectiveExpectedReports]
        val directiveExpected = directives.map { directive =>
	          val seq = computeCardinality.getCardinality(directive)

	          seq.map { case(componentName, componentsValues, unexpandedCompValues) =>
	                     DirectiveExpectedReports(
	                         directive.directiveId
	                       , Seq(
	                           ReportComponent(
	                               componentName
	                             , componentsValues.size
	                             , componentsValues
	                             , unexpandedCompValues
	                           )
	                         )
	                     )
	          }
        }

        (ruleId, serial, nodeConfigId, directiveExpected.flatten)
      }
    }

    // we need to group by DirectiveExpectedReports, RuleId, Serial
    val flatten = expanded.flatten.flatMap { case (ruleId, serial, nodeConfigId, directives) =>
      directives.map (x => (ruleId, serial, nodeConfigId, x))
    }

    val preparedValues = flatten.groupBy[(RuleId, Int, DirectiveExpectedReports)]{ case (ruleId, serial, nodeId, directive) =>
      (ruleId, serial, directive) }.map { case (key, value) => (key -> value.map(x=> x._3))}.toSeq

    // here we group them by rule/serial/seq of node, so that we have the list of all DirectiveExpectedReports that apply to them
    val groupedContent = preparedValues.toSeq.map { case ((ruleId, serial, directive), nodes) => (ruleId, serial, directive, nodes) }.
       groupBy[(RuleId, Int, Seq[NodeConfigurationId])]{ case (ruleId, serial, directive, nodes) => (ruleId, serial, nodes.toSeq)}.map {
         case (key, value) => (key -> value.map(x => x._3))
       }.toSeq
    // now we save them

    for {
      updatedExpectedReports   <- sequence(groupedContent) { case ((ruleId, serial, nodes), directives) =>
                                    confExpectedRepo.saveExpectedReports(ruleId, serial, directives, nodes)
                                  }
      nodeConfigToUpdate       =  (updatedNodeConfigVersion.groupBy { case (ruleId, _) => ruleId }).mapValues { case set =>
                                    set.toSeq.map { case (_, NodeConfigurationId(nodeId, version)) =>
                                      (nodeId, version)
                                    }.groupBy( _._1).mapValues( _.map(_._2 ) )
                                  }
      updatedNodeConfigVersion <- confExpectedRepo.updateNodeConfigVersion(nodeConfigToUpdate)
    } yield {
      updatedExpectedReports
    }
  }

  /**
   * Find the latest reports for a given rule (for all servers)
   * Note : if there is an expected report, and that we don't have it, we should say that it is empty
   */
  override private[reports] def findImmediateReportsByRule(ruleId : RuleId) : Box[Option[ExecutionBatch]] = {
     // look in the configuration
    confExpectedRepo.findCurrentExpectedReports(ruleId) match {
      case Empty => Empty
      case e:Failure => logger.error("Error when fetching reports for Rule %s : %s".format(ruleId.value, e.messageChain)); e
      case Full(expected) => Full(expected.map(createLastBatchFromConfigurationReports(_)))
    }
  }

  override def findDirectiveRuleStatusReportsByRule(ruleId: RuleId): Box[Seq[DirectiveRuleStatusReport]] = {
    for {
      optBatch <- findImmediateReportsByRule(ruleId)
    } yield {
      optBatch match {
        case None => Seq()
        case Some(batch) => batch.getRuleStatus
      }
    }
  }

  /**
   * Find the latest reports for a seq of rules (for all node)
   * Note : if there is an expected report, and that we don't have it, we should say that it is empty
   * The returned Map should have the same elements than the Seq in argument, so that we
   * can reconcile in the RuleGrid the values, and display properly the success or failure, or applying
   */
  override private[reports] def findImmediateReportsByRules(rulesIds : Set[RuleId]) : Map[RuleId, Box[Option[ExecutionBatch]]] = {
    val agentRunInterval = getAgentRunInterval()

    val expectedReports = rulesIds.map { ruleId =>
      (ruleId, confExpectedRepo.findCurrentExpectedReports(ruleId))
    }
    // For optimization purpose, we want to do only one query to the database
    // so we handle all the expected reports at once
    // We need to go through each full non none element, and put back the elements in the map
    val nonEmptyExpected = expectedReports.map(_._2).flatten.flatten

    val rulesAndSerials = nonEmptyExpected.map(x => (x.ruleId, x.serial))
    val allReports = reportsRepository.findLastReportsByRules(rulesAndSerials, agentRunInterval)

    // Here we go over each elements of the map [ruleId, Box[ExpectedReports], and reconcile the
    // entries with the batches
    expectedReports.map { case (ruleId, status) =>
      val executionBatch = status match {
        case Full(Some(expected)) =>
          val reports = allReports.filter( report => report.ruleId == expected.ruleId)
          Full(
              Some(
                  ExecutionBatch(
                      expected.ruleId
                    , expected.directivesOnNodes.map(dir => DirectivesOnNodeExpectedReport(dir.nodeConfigurationIds, dir.directiveExpectedReports))
                    , reports
                    , expected.beginDate
                    , agentRunInterval
                  )
              )
          )
        case Full(None) => Full(None)
        case e : EmptyBox => e
      }
      (ruleId, executionBatch)
    }.toMap
  }

  override def findNodeStatusReportsByRules(rulesIds : Set[RuleId]) : Map[RuleId, Box[Seq[NodeStatusReport]]] = {
    findImmediateReportsByRules(rulesIds).map { case (id, box) => (id, box.map( _.map( _.getNodeStatus).getOrElse(Seq()) )) }
  }

  /**
   * Find the latest reports for a given Node.
   * The tactic is to try to find the last node execution,
   * and if not available, to fallback on a "last" reports
   * available
   */
  override def findNodeStatusReportsByNode(nodeId: NodeId) : Box[Seq[NodeStatusReport]] = {
    for {
      optLastRun <- agentRunRepository.getNodeLastExecution(nodeId)
      allReports <- optLastRun match {
                      case None =>
                        //in that case, we don't have any reports, and no mean to know the nodeConfigVersion
                        //could be. Just get the last expected report for that node
                        for {
                          expected <- confExpectedRepo.findCurrentExpectedReportsByNode(nodeId)
                        } yield {
                          (None, None, expected, Seq())
                        }
                      case Some(run) => run.nodeConfigVersion match {
                        case None =>
                          //this is a migration mode, look for last reports
                          for {
                            expected <- confExpectedRepo.findCurrentExpectedReportsByNode(nodeId)
                            //Question: shouldn't we get reports based on the run.runId.date ?
                            reports  <- reportsRepository.findReportByAgentExecution(Set(run.runId))
                          } yield {
                            (Some(run.runId.date), None, expected, reports)
                          }
                        case Some(version) =>
                          for {
                            expected <- confExpectedRepo.findExpectedReportsByNodeConfigId(NodeConfigurationId(nodeId,version))
                            reports  <- reportsRepository.findReportByAgentExecution(Set(run.runId))
                          } yield {
                            (Some(run.runId.date), Some(version), expected, reports)
                          }
                      }
                    }
      compliance <- getComplianceMode()
    } yield {

      val (optAgentRunTime, optVersion, expectedReports, nodeReports) = allReports

      ExecutionBatch.getNodeStatusReports(
          nodeId
        , optAgentRunTime
        , optVersion
        , expectedReports
        , nodeReports
        , getAgentRunInterval()
        , compliance
      )
    }
  }


  /************************ Helpers functions **************************************/

  /**
   * From a RuleExpectedReports, create batch synthetizing the last run
   * @param expectedOperationReports
   * @param reports
   * @return
   */
  private def createLastBatchFromConfigurationReports(
      expectedConfigurationReports : RuleExpectedReports
    , nodeId                       : Option[NodeId] = None
  ) : ExecutionBatch = {
    val agentRunInterval = getAgentRunInterval()

    // Fetch the reports corresponding to this rule, and filter them by nodes
    val reports = reportsRepository.findLastReportByRule(
            expectedConfigurationReports.ruleId
          , expectedConfigurationReports.serial
          , nodeId
          , agentRunInterval)

    // If we are only searching on a node, then we restrict the directivesonnode to this node
    val filter = (id:NodeId) => (x: DirectivesOnNodes) => x.nodeConfigurationIds.exists(_.nodeId == id)

    val directivesOnNodes = nodeId match {
      case None => expectedConfigurationReports.directivesOnNodes.map(x => DirectivesOnNodeExpectedReport(x.nodeConfigurationIds, x.directiveExpectedReports))
      case Some(node) =>
        expectedConfigurationReports.directivesOnNodes.
          filter(filter(node)).map(x =>
            DirectivesOnNodeExpectedReport(x.nodeConfigurationIds.filter(_.nodeId == node), x.directiveExpectedReports)
          )
    }

    ExecutionBatch(
          expectedConfigurationReports.ruleId
        , directivesOnNodes
        , reports
        , expectedConfigurationReports.beginDate
        , agentRunInterval
      )
  }


}

/**
 * From a directiveVal, compute all the necessary data to insert within database
 * the correct expected reports
 */
class ComputeCardinalityOfDirectiveVal {
  val logger = LoggerFactory.getLogger(classOf[ComputeCardinalityOfDirectiveVal])
  /**
   * Returns a seq of
   * Component, ComponentValues(expanded), ComponentValues (unexpanded))
   *
   */
  def getCardinality(container : ExpandedDirectiveVal) : Seq[(String, Seq[String], Seq[String])] = {
    // Computes the components values, and the unexpanded component values
    val getTrackingVariableCardinality : (Seq[String], Seq[String]) = {
      val boundingVar = container.trackerVariable.spec.boundingVariable.getOrElse(container.trackerVariable.spec.name)
      // now the cardinality is the length of the boundingVariable
      (container.variables.get(boundingVar), container.originalVariables.get(boundingVar)) match {
        case (None, None) =>
          logger.debug("Could not find the bounded variable %s for %s in DirectiveVal %s".format(
              boundingVar, container.trackerVariable.spec.name, container.directiveId.value))
          (Seq(DEFAULT_COMPONENT_KEY),Seq()) // this is an autobounding policy
        case (Some(variable), Some(originalVariables)) if (variable.values.size==originalVariables.values.size) =>
          (variable.values, originalVariables.values)
        case (Some(variable), Some(originalVariables)) =>
          logger.warn("Expanded and unexpanded values for bounded variable %s for %s in DirectiveVal %s have not the same size : %s and %s".format(
              boundingVar, container.trackerVariable.spec.name, container.directiveId.value,variable.values, originalVariables.values ))
          (variable.values, originalVariables.values)
        case (None, Some(originalVariables)) =>
          (Seq(DEFAULT_COMPONENT_KEY),originalVariables.values) // this is an autobounding policy
        case (Some(variable), None) =>
          logger.warn("Somewhere in the expansion of variables, the bounded variable %s for %s in DirectiveVal %s appeared, but was not originally there".format(
              boundingVar, container.trackerVariable.spec.name, container.directiveId.value))
          (variable.values,Seq()) // this is an autobounding policy

      }
    }

    /**
     * We have two separate paths:
     * - if the technique is standart, we keep the complex old path
     * - if it is a meta technique, we take the easy paths
     */
    container.technique.providesExpectedReports match {
      case false =>
        // this is the old path
            /*
             * We can have several components, one by section.
             * If there is no component for that policy, the policy is autobounded to DEFAULT_COMPONENT_KEY
             */
            val allComponents = container.technique.rootSection.getAllSections.flatMap { section =>
              if(section.isComponent) {
                section.componentKey match {
                  case None =>
                    //a section that is a component without componentKey variable: card=1, value="None"
                    Some((section.name, Seq(DEFAULT_COMPONENT_KEY), Seq(DEFAULT_COMPONENT_KEY)))
                  case Some(varName) =>
                    //a section with a componentKey variable: card=variable card
                    val values = container.variables.get(varName).map( _.values).getOrElse(Seq())
                    val unexpandedValues = container.originalVariables.get(varName).map( _.values).getOrElse(Seq())
                    if (values.size != unexpandedValues.size)
                      logger.warn("Caution, the size of unexpanded and expanded variables for autobounding variable in section %s for directive %s are not the same : %s and %s".format(
                          section.componentKey, container.directiveId.value, values, unexpandedValues ))
                    Some((section.name, values, unexpandedValues))
                }
              } else {
                None
              }
            }

            if(allComponents.size < 1) {
              //that log is outputed one time for each directive for each node using a technique, it's far too
              //verbose on debug.
              logger.trace("Technique '%s' does not define any components, assigning default component with expected report = 1 for Directive %s".format(
                container.technique.id, container.directiveId))

              val trackingVarCard = getTrackingVariableCardinality
              Seq((container.technique.id.name.value, trackingVarCard._1, trackingVarCard._2))
            } else {
              allComponents
            }
      case true =>
        // this is easy, everything is in the DirectiveVal; the components contains only one value, the
        // one that we want
        val allComponents = container.technique.rootSection.getAllSections.flatMap { section =>
          if (section.isComponent) {
            section.componentKey match {
              case None =>
                logger.error(s"We don't have defined reports keys for section ${section.name} that should provide predefined values")
                None
              case Some(name) =>
                (container.variables.get(name), container.originalVariables.get(name)) match {
                  case (Some(expandedValues), Some(originalValues)) if expandedValues.values.size == originalValues.values.size =>
                    Some((section.name, expandedValues.values, originalValues.values))

                  case (Some(expandedValues), Some(originalValues)) if expandedValues.values.size != originalValues.values.size =>
                    logger.error(s"In section ${section.name}, the original values and the expanded values don't have the same size. Orignal values are ${originalValues.values}, expanded are ${expandedValues.values}")
                    None

                  case _ =>
                    logger.error(s"The reports keys for section ${section.name} do not exist")
                    None
                }
            }
          } else {
            None
          }
        }
        allComponents
    }

  }

}

