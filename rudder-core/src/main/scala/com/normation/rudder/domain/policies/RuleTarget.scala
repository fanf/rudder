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

package com.normation.rudder.domain.policies

import com.normation.rudder.domain.nodes.NodeGroupId
import com.normation.inventory.domain.NodeId
import com.normation.utils.HashcodeCaching
import com.normation.rudder.domain.nodes.NodeGroup
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import net.liftweb.util.JSONParser
import net.liftweb.common.Loggable


/**
 * A target is either
 * - a Group of Node (static or dynamic),
 * - a list of Node
 * - a special (system) target ("all", "policy server", etc)
 * - a specific node
 */
sealed abstract class RuleTarget {
  def target : String
  def toJson : JValue = JString(target)
}

sealed trait NonGroupRuleTarget extends RuleTarget

sealed trait CompositeRuleTarget extends RuleTarget {
  def target = compact(render(toJson))

  def remove(target : RuleTarget) : RuleTarget

}
object GroupTarget { def r = "group:(.+)".r }
final case class GroupTarget(groupId:NodeGroupId) extends RuleTarget with HashcodeCaching {
  override def target = "group:"+groupId.value
}

//object NodeTarget { def r = "node:(.+)".r }
//case class NodeTarget(nodeId:NodeId) extends RuleTarget {
//  override def target = "node:"+nodeId.value
//}

object PolicyServerTarget { def r = "policyServer:(.+)".r }
final case class PolicyServerTarget(nodeId:NodeId) extends NonGroupRuleTarget with HashcodeCaching {
  override def target = "policyServer:"+nodeId.value
}

final case object AllTarget extends NonGroupRuleTarget {
  override def target = "special:all"
  def r = "special:all".r
}

final case object AllTargetExceptPolicyServers extends NonGroupRuleTarget {
  override def target = "special:all_exceptPolicyServers"
  def r = "special:all_exceptPolicyServers".r
}

case object EmptyTarget extends RuleTarget {
  val target = ""
}

trait TargetComposition extends CompositeRuleTarget with Loggable {
  def targets : Set[RuleTarget]

  def updateTargets(updatedTargets : Set[RuleTarget]) : TargetComposition

  def + (target : RuleTarget) = {
    val toAdd = target match {
      case t:TargetComposition => t.targets
      case _ => Set(target)
    }
    logger.info(toAdd)
    updateTargets( targets ++ toAdd)
  }

  def remove(target : RuleTarget) : RuleTarget = {
    val removedTargets = (targets - target)
    val updatedTargets = removedTargets.map({
      case composite : CompositeRuleTarget => composite.remove(target)
      case otherTarget => otherTarget
    }).filter{
      case EmptyTarget => false
      case _ => true
    }
    if (updatedTargets.isEmpty) {
      EmptyTarget
    }
      else {
      updateTargets(updatedTargets)
    }
  }
}

case class TargetIntersection ( targets : Set[RuleTarget]) extends TargetComposition {
  override val toJson : JValue = ( "and" -> targets.map(_.toJson))
  override def toString = targets.map(_.toString).mkString("( "," and ", " )")
  def updateTargets(updated : Set[RuleTarget]) = copy(targets = updated)

}

case class TargetUnion ( targets : Set [RuleTarget]) extends TargetComposition {
  override val toJson : JValue = ( "or" -> targets.map(_.toJson))
  override def toString = targets.map(_.toString).mkString("( "," or ", " )")
  def updateTargets(updated : Set[RuleTarget]) = copy(targets = updated)

}

case class TargetExclusion (includedTarget: RuleTarget, excludedTarget : RuleTarget) extends CompositeRuleTarget with Loggable{
  override val toJson : JValue = ( "include" -> includedTarget.toJson ) ~ ( "exclude" -> excludedTarget.toJson )
  override def toString = target.toString()

  def updateInclude (target : RuleTarget) = {
    val newIncluded = includedTarget match {
      case union : TargetUnion =>  union + target
      case EmptyTarget => target
      case t => TargetUnion(Set(t ,target))
    }
    logger.error(newIncluded)
    copy(newIncluded)
  }

  def updateExclude (target : RuleTarget) = {

    val newExcluded = excludedTarget match {
      case union : TargetUnion => union + target
      case EmptyTarget => target
      case t => TargetUnion(Set(t ,target))
    }
    copy(includedTarget,newExcluded)
  }
  def remove(target : RuleTarget) : RuleTarget = {
    def updateTarget(ruleTarget : RuleTarget) = {
      ruleTarget match {
        case composite : CompositeRuleTarget => composite.remove(target)
        case a if a == target => EmptyTarget
        case _ => ruleTarget
      }
    }
    val updatedInclude = updateTarget(includedTarget)
    val updatedExclude = updateTarget(excludedTarget)

    (updatedInclude, updatedExclude) match {
      case (EmptyTarget, EmptyTarget) => EmptyTarget
      case _ => this.copy(includedTarget = updatedInclude, excludedTarget = updatedExclude)
    }

  }
}

object RuleTarget extends Loggable {

  def unserJson(json : JValue) : Option[RuleTarget] = {
    logger.debug(json)
    json match {
      case JNothing => Some(EmptyTarget)
      case JString(s) => unser(s)
      case JObject(JField("and",JArray(values)) :: Nil) =>
        val res = values.map(unserJson).collect{case Some(c) => c}
        Some(TargetIntersection(res.toSet))
      case JObject(JField("or",JArray(values)) :: Nil) =>
        val res = values.map(unserJson).collect{case Some(c) => c}
        Some(TargetUnion(res.toSet))
      case JObject(values) =>
        values \ "include" match {
          case JNothing =>
            logger.error(s"${json.toString} target needs an 'include' child")
            None
          case includeJson =>
            for {
              includeTargets <- unserJson(includeJson)
              excludeTargets <- values \ "exclude" match {
                                 case JNothing =>
                                   logger.error(s"${json.toString} target needs an 'exclude' child")
                                   None
                                 case excludeJson =>
                                   unserJson(excludeJson)
                               }
            } yield {
              TargetExclusion(includeTargets,excludeTargets)
            }
        }
      case _ =>
        logger.error(s"${json.toString} is not a valid rule target")
        None
    }
  }

  def merge(targets : Set[RuleTarget]) : TargetExclusion = {

    val start = TargetExclusion(TargetUnion(Set()),TargetUnion(Set()))
    val res = (start /: targets) {
      case (res,e:TargetExclusion) =>
        logger.info(e)
        logger.warn(res)
       res.updateInclude(e.includedTarget).updateExclude(e.excludedTarget)
      case (res,t) => res.updateInclude(t)
      }
    logger.error(s"merge is $res")
    res
  }


  def unser(s:String) : Option[RuleTarget] = {
    logger.debug(s)
    s match {
      case GroupTarget.r(g) => Some(GroupTarget(NodeGroupId(g)))
      // case NodeTarget.r(s) => Some(NodeTarget(NodeId(s)))
      case PolicyServerTarget.r(s) => Some(PolicyServerTarget(NodeId(s)))
      case AllTarget.r() => Some(AllTarget)
      case AllTargetExceptPolicyServers.r() => Some(AllTargetExceptPolicyServers)
      case _ => try { unserJson(parse(s)) } catch { case e : Exception => logger.error(s"could not parse $s cause is :${e.getMessage()}"); None}
    }
  }
}

/** common information on a target */

case class RuleTargetInfo(
    target     : RuleTarget
  , name       : String
  , description: String
  , isEnabled  : Boolean
  , isSystem   : Boolean
) extends HashcodeCaching

///// the full version with all information /////

sealed trait FullRuleTarget {
  def target: RuleTarget
}

final case class FullGroupTarget(
    target   : GroupTarget
  , nodeGroup: NodeGroup
) extends FullRuleTarget

final case class FullCompositeRuleTarget(
    target: CompositeRuleTarget
) extends FullRuleTarget

final case class FullOtherTarget(
    target: NonGroupRuleTarget
) extends FullRuleTarget


final case class FullRuleTargetInfo(
    target     : FullRuleTarget
  , name       : String
  , description: String
  , isEnabled  : Boolean
  , isSystem   : Boolean
) extends HashcodeCaching {

  def toTargetInfo = RuleTargetInfo(
      target = target.target
    , name = name
    , description = description
    , isEnabled = isEnabled
    , isSystem = isSystem
  )
}

