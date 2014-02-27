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
import net.liftweb.common._
import com.normation.utils.Control.sequence


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


/**
 * A composite target is a target composed of different target
 * This target is rendered as Json
 */
sealed trait CompositeRuleTarget extends RuleTarget {
  def target = compact(render(toJson))
  def remove(target : RuleTarget) : RuleTarget
}


/**
 *  To determine if Target composition are the same
 */
sealed trait CompositionKind {
  val token : String
}

object CompositionKind {
  def fromToken (token : String) : Box[CompositionKind] = {
    token match {
      case Union.token => Full(Union)
      case Intersection.token => Full(Intersection)
      case _ => Failure(s" '${token}' is not a valid composition kind")
    }
  }

}

case object Union extends CompositionKind {
  val token = "union"
}
case object Intersection extends CompositionKind {
  val token = "intersection"
}

/**
 * Target Composition allow you to compose multiple targets in one target
 */
trait TargetComposition extends CompositeRuleTarget {
  /**
   * Targets contained in that composition
   */
  def targets : Set[RuleTarget]

  /**
   * Kind of composition
   */
  def kind : CompositionKind

  /**
   * Create a new target replacing the target by those in the target passed as parameter.
   */
  def updateTargets (updatedTargets : Set[RuleTarget]) : TargetComposition

  /**
   * Json value of a composition:
   * { "targets" -> [ targets of the composition ], "kind" -> Kind of composition }
   */
  override val toJson : JValue = {
    ( "targets" -> targets.map(_.toJson)) ~
    ( "kind" -> kind.token)
  }

  override def toString = {
    targets.map(_.toString).mkString("( ",s" ${kind.token} ", " )")
  }

  /**
   * Add a target:
   * - If the same kind of composition: merge all targets
   * - otherwise: add the target as one of the target handled by the composition
   */
  def + (target : RuleTarget) : TargetComposition = {
    val updatedTarget = target match {
        case t:TargetComposition if t.kind == kind => targets ++ t.targets
        case _ =>  ( targets + target )
      }
    updateTargets(updatedTarget)
  }

  /**
   * Remove a target from targets and remove it from targets too
   */
  def remove(target : RuleTarget) : TargetComposition = {
    val removedTargets = (targets - target)
    val updatedTargets = removedTargets.map({
      case composite : CompositeRuleTarget => composite.remove(target)
      case otherTarget => otherTarget
    })
    updateTargets(updatedTargets)
  }
}


object TargetComposition {
  def fromKind (kind:CompositionKind, targets : Set[RuleTarget]): TargetComposition = {
    kind match {
      case Union => TargetUnion(targets)
      case Intersection => TargetIntersection(targets)
    }
  }
}

/**
 * Union of all Targets, Should take all Nodes from these targets
 */
case class TargetUnion (targets:Set[RuleTarget] = Set()) extends TargetComposition {
  def kind = Union
  def updateTargets(updatedTargets: Set[RuleTarget]) = copy(updatedTargets)
}


/**
 * Intersection of all Targets, Should take Nodes belongings to all targets
 */
case class TargetIntersection (targets:Set[RuleTarget] = Set()) extends TargetComposition {
  def kind = Intersection
  def updateTargets(updatedTargets: Set[RuleTarget]) = copy(updatedTargets)
}


/**
 * this Target take 2 composition targets as parameters:
 * - Included : Targets that should be counted in
 * - Excluded : Targets that should be removed
 * Final result should be Included set of nodes with Nodes from Excluded removed
 */
case class TargetExclusion(
    includedTarget : TargetComposition
  , excludedTarget : TargetComposition
) extends CompositeRuleTarget {

  /**
   * Json value of a composition:
   * { "include" -> target composition, "kind" -> target composition }
   */
  override val toJson : JValue = {
    ( "include" -> includedTarget.toJson ) ~
    ( "exclude" -> excludedTarget.toJson )
  }

  override def toString = {
    target
  }

  /**
   * Add a target to the included target
   */
  def updateInclude (target : RuleTarget) = {
    val newIncluded = includedTarget + target
    copy(newIncluded)
  }


  /**
   * Add a target to the excluded target
   */
  def updateExclude (target : RuleTarget) = {
    val newExcluded = excludedTarget + target
    copy(includedTarget,newExcluded)
  }

  /**
   * Remove a target from both the included and the excluded target
   */
  def remove(target : RuleTarget) = {

    val updatedInclude = includedTarget.remove(target)
    val updatedExclude = excludedTarget.remove(target)
    copy(updatedInclude,updatedExclude)

  }
}

object RuleTarget extends Loggable {

  /**
   * Unserialize RuleTarget from Json
   */
  def unserJson(json : JValue) : Box[RuleTarget] = {

    def unserComposition(json : JValue) : Box[TargetComposition] = {
      json match {
        case JObject(JField("targets",JArray(content)) :: JField("kind",JString(kind)) :: Nil) =>
          for {
            targets <- sequence(content)(unserJson)
            compositionKind <- CompositionKind.fromToken(kind)
          } yield {
            TargetComposition.fromKind(compositionKind, targets.toSet)
          }
      case _ =>
        Failure(s"${json.toString} is not a valid rule target")
      }
    }

    json match {
      case JString(s) => unser(s)
      case JObject(values) =>
        values \ "include" match {
          case JNothing =>
            Failure(s"${json.toString} target needs an 'include' child")
          case includeJson =>
            for {
              includeTargets <-
                unserComposition(includeJson)
              excludeTargets <-
                values \ "exclude" match {
                  case JNothing =>
                    Failure(s"${json.toString} target needs an 'exclude' child")
                  case excludeJson =>
                    unserComposition(excludeJson)
                }
            } yield {
              TargetExclusion(includeTargets,excludeTargets)
            }
        }
      case _ =>
        unserComposition(json)
    }
  }

  def unser(s:String) : Option[RuleTarget] = {
    s match {
      case GroupTarget.r(g) =>
        Some(GroupTarget(NodeGroupId(g)))
      case PolicyServerTarget.r(s) =>
        Some(PolicyServerTarget(NodeId(s)))
      case AllTarget.r() =>
        Some(AllTarget)
      case AllTargetExceptPolicyServers.r() =>
        Some(AllTargetExceptPolicyServers)
      case _ =>
        try {
          unserJson(parse(s))
        } catch {
          case e : Exception =>
            logger.error(s"could not parse $s cause is :${e.getMessage()}")
            None
        }
    }
  }

  /**
   * Create a targetExclusion from a Set of RuleTarget
   * If the set contains only a TargetExclusion, use it
   * else put all targets into a new target Exclusion using TargetUnion as composition
   */
  def merge(targets : Set[RuleTarget]) : TargetExclusion = {
    targets.toSeq match {
      case Seq(t:TargetExclusion) => t
      case _ =>
        val start = TargetExclusion(TargetUnion(Set()),TargetUnion(Set()))
        val res = (start /: targets) {
          case (res,e:TargetExclusion) =>
           res.updateInclude(e.includedTarget).updateExclude(e.excludedTarget)
          case (res,t) => res.updateInclude(t)
          }
        res
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

