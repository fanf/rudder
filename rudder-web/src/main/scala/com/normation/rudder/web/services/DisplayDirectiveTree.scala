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

package com.normation.rudder.web.services

import scala.xml.NodeSeq
import scala.xml.NodeSeq.seqToNodeSeq
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.FullGroupTarget
import com.normation.rudder.domain.policies.FullOtherTarget
import com.normation.rudder.domain.policies.FullRuleTargetInfo
import com.normation.rudder.repository.FullActiveTechniqueCategory
import com.normation.rudder.web.model.JsTreeNode
import net.liftweb.common.Loggable
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers.{ boolean2, strToSuperArrowAssoc }
import com.normation.rudder.repository.FullActiveTechniqueCategory
import com.normation.rudder.repository.FullActiveTechnique
import com.normation.rudder.domain.policies.Directive
import scala.xml.Text
import com.normation.rudder.repository.FullActiveTechnique
import com.normation.rudder.repository.FullActiveTechnique
import com.normation.rudder.repository.FullActiveTechniqueCategory
import com.normation.rudder.repository.FullActiveTechnique

/**
 *
 * A service that is able to render the node group tree.
 *
 */
object DisplayDirectiveTree extends Loggable {


  /**
   * Display the directive tree, optionaly filtering out
   * some category or group by defining which one to
   * keep. By default, display everything.
   */
  def displayTree(
      directiveLib    : FullActiveTechniqueCategory
    , onClickCategory : Option[FullActiveTechniqueCategory => JsCmd]
    , onClickTechnique: Option[(FullActiveTechnique, FullActiveTechniqueCategory) => JsCmd]
    , onClickDirective: Option[(Directive, FullActiveTechnique, FullActiveTechniqueCategory) => JsCmd]
    , keepCategory    : FullActiveTechniqueCategory => Boolean = _ => true
    , keepTechnique   : FullRuleTargetInfo => Boolean = _ => true
    , keepDirective   : FullRuleTargetInfo => Boolean = _ => true
  ) : NodeSeq =  {


    def displayCategory(
        category        : FullActiveTechniqueCategory
      , onClickCategory : Option[FullActiveTechniqueCategory => JsCmd]
      , onClickTechnique: Option[(FullActiveTechniqueCategory, FullActiveTechnique) => JsCmd]
      , onClickDirective: Option[(FullActiveTechniqueCategory, FullActiveTechnique, Directive) => JsCmd]
    ) : JsTreeNode = new JsTreeNode {

      private[this] val localOnClickTechnique = onClickTechnique.map( _.curried(category) )

      private[this] val localOnClickDirective = onClickDirective.map( _.curried(category) )

      private[this] val tooltipId = Helpers.nextFuncName
      private[this] val xml = (
        <span class="treeActiveTechniqueCategoryName tooltipable" tooltipid={tooltipId} title={category.description}>
          {Text(category.name)}
        </span>
        <div class="tooltipContent" id={tooltipId}>
          <h3>{category.name}</h3>
          <div>{category.description}</div>
        </div>
      )

      override def body = onClickCategory match {
        case None    => <a href="#">{xml}</a>
        case Some(f) => SHtml.a({() => f(category)}, xml)
      }

      override def children = {
        /*
         * sortedActiveTechnique contains only techniques that have directives
         */
        val sortedActiveTechnique = {
          category.activeTechniques.collect { case x if(keepCategory(x)) => displayCategory(x, onClickCategory, onClickTechnique, onClickDirective) }
            .sortWith {
              case ( (_, None) , _ ) => true
              case ( _ , (_, None) ) => false
              case ( (refPt1, refPt2) ) =>
                treeUtilService.sortPt(refPt1,refPt2)
            }
            .map { case (node,_) => node }
        }

        val sortedCat = {
          category.children
            .filter { categoryId =>
              directiveRepository.containsDirective(categoryId)
            }
            .flatMap(x => activeTechniqueCategoryIdToJsTreeNode(x))
            .toList
            .sortWith { case ( (node1, cat1) , (node2, cat2) ) =>
              treeUtilService.sortActiveTechniqueCategory(cat1,cat2)
            }
            .map { case (node,_) => node }
        }

        val res = sortedActiveTechnique ++ sortedCat
        res
      }


      val rel = {
        if(category.id == groupLib.id) "root-category"
        else if (category.isSystem)    "system_category"
        else                           "category"
      }

      override val attrs = ( "rel" -> "category") :: Nil
    }


   /////////////////////////////////////////////////////////////////////////////////////////////

    def activeTechniqueToJsTreeNode(
        activeTechnique: FullActiveTechnique
      , onClickNode    : Option[(FullActiveTechnique, FullActiveTechniqueCategory) => JsCmd]
    ) : JsTreeNode = new JsTreeNode {

      override def children = Nil

      override val attrs = (
        ( "rel" -> "template") :: Nil :::
        ( if(!activeTechnique.isEnabled)
            ("class" -> "disableTreeNode") :: Nil
          else Nil
        )
      )
      override def body = {
        val tooltipId = Helpers.nextFuncName

        val xml  = {
          <span class="treeActiveTechniqueName tooltipable" tooltipid={tooltipid} title={technique.description}>
            {technique.name}
          </span>
          <div class="tooltipContent" id={tooltipId}>
            <h3>{technique.name}</h3>
            <div>{technique.description}</div>
          </div>
        }

        override def children =
          activeTechnique.directives
            .map(x => directiveIdToJsTreeNode(x)).toList
            .sortWith {
              case ( (_, None) , _  ) => true
              case (  _ ,  (_, None)) => false
              case ( (node1, Some(pi1)), (node2, Some(pi2)) ) =>
                treeUtilService.sortPi(pi1,pi2)
            }
            .map { case (node, _) => node }
        }

        onClickNode match {
          case None | _ if(targetInfo.isSystem) => <a style="cursor:default">{xml}</a>
          case Some(f)                          => SHtml.a(() => f(targetInfo), xml)
        }
      }

    }

    displayCategory(groupLib, onClickCategory, onClickTarget).toXml
  }

    def directiveToJsTreeNode(
        directive  : Directive
      , onClickNode: Option[Directive => JsCmd]
    ) : JsTreeNode = new JsTreeNode {

      override def children = Nil

      override val attrs = (
                  ( "rel" -> "directive") ::
                  ( "id" -> ("jsTree-" + directive.id.value)) ::
                  ( if(!directive.isEnabled)
                      ("class" -> "disableTreeNode") :: Nil
                    else Nil
                  )

      )
      override def body = {
        val tooltipId = Helpers.nextFuncName

        val xml  = {
                    <span class="treeDirective tooltipable" tooltipid={tooltipId}
                      title={directive.shortDescription}>
                      {directive.name}
                    </span>
                    <div class="tooltipContent" id={tooltipId}>
                      <h3>{directive.name}</h3>
                      <div>{directive.shortDescription}</div>
                    </div>
        }

        override def children = Nil

        onClickNode match {
          case None | _ if(directive.isSystem) => <a style="cursor:default">{xml}</a>
          case Some(f)                         => SHtml.a(() => f(directive), xml)
        }
      }
    }

    displayCategory(directiveLib, onClickCategory, onClickTarget).toXml
  }



  //build the tree category, filtering only category with groups
  def buildTreeKeepingGroupWithNode(
      groupLib       : FullActiveTechniqueCategory
    , nodeId         : NodeId
    , onClickCategory: Option[FullActiveTechniqueCategory => JsCmd] = None
    , onClickTarget  : Option[(FullRuleTargetInfo, FullActiveTechniqueCategory) => JsCmd] = None
  ) : NodeSeq = {

    displayTree(
        groupLib
      , onClickCategory
      , onClickTarget
      , keepCategory   = (cat => cat.allGroups.values.exists( _.nodeGroup.serverList.contains(nodeId)))
      , keepTargetInfo = (ti => ti match {
          case FullRuleTargetInfo(FullGroupTarget(_, g), _, _, _, _) => g.serverList.contains(nodeId)
          case _ => false
        })
    )
  }

}
