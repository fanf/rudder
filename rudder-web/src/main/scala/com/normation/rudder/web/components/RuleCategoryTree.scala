/*
*************************************************************************************
* Copyright 2013 Normation SAS
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

package com.normation.rudder.web.components

import com.normation.rudder.web.model.JsTreeNode
import net.liftweb.common._
import net.liftweb.http.{SHtml,S}
import scala.xml._
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.js._
import JsCmds._
import JE._
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import com.normation.rudder.repository._
import com.normation.rudder.services.policies._
import bootstrap.liftweb.RudderConfig
import com.normation.rudder.rule.category._

/**
 * A component to display a tree based on a
 * Technique.
 *
 * The tree show all parent of the Technique
 * and dependent Rules.
 *
 */
class RuleCategoryTree(
  htmlId_activeTechniquesTree:String
) extends DispatchSnippet with Loggable {

  //find Technique
  val ruleCategoryRepository = RudderConfig.roRuleCategoryRepository
  val ruleCategoryService    = RudderConfig.ruleCategoryService

  def dispatch = {
    case "tree" => { _ => tree }
  }


  def tree() : NodeSeq = {

    (for {
      root <-  ruleCategoryRepository.getRootCategory
    } yield {
      categoryNode(root)
    }) match {

      case Full(treeNode) =>
        {<ul>{treeNode.toXml}</ul>} ++ Script(OnLoad(JsRaw(
          s"""buildRuleCategoryTree('#${htmlId_activeTechniquesTree}','${ruleCategoryRepository.getRootCategory.map(_.id.value).getOrElse("")}','${S.contextPath}'); createTooltip();"""
      )))
      case e:EmptyBox =>
        val msg = "Can not build tree of Rule categories"
        logger.error(msg,e)
        (new JsTreeNode {
          override def body = <span class="error">Can not find dependencies. <span class="errorDetails">{(e ?~! msg).messageChain}</span></span>
          override def children = Nil
        }).toXml
    }
  }


  private[this] def categoryNode(category : RuleCategory) : JsTreeNode = new JsTreeNode {
    override val attrs = ( "rel" -> "category" ) :: ("id", category.id.value) :: Nil
    override def body = {

      val xml = {
           <span class="treeActiveTechniqueCategoryName tooltipable" tooltipid={category.id.value}  title={category.description}>
             {category.name}
           </span>
         <div class="tooltipContent" id={category.id.value}>
           <h3>{category.name}</h3>
           <div>{category.description}</div>
         </div>
      }
       SHtml.a(() =>  SetHtml("categoryDisplay",Text(ruleCategoryService.fqdn(category.id).getOrElse("Error"))), xml)
    }
    override def children = category.childs.map(categoryNode(_))


  }


}