/*
*************************************************************************************
* Copyright 2013 Normation SAS
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

package com.normation.rudder.web.components

import bootstrap.liftweb.RudderConfig
import net.liftweb.http.DispatchSnippet
import net.liftweb.common._
import com.normation.rudder.domain.policies.Directive
import net.liftweb.http.{SHtml,S}
import scala.xml._
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.js._
import JsCmds._
import com.normation.rudder.web.components.popup.CreateOrCloneRulePopup
import JE._
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import com.normation.eventlog.ModificationId
import com.normation.rudder.web.model.CurrentUser
import com.normation.rudder.rule.category._
import com.normation.rudder.domain.policies.Rule
import com.normation.rudder.domain.policies.RuleId
import net.liftweb.http.LocalSnippet
import com.normation.rudder.web.components.popup.RuleCategoryPopup

/**
 * the component in charge of displaying the rule grid, with category tree
 * and one line by rule.
 */
class RuleDisplayer (
    directive           : Option[DirectiveApplicationManagement]
  , gridId              : String
  , detailsCallbackLink : (Rule, String) => JsCmd
  , onCreateRule        : (Rule) => JsCmd
  , showRulePopup        : (Option[Rule]) => JsCmd
) extends DispatchSnippet with Loggable  {

  private[this] val ruleRepository       = RudderConfig.roRuleRepository
  private[this] val roCategoryRepository = RudderConfig.roRuleCategoryRepository
  private[this] val woCategoryRepository = RudderConfig.woRuleCategoryRepository
  private[this] val ruleCategoryService  = RudderConfig.ruleCategoryService
  private[this] val uuidGen              = RudderConfig.stringUuidGenerator
  private[this] val configService        = RudderConfig.configService

  private[this] val htmlId_popup = "createRuleCategoryPopup"

  def getRootCategory() = {
    directive match  {
      case Some(appManagement) =>
        Full(appManagement.rootCategory)
      case None =>
        roCategoryRepository.getRootCategory
    }
  }

  private[this] var root : Box[RuleCategory]= {
      getRootCategory
  }

  def dispatch = {
    case "display" => { _ => NodeSeq.Empty }
  }

  // Update Rule displayer after a Rule has changed ( update / creation )
  def onRuleChange (selectedCategoryUpdate : RuleCategoryId)= {
    refreshGrid & refreshTree & ruleCategoryTree.map(_.updateSelectedCategory(selectedCategoryUpdate)).getOrElse(Noop)
  }

  // refresh the rule grid
  private[this] def refreshGrid = {
    SetHtml(gridId, viewRules)
  }

  // refresh the rule category Tree
  private[this] def refreshTree = {
    root = getRootCategory
    ruleCategoryTree.map(tree => SetHtml("categoryTreeParent", viewCategories(tree))).getOrElse(Noop)
  }

  private[this] val ruleCategoryTree = {
    root.map(
      new RuleCategoryTree(
          "categoryTree"
        , _
        , directive
        , (() =>  check)
        , ((c:RuleCategory) => showCategoryPopup(Some(c)))
        , ((c:RuleCategory) => showDeleteCategoryPopup(c))
        , () => refreshGrid
    ) )
  }

  def includeSubCategory = {
    SHtml.ajaxCheckbox(
        true
      , value =>  OnLoad(JsRaw(s"""
        include=${value};
        filterTableInclude('#grid_rules_grid_zone',filter,include); """)) & check()
      , ("id","includeCheckbox")
    )
  }
  def actionButtonCategory =
               if (directive.isEmpty) {
                SHtml.ajaxButton("New Category", () => showCategoryPopup(None), ("class" -> "new-icon btn btn-success btn-sm"))
              } else {
                NodeSeq.Empty
              }

  def displaySubcategories : NodeSeq = {
    <ul class="form-group">
      <li class="rudder-form">
        <div class="input-group">
          <label for="includeCheckbox" class="input-group-addon" id="includeSubCategory">
            {includeSubCategory}
            <label class="label-radio" for="includeCheckbox">
              <span class="ion ion-checkmark-round"></span>
            </label>
            <span class="ion ion-checkmark-round check-icon"></span>
          </label>
          <label for="includeCheckbox" class="form-control">
            Display Rules from subcategories
          </label>
        </div>
      </li>
    </ul>
  }
  def viewCategories(ruleCategoryTree : RuleCategoryTree) : NodeSeq = {
    <div id="treeParent">
      <div id="categoryTree">
        {ruleCategoryTree.tree}
      </div>
    </div>
  }

  def check() = {
    def action(ruleId : RuleId, status:Boolean) = {
      JsRaw(s"""$$('#${ruleId.value}Checkbox').prop("checked",${status}); """)
    }

    directive match {
      case Some(d) => d.checkRules match {case (toCheck,toUncheck) => (toCheck.map(r => action(r.id,true)) ++ toUncheck.map(r => action(r.id,false)) :\ Noop){ _ & _}}
      case None    => Noop
    }
  }

  def actionButtonRule = {
    if (directive.isDefined) {
      NodeSeq.Empty
    } else {
      SHtml.ajaxButton("New Rule", () => showRulePopup(None), ("class" -> "new-icon btn btn-success btn-sm"))
    }
  }

  def viewRules : NodeSeq = {

    val callbackLink = {
      if (directive.isDefined) {
        None
      } else {
        Some(detailsCallbackLink)
      }
    }
    val ruleGrid = {
      new RuleGrid(
          "rules_grid_zone"
        , callbackLink
        , configService.display_changes_graph
        , directive.isDefined
        , directive
      )
    }

    <div>
      { ruleGrid.rulesGridWithUpdatedInfo(None, !directive.isDefined, true, false)  ++
        Script(OnLoad(ruleGrid.asyncDisplayAllRules(None, true, configService.display_changes_graph().openOr(true)).applied))
      }
    </div>

  }

  def display = {
   val columnToFilter = {
     if (directive.isDefined) 3 else 2
   }

   ruleCategoryTree match {
     case Full(ruleCategoryTree) =>
       <div>
          <div class="row col-small-padding">
            <div class="col-xs-12 col-lg-3 col-md-4">
              <div class="box">
                <div class="box-header with-border">
                  <h3 class="box-title"><i class="fa fa-filter" aria-hidden="true"></i>Filters</h3>
                  <div class="box-tools pull-right">
                    <button class="btn btn-box-tool" data-widget="collapse"><i class="fa fa-minus"></i></button>
                  </div>
                </div><!-- /.box-header -->
                <div class="box-body">
                  <div class="row">
                    <div class="col-xs-12">
                      <div id="showFiltersRules" ng-controller="filterTagRuleCtrl" class="filters tw-bs" ng-cloak="">
                        <div class="filters-container">
                          <div class="filterTag">
                            <div class="input-group search-addon">
                              <label for="searchStr" class="input-group-addon search-addon"><span class="ion ion-search"></span></label>
                              <input type="text" id="searchStr" class="input-sm form-control" placeholder="Filter" ng-model="strSearch" ng-keyup="filterGlobal(strSearch)"/>
                            </div>
                            <div class="form-group">
                              <label>Tags</label>
                              <div class="input-group">
                                <input placeholder="key" class="form-control input-sm input-key" type="text" ng-model="newTag.key"/>
                                <span class="input-group-addon addon-json">=</span>
                                <input placeholder="value" class="form-control input-sm input-value" type="text" ng-model="newTag.value"/>
                                <span class="input-group-btn">
                                  <button type="button" ng-click="addTag()" class="btn btn-success btn-sm" ng-disabled=" (isEmptyOrBlank(newTag.key) && isEmptyOrBlank(newTag.value)); ">
                                    <span class="fa fa-plus"></span>
                                  </button>
                                </span>
                              </div>
                            </div>
                            <div class="only-tags">
                              <a href="" ng-click="onlyAll($event)" class="all" ng-class="{'active':getOnlyAllValue()}"> All </a>
                              <span class="separator">/</span>
                              <a href="" ng-click="onlyKey($event)" class="key" ng-class="{'active':only.key}"> Filter keys only   </a>
                              <span class="separator">/</span>
                              <a href="" ng-click="onlyValue($event)" class="value" ng-class="{'active':only.value}"> Filter values only </a>
                              <button class="btn btn-default btn-xs pull-right" ng-click="clearAllTags()" ng-disabled="tags.length==0">
                                Clear all tags
                                <i class="fa fa-trash" aria-hidden="true"></i>
                              </button>
                            </div>
                            <div class="tags-container">
                              <div class="btn-group btn-group-xs" role="group"  ng-repeat="tag in tags track by $index">
                                <button class="btn btn-default tag" ng-class="{'onlyKey':only.key, 'onlyValue':only.value, 'already-exist':tag.alreadyExist}" ng-click="modifyTag($index,tag)" >
                                  <span class="tag-key">
                                    <span ng-show="tag.key!=''">{{{{tag.key}}}}</span>
                                    <i class='fa fa-asterisk' aria-hidden='true' ng-show="tag.key==''"></i>
                                  </span>
                                  <span class="tag-separator">=</span>
                                  <span class="tag-value">
                                    <span ng-show="tag.value!=''">{{{{tag.value}}}}</span>
                                    <i class='fa fa-asterisk' aria-hidden='true' ng-show="tag.value==''"></i>
                                  </span>
                                </button>
                                <button type="button" class="btn btn-default" ng-click="removeTag($index)">
                                  <span class="fa fa-times"></span>
                                </button>
                              </div>
                            </div>
                          </div>
                          {displaySubcategories}
                        </div>
                      </div>
                    </div>
                  </div><!-- /.row -->
                </div><!-- /.box-footer -->
              </div><!-- /.box -->
              <div class="box">
                <div class="box-header with-border">
                  <h3 class="box-title"><i class="fa fa-list" aria-hidden="true"></i>Categories</h3>
                  <div class="box-tools pull-right">
                    <lift:authz role="rule_write">
                      {actionButtonCategory}
                    </lift:authz>
                    <button class="btn btn-box-tool btn-sm" data-widget="collapse"><i class="fa fa-minus"></i></button>
                  </div>
                </div><!-- /.box-header -->
                <div class="box-body">
                  <div class="row">
                    <div class="col-xs-12" id="categoryTreeParent">
                      {viewCategories(ruleCategoryTree)}
                    </div>
                  </div>
                </div>
              </div>
            </div>
            <div class="col-lg-9 col-xs-12 col-md-8">
              <div class="box">
                <div class="box-header with-border">
                  <h3 class="box-title"><i class="fa fa-gears" aria-hidden="true"></i>Rules</h3>
                  <div class="box-tools pull-right">
                    <button class="btn btn-box-tool toggleTabFilter updateTable btn-sm" id="updateRuleTable">Refresh<span class="fa fa-refresh"></span></button>
                    <lift:authz role="rule_write">
                      {actionButtonRule}
                    </lift:authz>
                  </div>
                </div>
                <div class="box-body">
                  <div class="row">
                    <div class="col-xs-12" id={gridId}>
                      {viewRules}
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div> ++ Script(JsRaw(s"""
                  var include = true;
                  var filter = "";
                  var column = ${columnToFilter};"""
                ) )
     case eb:EmptyBox =>
       val fail = eb ?~! "Could not get root category"
       val msg = s"An error occured while fetching Rule categories , cause is ${fail.messageChain}"
       logger.error(msg)
       <div style="padding:10px;">
         <div class="error">{msg}</div>
       </div>
   }
  }

  def ruleCreationPopup (ruleToClone:Option[Rule]) = {
    ruleCategoryTree match {
      case Full(ruleCategoryTree) =>
        val root = ruleCategoryTree.getRoot
        new CreateOrCloneRulePopup(
            root
          , ruleToClone
          , ruleCategoryTree.getSelected
          , onSuccessCallback = onCreateRule
        ).popupContent
     case eb:EmptyBox =>
       val fail = eb ?~! "Could not get root category"
       val msg = s"An error occured while fetching Rule categories , cause is ${fail.messageChain}"
       logger.error(msg)
       <div style="padding:10px;">
         <div class="error">{msg}</div>
       </div>
    }
  }

  // Popup
    private[this] def creationPopup(category : Option[RuleCategory], ruleCategoryTree : RuleCategoryTree) = {
      val rootCategory = ruleCategoryTree.getRoot
      new  RuleCategoryPopup(
          rootCategory
        , category
        , ruleCategoryTree.getSelected
        , {(r : RuleCategory) =>
            root = roCategoryRepository.getRootCategory
            ruleCategoryTree.refreshTree(root) & refreshGrid
          }
      )
    }

   /**
    * Create the popup
    */
    private[this] def showCategoryPopup(category : Option[RuleCategory]) : JsCmd = {
    val popupHtml =
      ruleCategoryTree match {
        case Full(ruleCategoryTree) =>
          creationPopup(category,ruleCategoryTree).popupContent
        case eb:EmptyBox =>
          // Should not happen, the function will be called only if the rootCategory is Set
          val fail = eb ?~! "Could not get root category"
          val msg = s"An error occured while fetching Rule categories , cause is ${fail.messageChain}"
          logger.error(msg)
          <div style="padding:10px;">
            <div class="error">{msg}</div>
          </div>
     }
      SetHtml(htmlId_popup, popupHtml) &
      JsRaw( s""" createPopup("${htmlId_popup}") """)
    }

    /**
    * Create the delete popup
    */
    private[this] def showDeleteCategoryPopup(category : RuleCategory) : JsCmd = {
      val popupHtml =
        ruleCategoryTree match {
          case Full(ruleCategoryTree) =>
            val rules = directive.map(_.rules).getOrElse(ruleRepository.getAll().openOr(Seq())).toList
            creationPopup(Some(category), ruleCategoryTree).deletePopupContent(category.canBeDeleted(rules))
          case eb:EmptyBox =>
            // Should not happen, the function will be called only if the rootCategory is Set
            val fail = eb ?~! "Could not get root category"
            val msg = s"An error occured while fetching Rule categories , cause is ${fail.messageChain}"
            logger.error(msg)
            <div style="padding:10px;">
            <div class="error">{msg}</div>
          </div>
       }
      SetHtml(htmlId_popup, popupHtml) &
      JsRaw( s""" createPopup("${htmlId_popup}") """)
    }
}