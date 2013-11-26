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

package com.normation.rudder.rule.category

import net.liftweb.common.Box
import net.liftweb.common.Full

trait RoRuleCategoryRepository {

  def get(id: RuleCategoryId) : Box[RuleCategory]

  def getRootCategory : Box[RuleCategory]

}

trait WoRuleCategoryRepository {

  def create(category:RuleCategory, container:RuleCategoryId) : Box[RuleCategory]

  def update(category:RuleCategory) : Box[RuleCategory]

  def delete(category:RuleCategory) : Box[RuleCategory]


}

object MemoryRuleCategoryRepository extends RoRuleCategoryRepository with WoRuleCategoryRepository {

  val initRoot = RuleCategory (
    RuleCategoryId("root")
  , "Rules"
  , ""
  , Nil
  , Nil
  , true
  , None
  )



  var repo = Map((initRoot.id,initRoot))


  def get(id: RuleCategoryId) : Box[RuleCategory] = repo.get(id)

  def getRootCategory : Box[RuleCategory] = repo.get(initRoot.id)


  def create(category:RuleCategory, container:RuleCategoryId) : Box[RuleCategory] = {
    repo += ((category.id,category))
    val parent : RuleCategory = repo.getOrElse(container, getRootCategory.get)
    val updated = parent.copy(childs = category :: parent.childs)
    repo = repo.updated(parent.id, updated)
    Full(category)
  }

  def update(category:RuleCategory) : Box[RuleCategory] = {

    repo = repo.updated(category.id, category)
    Full(category)
  }

  def delete(category:RuleCategory) : Box[RuleCategory] = {
    repo = repo.filterKeys(_ != category.id)
    Full(category)
  }




}