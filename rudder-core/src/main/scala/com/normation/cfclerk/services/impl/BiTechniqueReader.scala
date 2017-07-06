/*
*************************************************************************************
* Copyright 2017 Normation SAS
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

package com.normation.cfclerk.services.impl

import scala.xml._
import com.normation.cfclerk.domain._
import java.io.FileNotFoundException
import org.xml.sax.SAXParseException
import com.normation.cfclerk.exceptions._
import java.io.File
import net.liftweb.common._
import scala.collection.mutable.{ Map => MutMap }
import com.normation.utils.Utils
import scala.collection.immutable.SortedMap
import java.io.InputStream
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.lib.ObjectId
import scala.collection.mutable.{ Map => MutMap }
import com.normation.cfclerk.xmlparsers.TechniqueParser
import com.normation.cfclerk.services._
import scala.collection.JavaConversions._
import org.eclipse.jgit.diff.DiffFormatter
import org.eclipse.jgit.errors.MissingObjectException
import org.eclipse.jgit.diff.DiffEntry.ChangeType
import org.apache.commons.io.IOUtils

/**
 *
 * A TechniqueReader that reads policy techniques from
 * a git repository and then override or replace
 * some of the technique with the technique from a second
 * technique reader.
 *
 */
class BiTechniqueReader(
    masterTechniqueReader  : TechniqueReader
  , overrideTechniqueReader: TechniqueReader
  ) extends TechniqueReader with Loggable {

  private[this] var overridedTechniqueIds = Set[TechniqueId]()

  // get the correct repos to look for a technique
  private[this] def getRepos(techniqueId: TechniqueId): TechniqueReader = synchronized {
    if(overridedTechniqueIds.contains(techniqueId)) {
      overrideTechniqueReader
    } else {
      masterTechniqueReader
    }
  }


  override def getModifiedTechniques : Map[TechniqueName, TechniquesLibraryUpdateType] = {
    masterTechniqueReader.getModifiedTechniques ++ overrideTechniqueReader.getModifiedTechniques
  }

  override def getMetadataContent[T](techniqueId: TechniqueId)(useIt : Option[InputStream] => T) : T = {
    getRepos(techniqueId).getMetadataContent(techniqueId)(useIt)
  }

  override def getReportingDetailsContent[T](techniqueId: TechniqueId)(useIt : Option[InputStream] => T) : T = {
    getRepos(techniqueId).getReportingDetailsContent(techniqueId)(useIt)
  }

  /*
   * This one should not be used at all, it is generally not what we want !
   */
  override def checkreportingDescriptorExistence(techniqueId: TechniqueId) : Boolean = {
    getRepos(techniqueId).checkreportingDescriptorExistence(techniqueId)
  }

  override def getResourceContent[T](techniqueResourceId: TechniqueResourceId, postfixName: Option[String])(useIt : Option[InputStream] => T) : T = {
    masterTechniqueReader.getResourceContent(techniqueResourceId, postfixName) { (is: Option[InputStream]) =>
      is match {
        case None => overrideTechniqueReader.getResourceContent(techniqueResourceId, postfixName)(useIt)
        case Some(x) => useIt(Some(x))
      }
    }
  }

  override def readTechniques : TechniquesInfo = synchronized {
    val master = masterTechniqueReader.readTechniques()
    val second = overrideTechniqueReader.readTechniques()

    //update the index of which techniques are in the second provider
    overridedTechniqueIds = second.techniquesCategory.keySet

    val mutMaster = master.toInternalTechniqueInfo
    val mutSecond = second.toInternalTechniqueInfo

    //remove any technique in second from master
    //even if in other categories. Technique must be unique
    //(what happen if different version in different categories, but even in
    // our current case of only one tree? Impossible by construction?)
    mutMaster.subCategories ++= mutMaster.subCategories.map { case (id, subCat) =>
      val intersect = subCat.techniqueIds.intersect(overridedTechniqueIds)
      if(intersect.nonEmpty) {
        (id, subCat.copy(techniqueIds = subCat.techniqueIds -- intersect))
      } else {
        (id, subCat)
      }
    }

    // now add from override
    // - the root of second is either an existing category,
    //   or a new children for root
    // oversimplification: the override must be a subcategory
    // of root. IE: no search for root/a/b/ and override "b/",
    // that would lead to root/b, and root/a/b without root/b content.
    // oversimplification2: the override doesn't have subcaterogies
    val subCatId = new SubTechniqueCategoryId(TechniqueCategoryName(second.rootCategory.name), master.rootCategory.id)
    if(!master.subCategories.contains(subCatId)) {
      //update sub categories
      mutMaster.rootCategory = Some(master.rootCategory.copy(subCategoryIds = master.rootCategory.subCategoryIds + subCatId))
    }
    // we need to do the add a new subcat or update if id already exists
    mutMaster.subCategories.get(subCatId) match {
      case None =>
        val r = second.rootCategory
        mutMaster.subCategories += (subCatId -> SubTechniqueCategory(subCatId, r.name, r.description, r.subCategoryIds, r.techniqueIds, r.isSystem))
      case Some(c) => // just update it with techniques ids
        c.copy(techniqueIds = c.techniqueIds ++ overridedTechniqueIds)
    }

    //add techniques
    mutMaster.techniques ++= mutSecond.techniques
    mutMaster.techniquesCategory ++= mutSecond.techniquesCategory

    mutMaster.toTechniqueInfo(master.directivesDefaultNames)
  }


  override def needReload() = masterTechniqueReader.needReload() || overrideTechniqueReader.needReload()

}
