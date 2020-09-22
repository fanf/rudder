/*
*************************************************************************************
* Copyright 2020 Normation SAS
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

package com.normation.rudder.configuration

import com.normation.GitVersion
import com.normation.GitVersion.RevId
import com.normation.errors.IOResult
import com.normation.errors.effectUioUnit
import com.normation.rudder.domain.policies.ActiveTechnique
import com.normation.rudder.domain.policies.Directive
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.policies.DirectiveRId
import com.normation.rudder.repository.FullActiveTechniqueCategory
import com.normation.rudder.repository.RoDirectiveRepository
import com.normation.rudder.repository.xml.GitParseActiveTechniqueLibrary
import zio._
import zio.syntax._

/*
 * Easier te manage data
 */
final case class ActiveDirective(activeTechnique: ActiveTechnique, directive: Directive)

/*
 * This class is in charge of loading and updating configuration objects in rudder.
 * All configuration objects are versionned and are associated with a pair of identifier:
 * - their unique identifier (an uuid),
 * - their version identifier (a git commit id, ie a sha1/sha256)
 *
 * The repository has a notion of "current version", which is commit id for head in default branch.
 */
trait ConfigurationRepository extends RoConfigurationRepository with WoConfigurationRepository

trait RoConfigurationRepository {

  /*
   * Get a directive and its matching active technique for the given (id, version)
   */
  def getDirective(id: DirectiveRId): IOResult[Option[ActiveDirective]]

  def getDirectiveLibrary(ids: Set[DirectiveRId]): IOResult[FullActiveTechniqueCategory]
}

trait WoConfigurationRepository {

}

/****************************************************************************************/

class ConfigurationRepositoryImpl(
    roDirectiveRepository      : RoDirectiveRepository
  , parseActiveTechniqueLibrary: GitParseActiveTechniqueLibrary
) extends ConfigurationRepository {
  override def getDirective(id: DirectiveRId): IOResult[Option[ActiveDirective]] = {
    (id.revId match {
      case None | Some(GitVersion.defaultRev) =>
        roDirectiveRepository.getActiveTechniqueAndDirective(id)
      case Some(r)                            =>
        parseActiveTechniqueLibrary.getDirective(id)
    }).map( _.map{ case (at, d) => ActiveDirective(at, d)} )
  }

  def getDirectiveLibrary(ids: Set[DirectiveRId]): IOResult[FullActiveTechniqueCategory] = {
    val withVersion = ids.filter(x => x.revId.isDefined && x.revId != Some(GitVersion.defaultRev))
    for {
      opt        <- ZIO.foreach(withVersion.toList)(getDirective) // TODO: find a way to do that without N git treewalks
      _          <- {
        val found = opt.collect { case Some(d) => d.directive.rid }
        val missing = withVersion -- found
        ZIO.foreach_(missing) { m =>
          effectUioUnit(println(s"******* MISSING ${m.show}")) *>
          effectUioUnit(println(s"******* ${opt}"))
        }
      }
      versionned =  opt.collect { case Some(ad) => (ad.activeTechnique.techniqueName, ad.directive) }
      others     <- roDirectiveRepository.getFullDirectiveLibrary()
      lib        =  others.addAndFilter(versionned, ids)
    } yield {
      lib
    }
  }
}


