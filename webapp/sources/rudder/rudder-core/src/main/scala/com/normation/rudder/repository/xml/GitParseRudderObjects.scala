/*
*************************************************************************************
* Copyright 2011 Normation SAS
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

package com.normation.rudder.repository.xml

import java.nio.file.Paths

import com.normation.GitVersion.RevId
import com.normation.cfclerk.domain.Technique
import com.normation.cfclerk.domain.TechniqueId
import com.normation.cfclerk.domain.TechniqueName
import com.normation.cfclerk.domain.TechniqueVersion
import com.normation.cfclerk.services.GitRepositoryProvider
import com.normation.cfclerk.xmlparsers.TechniqueParser
import com.normation.errors.IOResult
import com.normation.errors._
import com.normation.rudder.domain.parameters.GlobalParameter
import com.normation.rudder.domain.policies.ActiveTechnique
import com.normation.rudder.domain.policies.Directive
import com.normation.rudder.domain.policies.DirectiveRId
import com.normation.rudder.domain.policies.GroupTarget
import com.normation.rudder.domain.policies.Rule
import com.normation.rudder.domain.policies.RuleTargetInfo
import com.normation.rudder.migration.XmlEntityMigration
import com.normation.rudder.repository._
import com.normation.rudder.rule.category.RuleCategory
import com.normation.rudder.services.marshalling.ActiveTechniqueCategoryUnserialisation
import com.normation.rudder.services.marshalling.ActiveTechniqueUnserialisation
import com.normation.rudder.services.marshalling.DirectiveUnserialisation
import com.normation.rudder.services.marshalling.GlobalParameterUnserialisation
import com.normation.rudder.services.marshalling.NodeGroupCategoryUnserialisation
import com.normation.rudder.services.marshalling.NodeGroupUnserialisation
import com.normation.rudder.services.marshalling.RuleCategoryUnserialisation
import com.normation.rudder.services.marshalling.RuleUnserialisation
import com.normation.utils.UuidRegex
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.lib.Repository
import zio._
import zio.syntax._
import com.normation.rudder.domain.logger.ConfigurationLoggerPure
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.utils.Version
import com.softwaremill.quicklens._

final case class GitRootCategory(
    root: String
) {
  def directoryPath = root + "/"
}

object GitRootCategory {
  def getGitDirectoryPath(rootDirectory: String): GitRootCategory = {
    val root = {
      val p = rootDirectory.trim
      if(p.size == 0) ""
      else if(p.endsWith("/")) p.substring(0, p.size-1)
      else p
    }

    GitRootCategory(root)
  }
}

// utilities
trait GitParseCommon[T] {

  def repo: GitRepositoryProvider

  def getArchive(archiveId: GitCommitId): IOResult[T] = {
    for {
      treeId  <- GitFindUtils.findRevTreeFromRevString(repo.db, archiveId.value)
      archive <- getArchiveForRevTreeId(treeId)
    } yield {
      archive
    }
  }

  def getArchiveForRevTreeId(revTreeId:ObjectId): IOResult[T]

  def getGitDirectoryPath(rootDirectory: String): GitRootCategory = {
    GitRootCategory.getGitDirectoryPath(rootDirectory)
  }
}

class GitParseRules(
    ruleUnserialisation: RuleUnserialisation
  , val repo           : GitRepositoryProvider
  , xmlMigration       : XmlEntityMigration
  , rulesRootDirectory : String //relative name to git root file
) extends ParseRules with GitParseCommon[List[Rule]] {

  def getArchiveForRevTreeId(revTreeId:ObjectId): IOResult[List[Rule]] = {

    val root = getGitDirectoryPath(rulesRootDirectory)

    for {
      //// BE CAREFUL: GIT DOES NOT LIST DIRECTORIES
      files <- GitFindUtils.listFiles(repo.db, revTreeId, List(root.root), List(".xml"))
      paths =  files.filter { p =>
                   p.size > root.directoryPath.size &&
                   p.startsWith(root.directoryPath) &&
                   p.endsWith(".xml") &&
                   UuidRegex.isValid(p.substring(root.directoryPath.size,p.size - 4))
               }
      xmls  <- ZIO.foreach(paths) { crPath =>
                 GitFindUtils.getFileContent(repo.db, revTreeId, crPath){ inputStream =>
                   ParseXml(inputStream, Some(crPath))
                 }
               }
      rules <- ZIO.foreach(xmls) { xml =>
                 for {
                   ruleXml <- xmlMigration.getUpToDateXml(xml).toIO
                   rule    <- ruleUnserialisation.unserialise(ruleXml).toIO
                 } yield {
                   rule
                 }
               }
    } yield {
      rules.toList
    }
  }
}


class GitParseGlobalParameters(
    paramUnserialisation    : GlobalParameterUnserialisation
  , val repo                : GitRepositoryProvider
  , xmlMigration            : XmlEntityMigration
  , parametersRootDirectory : String //relative name to git root file
) extends ParseGlobalParameters with GitParseCommon[List[GlobalParameter]] {

  def getArchiveForRevTreeId(revTreeId:ObjectId): IOResult[List[GlobalParameter]] = {

    val root = getGitDirectoryPath(parametersRootDirectory)

    //// BE CAREFUL: GIT DOES NOT LIST DIRECTORIES
    for {
      files  <- GitFindUtils.listFiles(repo.db, revTreeId, List(root.root), List(".xml"))
      paths  =  files.filter { p =>
                   p.size > root.directoryPath.size &&
                   p.startsWith(root.directoryPath) &&
                   p.endsWith(".xml")
                 }
      xmls   <- ZIO.foreach(paths) { paramPath =>
                  GitFindUtils.getFileContent(repo.db, revTreeId, paramPath){ inputStream =>
                    ParseXml(inputStream, Some(paramPath))
                  }
                }
      params <- ZIO.foreach(xmls) { xml =>
                    for {
                      paramXml <- xmlMigration.getUpToDateXml(xml).toIO
                      param    <- paramUnserialisation.unserialise(paramXml).toIO
                    } yield {
                      param
                    }
                }
    } yield {
      params.toList
    }
  }
}

class GitParseRuleCategories(
    unserialiser       : RuleCategoryUnserialisation
  , val repo           : GitRepositoryProvider
  , xmlMigration       : XmlEntityMigration
  , rulesRootDirectory : String //relative name to git root file
  , categoryFileName   : String = "category.xml"
) extends ParseRuleCategories with GitParseCommon[RuleCategory] {

  def getArchiveForRevTreeId(revTreeId:ObjectId) = {

    //directoryPath must end with "/"
    def recParseDirectory(paths: Set[String], directoryPath: String) : IOResult[RuleCategory] = {

      val categoryPath = directoryPath + categoryFileName
      // that's the directory of a RuleCategory.
      // don't forget to recurse sub-categories
      for {
        xml          <- GitFindUtils.getFileContent(repo.db, revTreeId, categoryPath){ inputStream =>
                          ParseXml(inputStream, Some(categoryPath)).chainError(s"Error when parsing file '${categoryPath}' as a category")
                        }
        categoryXml  <- xmlMigration.getUpToDateXml(xml).toIO
        category     <- unserialiser.unserialise(categoryXml).toIO.chainError(s"Error when unserializing category for file '${categoryPath}'")
        subDirs      =  {
                          //we only wants to keep paths that are non-empty directories with a rulecategory filename (category.xml)
                          paths.flatMap { p =>
                            if(p.size > directoryPath.size && p.startsWith(directoryPath)) {
                              val split = p.substring(directoryPath.size).split("/")
                              if(split.size == 2 && (split(1) == categoryFileName) ) {
                                Some(directoryPath + split(0) + "/")
                              } else None
                            } else None
                          }
                        }
        subCats      <- ZIO.foreach(subDirs.toSeq) { dir =>
                          recParseDirectory(paths, dir)
                        }
      } yield {
        category.copy(childs = subCats.toList)
      }
    }

    val root = getGitDirectoryPath(rulesRootDirectory)

    for {
      //// BE CAREFUL: GIT DOES NOT LIST DIRECTORIES
      paths <- GitFindUtils.listFiles(repo.db, revTreeId, List(root.root), List(".xml"))
      res   <- recParseDirectory(paths, root.directoryPath)
    } yield {
      res
    }
  }
}

class GitParseGroupLibrary(
    categoryUnserialiser: NodeGroupCategoryUnserialisation
  , groupUnserialiser   : NodeGroupUnserialisation
  , val repo            : GitRepositoryProvider
  , xmlMigration        : XmlEntityMigration
  , libRootDirectory    : String //relative name to git root file
  , categoryFileName    : String = "category.xml"
) extends ParseGroupLibrary with GitParseCommon[NodeGroupCategoryContent] {

  def getArchiveForRevTreeId(revTreeId:ObjectId): IOResult[NodeGroupCategoryContent] = {

    //directoryPath must end with "/"
    def recParseDirectory(paths: Set[String], directoryPath:String) : IOResult[NodeGroupCategoryContent] = {

      val categoryPath = directoryPath + categoryFileName

      // that's the directory of an NodeGroupCategory.
      // ignore files other than NodeGroup (UUID.xml) and directories
      // don't forget to recurse sub-categories
      for {
        xml          <- GitFindUtils.getFileContent(repo.db, revTreeId, categoryPath){ inputStream =>
                          ParseXml(inputStream, Some(categoryPath)).chainError(s"Error when parsing file '${categoryPath}' as a category")
                        }
        categoryXml  <- xmlMigration.getUpToDateXml(xml).toIO
        category     <- categoryUnserialiser.unserialise(categoryXml).toIO.chainError(s"Error when unserializing category for file '${categoryPath}'")
        groupFiles   =  {
                          paths.filter { p =>
                            p.size > directoryPath.size &&
                            p.startsWith(directoryPath) &&
                            p.endsWith(".xml") &&
                            UuidRegex.isValid(p.substring(directoryPath.size,p.size - 4))
                          }
                        }
        groups       <- ZIO.foreach(groupFiles.toSeq) { groupPath =>
                          for {
                            xml2     <- GitFindUtils.getFileContent(repo.db, revTreeId, groupPath){ inputStream =>
                              ParseXml(inputStream, Some(groupPath)).chainError(s"Error when parsing file '${groupPath}' as a directive")
                            }
                            groupXml <- xmlMigration.getUpToDateXml(xml2).toIO
                            group    <- groupUnserialiser.unserialise(groupXml).toIO.chainError(s"Error when unserializing group for file '${groupPath}'")
                          } yield {
                            group
                          }
                        }
        subDirs      =  {
                          //we only wants to keep paths that are non-empty directories with a uptcFileName/uptFileName in them
                          paths.flatMap { p =>
                            if(p.size > directoryPath.size && p.startsWith(directoryPath)) {
                              val split = p.substring(directoryPath.size).split("/")
                              if(split.size == 2 && (split(1) == categoryFileName) ) {
                                Some(directoryPath + split(0) + "/")
                              } else None
                            } else None
                          }
                        }
        subCats      <- ZIO.foreach(subDirs.toSeq) { dir =>
                          recParseDirectory(paths, dir)
                        }
      } yield {
        val s = subCats.toSet
        val g = groups.toSet

        val cat = category.copy(
            children = s.map { _.category.id }.toList
          , items = g.map { x =>
                      RuleTargetInfo(
                          target      = GroupTarget(x.id)
                        , name        = x.name
                        , description = x.description
                        , isEnabled = x.isEnabled
                        , isSystem    = x.isSystem
                      )
                    }.toList
        )

        NodeGroupCategoryContent(cat, s, g)
      }
    }

    val root = getGitDirectoryPath(libRootDirectory)

    for {
      //// BE CAREFUL: GIT DOES NOT LIST DIRECTORIES
      paths <- GitFindUtils.listFiles(repo.db, revTreeId, List(root.root), Nil)
      res   <- recParseDirectory(paths, root.directoryPath)
    } yield {
      res
    }
  }
}

class GitParseTechniqueLibrary(
    techniqueParser   : TechniqueParser
  , val repo          : GitRepositoryProvider
  , libRootDirectory  : String //relative name to git root file
  , techniqueMetadata : String
) {

  /**
   * Get a technique for the specific given revisionId;
   */
  def getTechnique(name: TechniqueName, version: Version, revId: RevId): IOResult[Option[Technique]] = {
    val root = GitRootCategory.getGitDirectoryPath(libRootDirectory).root
    val id   = TechniqueId(name, TechniqueVersion(version, Some(revId)))
    (for {
      _      <- ConfigurationLoggerPure.revision.debug(s"Looking for technique: ${id.show}")
      treeId <- GitFindUtils.findRevTreeFromRevString(repo.db, revId.value)
      _      <- ConfigurationLoggerPure.revision.trace(s"Git tree corresponding to revisionId: ${revId.value}: ${treeId.toString}")
      paths  <- GitFindUtils.listFiles(repo.db, treeId, List(root), List(s"${id.withDefaultRevId.serialize}/${techniqueMetadata}"))
      _      <- ConfigurationLoggerPure.revision.trace(s"Found candidate paths: ${paths}")
      tech   <- paths.size match {
                  case 0 =>
                    ConfigurationLoggerPure.debug(s"Technique ${id.show} not found") *>
                    None.succeed
                  case 1 =>
                    val path = paths.head

                    (for {
                      t <- loadTechnique(repo.db, treeId, path, id)
                    } yield {
                      // we need to correct techniqueId revision to the one we just looked-up.
                      // (it's normal to not have it serialized)
                      Some(t.modify(_.id.version.revId).setTo(Some(revId)))
                    }).tapError(err =>
                      ConfigurationLoggerPure.revision.debug(s"Impossible to find technique with id/revision: '${id.show}': ${err.fullMsg}.")
                    )
                  case _ =>
                    Unexpected(s"There is more than one technique with ID '${id}' in git: ${paths.mkString(",")}").fail
                 }
    } yield {
      tech
    }).tapBoth(err => ConfigurationLoggerPure.error(err.fullMsg), _ => ConfigurationLoggerPure.debug(s" -> found it!"))
  }


  def loadTechnique(db: Repository, revTreeId: ObjectId, gitPath: String, id: TechniqueId): IOResult[Technique] = {
    for {
      xml <- GitFindUtils.getFileContent(db, revTreeId, gitPath){ inputStream =>
               ParseXml(inputStream, Some(gitPath)).chainError(s"Error when parsing file '${gitPath}' as XML")
             }
      res <- techniqueParser.parseXml(xml, id).toIO.chainError(s"Error when unserializing technique from file '${gitPath}'")
    } yield {
      res
    }
  }
}

class GitParseActiveTechniqueLibrary(
    categoryUnserialiser: ActiveTechniqueCategoryUnserialisation
  , uptUnserialiser     : ActiveTechniqueUnserialisation
  , piUnserialiser      : DirectiveUnserialisation
  , val repo            : GitRepositoryProvider
  , xmlMigration        : XmlEntityMigration
  , libRootDirectory    : String //relative name to git root file
  , uptcFileName        : String = "category.xml"
  , uptFileName         : String = "activeTechniqueSettings.xml"
) extends ParseActiveTechniqueLibrary with GitParseCommon[ActiveTechniqueCategoryContent] {

  /**
   * Get a directive for the specific given revisionId;
   */
  def getDirective(id: DirectiveId, revId: RevId): IOResult[Option[(ActiveTechnique, Directive)]] = {
    val root = getGitDirectoryPath(libRootDirectory).root
    (for {
      _      <- ConfigurationLoggerPure.revision.debug(s"Looking for directive: ${DirectiveRId(id, Some(revId)).show}")
      treeId <- GitFindUtils.findRevTreeFromRevString(repo.db, revId.value)
      _      <- ConfigurationLoggerPure.revision.trace(s"Git tree corresponding to revisionId: ${revId.value}: ${treeId.toString}")
      paths  <- GitFindUtils.listFiles(repo.db, treeId, List(root), List(s"${id.value}.xml"))
      _      <- ConfigurationLoggerPure.revision.trace(s"Found candidate paths: ${paths}")
      pair   <- paths.size match {
                  case 0 =>
                    ConfigurationLoggerPure.debug(s"Directive ${DirectiveRId(id, Some(revId)).show} not found") *>
                    None.succeed
                  case 1 =>
                    val path = paths.head
                    val atPath = Paths.get(Paths.get(path).getParent.toString, uptFileName)

                    (for {
                      d  <- loadDirective(repo.db, treeId, path)
                      at <- loadActiveTechnique(repo.db, treeId, atPath.toString)
                    } yield {
                      // for directive, we need to set directiveId and techniqueId revision to the one we just looked-up.
                      // (it's normal to not have it serialized. We could perhaps make load
                      val rd = (d
                        .modify(_.revId).setTo(Some(revId))
                        // we need to check if the technique version wasn't already frozen
                        .modify(_.techniqueVersion).using(v => if(v.revId.isEmpty) v.copy(revId = Some(revId)) else v)
                      )
                      Some((at, rd))
                    }).tapError(err =>
                      ConfigurationLoggerPure.revision.debug(s"Impossible to find directive with id/revision: '${DirectiveRId(id, Some(revId)).show}': ${err.fullMsg}.")
                    )
                  case _ =>
                    Unexpected(s"There is more than one directive with ID '${id}' in git: ${paths.mkString(",")}").fail
                 }
    } yield {
      pair
    }).tapError(err => ConfigurationLoggerPure.error(err.fullMsg)).tap(_ => ConfigurationLoggerPure.debug(s" -> found it!"))
  }


  def loadDirective(db: Repository, revTreeId: ObjectId, directiveGitPath: String): IOResult[Directive] = {
    for {
      oldXml <- GitFindUtils.getFileContent(db, revTreeId, directiveGitPath){ inputStream =>
                  ParseXml(inputStream, Some(directiveGitPath)).chainError(s"Error when parsing file '${directiveGitPath}' as a directive")
                }
      xml    <- xmlMigration.getUpToDateXml(oldXml).toIO
      res    <- piUnserialiser.unserialise(xml).toIO.chainError(s"Error when unserializing directive from file '${directiveGitPath}'")
    } yield {
      res._2
    }
  }

  def loadActiveTechnique(db: Repository, revTreeId: ObjectId, activeTechniqueGitPath: String): IOResult[ActiveTechnique] = {
    for {
      oldXml <- GitFindUtils.getFileContent(db, revTreeId, activeTechniqueGitPath){ inputStream =>
                  ParseXml(inputStream, Some(activeTechniqueGitPath)).chainError(s"Error when parsing file '${activeTechniqueGitPath}' as an active technique")
                }
      xml    <- xmlMigration.getUpToDateXml(oldXml).toIO
      at     <- uptUnserialiser.unserialise(xml).toIO.chainError(s"Error when unserializing active technique for file '${activeTechniqueGitPath}'")
    } yield {
      at
    }
  }


  def getArchiveForRevTreeId(revTreeId:ObjectId): IOResult[ActiveTechniqueCategoryContent] = {

    //directoryPath must end with "/"
    def recParseDirectory(paths: Set[String], directoryPath: String) : IOResult[Either[ActiveTechniqueContent, ActiveTechniqueCategoryContent]] = {

      val category = directoryPath + uptcFileName
      val template = directoryPath + uptFileName

      (paths.contains(category), paths.contains(template)) match {
        //herrr... skip that one
        case (false, false) => Inconsistency(s"The directory '${directoryPath}' does not contain '${category}' or '${template}' and should not have been considered").fail
        case (true, true) => Inconsistency(s"The directory '${directoryPath}' contains both '${uptcFileName}' and '${uptFileName}' descriptor file. Only one of them is authorized").fail
        case (true, false) =>
          // that's the directory of an ActiveTechniqueCategory.
          // ignore files other than uptcFileName (parsed as an ActiveTechniqueCategory), recurse on sub-directories
          // don't forget to sub-categories and UPT and UPTC
          for {
            xml      <- GitFindUtils.getFileContent(repo.db, revTreeId, category){ inputStream =>
                          ParseXml(inputStream, Some(category)).chainError(s"Error when parsing file '${category}' as a category")
                        }
            //here, we have to migrate XML fileformat, if not up to date
            uptcXml  <- xmlMigration.getUpToDateXml(xml).toIO
            uptc     <- categoryUnserialiser.unserialise(uptcXml).toIO.chainError(s"Error when unserializing category for file '${category}'")
            subDirs  =  {
                          //we only wants to keep paths that are non-empty directories with a uptcFileName/uptFileName in them
                          paths.flatMap { p =>
                            if(p.size > directoryPath.size && p.startsWith(directoryPath)) {
                              val split = p.substring(directoryPath.size).split("/")
                              if(split.size == 2 && (split(1) == uptcFileName || split(1) == uptFileName) ) {
                                Some(directoryPath + split(0) + "/")
                              } else None
                            } else None
                          }
                        }
            subItems <- ZIO.foreach(subDirs.toSeq) { dir =>
                          recParseDirectory(paths, dir)
                        }
          } yield {
            val subCats = subItems.collect { case Right(x) => x }.toSet
            val upts    = subItems.collect { case Left(x)  => x }.toSet

            val category = uptc.copy(
                children = subCats.map { case ActiveTechniqueCategoryContent(cat, _, _)  => cat.id }.toList
              , items    = upts.map    { case ActiveTechniqueContent(activeTechnique, _) => activeTechnique.id}.toList
            )

            Right(ActiveTechniqueCategoryContent(category, subCats, upts))
          }

        case (false, true) =>
          // that's the directory of an ActiveTechnique
          // ignore sub-directories, parse uptFileName as an ActiveTechnique, parse UUID.xml as PI
          // don't forget to add PI ids to UPT
          for {
            xml    <- GitFindUtils.getFileContent(repo.db, revTreeId, template){ inputStream =>
                         ParseXml(inputStream, Some(template)).chainError(s"Error when parsing file '${template}' as a category")
                       }
            uptXml  <- xmlMigration.getUpToDateXml(xml).toIO
            activeTechnique <- uptUnserialiser.unserialise(uptXml).toIO.chainError(s"Error when unserializing template for file '${template}'")
            piFiles =  {
                         paths.filter { p =>
                           p.size > directoryPath.size &&
                           p.startsWith(directoryPath) &&
                           p.endsWith(".xml") &&
                           UuidRegex.isValid(p.substring(directoryPath.size, p.size - 4))
                         }
                       }
            directives <- ZIO.foreach(piFiles.toSeq) { directivePath =>
                            loadDirective(repo.db, revTreeId, directivePath)
                          }
          } yield {
            val pisSet = directives.toSet
            Left(ActiveTechniqueContent(
                activeTechnique.copy(directives = pisSet.map(_.id).toList)
              , pisSet
            ))
          }
      }
    }

    val root = getGitDirectoryPath(libRootDirectory)

    (for {
      paths <- GitFindUtils.listFiles(repo.db, revTreeId, List(root.root), Nil)
      //// BE CAREFUL: GIT DOES NOT LIST DIRECTORIES
      res   <- recParseDirectory(paths, root.directoryPath)
    } yield {
      res
    }).flatMap {
      case Left(x)  => Inconsistency(s"We found an Active Technique where we were expected the root of active techniques library, and so a category. Path: '${root.root}'; found: '${x.activeTechnique}'").fail
      case Right(x) => x.succeed
    }
  }
}
