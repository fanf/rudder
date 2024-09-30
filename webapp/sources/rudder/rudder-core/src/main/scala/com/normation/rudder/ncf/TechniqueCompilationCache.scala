/*
 *************************************************************************************
 * Copyright 2024 Normation SAS
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

package com.normation.rudder.ncf

import com.normation.errors.IOResult
import com.normation.inventory.domain.Version
import com.normation.rudder.batch.UpdateCompilationStatus
import com.normation.rudder.domain.logger.ConfigurationStatusLoggerPure
import net.liftweb.common.SimpleActor
import zio.*
import zio.NonEmptyChunk

case class EditorTechniqueError(
    id:           BundleName,
    version:      Version,
    name:         String,
    errorMessage: String
)

sealed trait CompilationStatus
case object CompilationStatusAllSuccess                                                    extends CompilationStatus
case class CompilationStatusErrors(techniquesInError: NonEmptyChunk[EditorTechniqueError]) extends CompilationStatus

/**
  * Update and get latest global techniques compilation status
  */
trait ReadTechniqueCompilationStatusService {

  def get(): IOResult[CompilationStatus]

}

/**
  * Update technique compilation status from technique compilation output and technique info
  */
trait WriteTechniqueCompilationStatusService {

  def update(technique: EditorTechnique, output: TechniqueCompilationOutput): UIO[CompilationStatus]

}

/**
  * Service to update global technique compilation status and get the latest one from filesystem
  */
class TechniqueCompilationStatusService(
    readTechniques:           EditorTechniqueReader,
    techniqueCompiler:        TechniqueCompiler,
    compilationStatusService: WriteTechniqueCompilationStatusService
) extends ReadTechniqueCompilationStatusService {

  override def get(): IOResult[CompilationStatus] = {
    readTechniques.readTechniquesMetadataFile.flatMap {
      // _._3 errors are not compilation errors but error on techniques, we ignore them for status
      case (techniques, _, _) => {

        techniques match {
          case Nil        =>
            ConfigurationStatusLoggerPure
              .trace(
                s"Get compilation status : no technique found"
              )
              .as(CompilationStatusAllSuccess)
          case c @ _ :: _ =>
            // aggregate all outputs,
            // take last status,
            // and if no output at all it means all is success

            val outputs: IOResult[NonEmptyChunk[Option[CompilationStatus]]] = ZIO
              .foreach(NonEmptyChunk.fromCons(c))(technique => {
                techniqueCompiler
                  .getCompilationOutput(technique)
                  .flatMap(ZIO.foreach(_)(compilationStatusService.update(technique, _)))
              })
            outputs.map(_.reverse.collectFirst { case Some(out) => out }
              .getOrElse(CompilationStatusAllSuccess)) <* ConfigurationStatusLoggerPure.trace(
              s"Get compilation status : read ${techniques.size} editor techniques to update compilation status with"
            )
        }
      }
    }
  }

}

/**
  * Technique compilation output needs to be saved in a cache (frequent reads, we don't want to get files on the FS every time).
  * 
  * This cache is a simple in-memory one which only saves errors, 
  * so it saves only compilation output stderr message in case the compilation failed.
  * It notifies the lift actor on update of the compilation status.
  * 
  * It is used mainly to get errors in the Rudder UI, and is updated when API requests to update techniques are made,
  * when technique library is reloaded
  */
class TechniqueCompilationErrorsCache(
    actor: SimpleActor[UpdateCompilationStatus],
    ref:   Ref[Map[(BundleName, Version), EditorTechniqueError]]
) extends WriteTechniqueCompilationStatusService {

  override def update(technique: EditorTechnique, output: TechniqueCompilationOutput): UIO[CompilationStatus] = {
    val key = (technique.id, technique.version)
    (if (output.isError) {
       ref.updateAndGet(_ + (key -> EditorTechniqueError(technique.id, technique.version, technique.name, output.stderr)))
     } else {
       ref.updateAndGet(_ - key)
     })
      .map(m => {
        val status = getStatus(m.values)
        // update every time even if value is the same for now, actor will handle the change
        actor ! UpdateCompilationStatus(status)
        status
      })
  }

  private def getStatus(errors: Iterable[EditorTechniqueError]): CompilationStatus = {
    NonEmptyChunk.fromIterableOption(errors) match {
      case None        => CompilationStatusAllSuccess
      case Some(value) => CompilationStatusErrors(value)
    }
  }

}

object TechniqueCompilationErrorsCache {
  def make(
      actor: SimpleActor[UpdateCompilationStatus]
  ): UIO[TechniqueCompilationErrorsCache] = {
    Ref
      .make(Map.empty[(BundleName, Version), EditorTechniqueError])
      .map(new TechniqueCompilationErrorsCache(actor, _))
  }
}
