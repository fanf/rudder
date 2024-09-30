package bootstrap.liftweb.checks.action

import bootstrap.liftweb.BootstrapChecks
import bootstrap.liftweb.BootstrapLogger
import com.normation.rudder.domain.logger.ConfigurationStatusLoggerPure
import com.normation.rudder.ncf.CompilationStatusAllSuccess
import com.normation.rudder.ncf.CompilationStatusErrors
import com.normation.rudder.ncf.ReadTechniqueCompilationStatusService
import com.normation.zio.UnsafeRun

/**
  * Reload the global technique compilation status at startup.
  * It is done asynchronously because errors (e.g. on a yml file in the filesystem)
  * are not supposed to prevent Rudder startup.
  */
class CheckTechniqueCompilationStatus(
    techniqueCompilationStatusService: ReadTechniqueCompilationStatusService
) extends BootstrapChecks {

  override def description: String = "Check for technique compilation errors"

  override def checks(): Unit = {
    techniqueCompilationStatusService
      .get()
      .flatMap {
        case CompilationStatusAllSuccess => ConfigurationStatusLoggerPure.info("All techniques have success compilation result")
        case e: CompilationStatusErrors => {
          val techniques = e.techniquesInError.map(t => s"${t.id.value}(v${t.version.value})").toList.mkString(",")
          ConfigurationStatusLoggerPure.warn(
            s"Found ${e.techniquesInError.size} techniques with compilation errors when starting server : ${techniques}"
          )
        }
      }
      .catchAll(err => BootstrapLogger.error(s"Error when trying to check technique compilation errors: ${err.fullMsg}"))
      .forkDaemon
      .runNow
  }
}
