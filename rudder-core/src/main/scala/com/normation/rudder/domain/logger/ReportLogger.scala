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

package com.normation.rudder.domain.logger

import com.normation.rudder.domain.reports._
import net.liftweb.common.Logger
import org.slf4j.LoggerFactory

object ReportLogger extends Logger {
  override protected def _logger = LoggerFactory.getLogger("report")
}

/**
 * A data structure for our Report Level
 */
sealed trait ReportLoggerLevel {
  def log: (=> AnyRef) => Unit
}


object AllReportLogger extends Logger {
  override protected def _logger = LoggerFactory.getLogger("non-compliant-reports")

  final object ReportLoggerLevel {

    final case object Error extends ReportLoggerLevel {
      override def log = error
    }

    final case object Warn extends ReportLoggerLevel {
      override def log = info
    }

    final case object Info extends ReportLoggerLevel {
      override def log = info
    }

  }

  def findLogger(reportType : String): ReportLoggerLevel = {
    import Reports._

    reportType match{
      // error
      case RESULT_ERROR       => ReportLoggerLevel.Error
      case AUDIT_NONCOMPLIANT => ReportLoggerLevel.Error
      case AUDIT_ERROR        => ReportLoggerLevel.Error

      // warning
      case RESULT_REPAIRED => ReportLoggerLevel.Warn
      case LOG_WARN        => ReportLoggerLevel.Warn
      case LOG_WARNING     => ReportLoggerLevel.Warn

      case _               => ReportLoggerLevel.Info
    }
  }
}


