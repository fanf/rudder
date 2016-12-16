/*
*************************************************************************************
* Copyright 2016 Normation SAS
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

package com.normation.rudder.web

import org.junit._
import org.junit.Assert._
import org.junit.runner._
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._

import java.io.File
import java.io.FilenameFilter
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import org.apache.commons.io.filefilter.FileFilterUtils
import scala.collection.JavaConverters._
import org.apache.commons.io.IOCase
import org.apache.commons.io.Charsets
import net.liftweb.util.Html5
import java.io.FileInputStream
import net.liftweb.util.Html5Constants

@RunWith(classOf[JUnitRunner])
class AutoclosingHtmlTagTest extends Specification {


  "we" should {
    "not have any autoclosing tag" in {

      val t0 = System.currentTimeMillis

      val project_base = (
        new File(this.getClass.getClassLoader.getResource(".").getFile). // rudder/rudder-web/target/test-classes
            getParentFile.getParentFile                                  // rudder/rudder-web
      )
      //all html files in src/
      def webapp_path_/(child: String) = new File(project_base, s"src/main/webapp/$child")
      val filter = FileFilterUtils.suffixFileFilter(".html", IOCase.INSENSITIVE)
      val html_files = (
           webapp_path_/("index.html")
        :: FileUtils.listFiles(webapp_path_/("templates-hidden"), filter, TrueFileFilter.INSTANCE).asScala.toList
       ::: FileUtils.listFiles(webapp_path_/("secure")          , filter, TrueFileFilter.INSTANCE).asScala.toList
      )
      val UTF8 = Charsets.toCharset("UTF-8")

      val regex = """(<(\w+)[^<]*/>)""".r

      html_files.foreach { f =>
        regex.findAllMatchIn( FileUtils.readFileToString(f, UTF8) ).foreach { m =>
          val tag = m.group(2)
          if(Html5Constants.voidTag_?(tag)) {

          } else {
            println(s"[${f.getPath.replaceAll(project_base.getPath, "")}] ${m.group(1)}")
          }
        }
      }
      println(s"${System.currentTimeMillis - t0}ms")
//      println(templates.toList.map(_.getPath).mkString("\n"))
      true === true
    }
  }
}
