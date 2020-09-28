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

package com.normation.cfclerk.domain

import org.junit.runner._
import org.specs2.mutable._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class TechniqueVersionTest extends Specification {

  sequential

  "Equal versions" should {
    equalVersions("1.0", "1.0")
    equalVersions("1.0", "1.0.0")
  }

  "Different sizes" should {
    increasingVersions("1.0", "1.0.1")
    increasingVersions("1", "1.0.0.0.0.0.1")
  }

  "with revisionId" should { // the higher one is head
    increasingVersions("1.0+commitid", "1.0")
    increasingVersions("1.0+commitid", "1.0.0")
  }

  "technique version are simple" should {
    TechniqueVersion.parse("1.0-alpha5") must beLeft()
    TechniqueVersion.parse("1.0-SNAPSHOT") must beLeft()
    TechniqueVersion.parse("1.0-beta1") must beLeft()
    TechniqueVersion.parse("1.0~anything") must beLeft()
    TechniqueVersion.parse("1.0~1") must beLeft()
    TechniqueVersion.parse("1.a") must beLeft()
    TechniqueVersion.parse("1:a") must beLeft()
  }
  private[this] def equalVersions(version1: String, version2: String) = {
    //the actual comparison test
    "be so that '%s' == '%s'".format(version1, version2) in {
      TechniqueVersionHelper(version1) == TechniqueVersionHelper(version2) must beTrue
    }
    "be so that '%s' > '%s' is false".format(version1, version2) in {
      TechniqueVersionHelper(version1) > TechniqueVersionHelper(version2) must beFalse
    }
    "be so that '%s' < '%s' is false".format(version1, version2) in {
      TechniqueVersionHelper(version1) < TechniqueVersionHelper(version2) must beFalse
    }
  }

  // test if version1 < version2
  private[this] def increasingVersions(version1: String, version2: String) = {
    //the actual comparison test
    "be so that '%s' < '%s'".format(version1, version2) in {
      TechniqueVersionHelper(version1) < TechniqueVersionHelper(version2) must beTrue
    }
    "be so that '%s' > '%s' is false".format(version1, version2) in {
      TechniqueVersionHelper(version1) > TechniqueVersionHelper(version2) must beFalse
    }
    "be so that '%s' == '%s' is false".format(version1, version2) in {
      TechniqueVersionHelper(version1) == TechniqueVersionHelper(version2) must beFalse
    }
  }
}

