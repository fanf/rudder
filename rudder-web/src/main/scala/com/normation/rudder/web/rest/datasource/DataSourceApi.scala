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

package com.normation.rudder.web.rest.compliance

import com.normation.rudder.web.rest.RestAPI
import org.joda.time.DateTime
import net.liftweb.common._
import com.normation.rudder.datasources._
import com.normation.rudder.datasources.OneRequestAllNodes
import com.normation.rudder.datasources.OneRequestByNode
import org.joda.time.Seconds
import scala.concurrent.duration.Duration

trait DataSourceApi extends RestAPI {
  val kind = "datasources"
}
/*
sealed trait RestRequestMode[T <: HttpRequestMode]{
  def name : String
  def update(data : T) : T
  def create() : T
}

final case object RestByNodeMode extends RestRequestMode[OneRequestByNode.type] {
  def name = OneRequestByNode.name
  def create = OneRequestByNode
  def update(data : OneRequestByNode.type) = OneRequestByNode
}

final case class RestOneRequestMode(
    matchingPath  : Option[String]
  , nodeAttribute : Option[String]
) extends RestRequestMode[OneRequestAllNodes] {
  def name = OneRequestAllNodes.name
  def create =
    OneRequestAllNodes(
        matchingPath.getOrElse("")
      , nodeAttribute.getOrElse("")
    )

  def update(base : OneRequestAllNodes) =
    OneRequestAllNodes(
        matchingPath.getOrElse(base.matchingPath)
      , nodeAttribute.getOrElse(base.nodeAttribute)
    )
}

sealed trait RestSourceType[T <: DataSourceType] {
  def name : String

  def update(data : T) : T

  def create() : T
}

case class RestHttpSourceType (
    url            : Option[String]
  , headers        : Option[Map[String,String]]
  , httpMethod     : Option[String]
  , path           : Option[String]
  , requestMode    : Option[RestRequestMode[HttpRequestMode]]
  , requestTimeOut : Option[Duration]
) extends RestSourceType[HttpDataSourceType] {
  val name = "http"

  def update( base : HttpDataSourceType) = {
    base.copy(
        url = url.getOrElse(base.url)
      , headers = headers.getOrElse(base.headers)
      , httpMethod = httpMethod.getOrElse(base.httpMethod)
      , path = path.getOrElse(base.path)
      , requestMode = requestMode.map(_.update(base.requestMode)).getOrElse(base.requestMode)
      , requestTimeOut = requestTimeOut.getOrElse(base.requestTimeOut)
    )
  }

  def create = {
    HttpDataSourceType(
        url = url.getOrElse("")
      , headers = headers.getOrElse(Map())
      , httpMethod = httpMethod.getOrElse("GET")
      , path = path.getOrElse("")
      , requestMode = requestMode.map(_.create).getOrElse(OneRequestByNode)
      , requestTimeOut = requestTimeOut.getOrElse(Duration.Inf)
    )
  }
}

final case class RestSourceRunParameters (
    schedule     : Option[DataSourceSchedule]
  , onGeneration : Option[Boolean]
  , onNewNode    : Option[Boolean]
) {
    def create() : DataSourceRunParameters = {

    DataSourceRunParameters(
        schedule.getOrElse(NoSchedule(Duration.Inf))
      , onGeneration.getOrElse(false)
      , onNewNode.getOrElse(false)
    )
  }

  def update(base : DataSourceRunParameters) : DataSourceRunParameters = {
    base.copy(
        schedule.getOrElse(base.schedule)
      , onGeneration.getOrElse(base.onGeneration)
      , onNewNode.getOrElse(base.onNewNode)
    )
  }
}

case class RestDataSource (
    id : DataSourceId
  , name : Option[DataSourceName]

  , description: Option[String]
  , sourceType : Option[RestSourceType[DataSourceType]]
  , enabled    : Option[Boolean]
  , timeOut    : Option[Duration]
) {

  def create() : Box[DataSource] = {

    val defaultSourceType = RestHttpSourceType(None,None,None,None,None,None)

    Full(DataSource(
        id
      , name.getOrElse(DataSourceName(""))
      , sourceType.getOrElse(defaultSourceType).create()
      , null // parameters
      , description.getOrElse("")
      , None
      , enabled.getOrElse(false)
      , timeOut.getOrElse(Duration.Inf)
    ))
  }

  def update(base : DataSource) : DataSource = {

    base.copy(
        name = name.getOrElse(base.name)
      , sourceType = sourceType.map(_.update(base.sourceType)).getOrElse(base.sourceType)
      , description = description.getOrElse(base.description)
      , enabled = enabled.getOrElse(base.enabled)
      , updateTimeOut = timeOut.getOrElse(base.updateTimeOut)
    )
  }
}
*/