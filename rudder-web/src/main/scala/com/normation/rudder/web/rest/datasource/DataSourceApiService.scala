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

import com.normation.rudder.datasources._
import net.liftweb.json.JsonAST.JValue
import com.normation.rudder.web.rest.RestDataSerializer
import net.liftweb.common._
import com.normation.rudder.web.rest.RestUtils

class MemoryDataSourceRepository extends DataSourceRepository {

  private[this] var sources : Map[DataSourceName,DataSource] = Map()

  def getAll() = Full(sources.values.toSeq)

  def get(name : DataSourceName) : Box[Option[DataSource]]= Full(sources.get(name))

  def save(source : DataSource) = {
    sources = sources +  ((source.name,source))
    println(sources.size)
    Full(source)
  }
  def delete(sourceName : DataSourceName) : Box[DataSource] = {
    sources.get(sourceName) match {
      case Some(source) =>
        sources = sources - (sourceName)
        Full(source)
      case None =>
        Failure(s"Data source '${sourceName}' does not exists, and thus can't be deleted")
    }
  }
}

class DataSourceApiService(
    dataSourceRepo     : DataSourceRepository
  , restDataSerializer : RestDataSerializer
) extends Loggable {
  import net.liftweb.json.JsonDSL._

  type ActionType = RestUtils.ActionType
  def getSources() : Box[JValue] = {
    for {
      sources <- dataSourceRepo.getAll
      data = sources.map(restDataSerializer.serializeDataSource(_))
    } yield {
      data
    }
  }

  def getSource(name : DataSourceName) : Box[JValue] = {
    for {
      optSource <- dataSourceRepo.get(name)
      source <- Box(optSource) ?~! s"Data source ${name} does not exist."
    } yield {
      restDataSerializer.serializeDataSource(source) :: Nil
    }
  }

  def deleteSource(name : DataSourceName) : Box[JValue] = {
    for {
      source <- dataSourceRepo.delete(name)
    } yield {
      restDataSerializer.serializeDataSource(source) :: Nil
    }
  }

  def createSource(restSource : RestDataSource ) : Box[JValue] = {
    for {
      source <- restSource.create()
      _ <- dataSourceRepo.save(source)
      data = restDataSerializer.serializeDataSource(source)
    } yield {
      data :: Nil
    }
  }

  def updateSource(restSource : RestDataSource ) : Box[JValue] = {
    for {
      base <- dataSourceRepo.get(restSource.name).flatMap { Box(_) ?~! s"Cannot update data source '${restSource.name}', because it does not exist" }
      updated = restSource.update(base)
      _ <- dataSourceRepo.save(updated)
      data = restDataSerializer.serializeDataSource(updated)
    } yield {
      data :: Nil
    }
  }
}
