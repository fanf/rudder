/*
 *************************************************************************************
 * Copyright 2022 Normation SAS
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

package com.normation.inventory.domain

import com.normation.utils.DateFormaterService
import java.net.InetAddress
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.ISODateTimeFormat
import zio.json.*

/*
 * This file provide serializer for core inventory objects.
 * They assume the object IS the law regarding field names and type.
 * It means that they are ill suited to public APIs that should use a translation stub
 * to provide garanties on stability.
 * They are well suited for internal API that needs to evolve with the code.
 *
 *
 * Use
 * import zio.json._
 * import com.normation.inventory.domain.JsonSerializers.implicits._
 *
 * to import serializer in code, then:
 *
 * val obj = fromJson(jsonString)
 * val jsonString = toJson(obj)
 */

object JsonSerializers {
  val softwareUpdateDateTimeFormat: DateTimeFormatter = ISODateTimeFormat.dateTimeNoMillis().withZoneUTC()

  trait SoftwareUpdateJsonCodec extends SoftwareUpdateJsonEncoders with SoftwareUpdateJsonDecoders

  trait InventoryCommonJsonCodec extends InventoryCommonJsonEncoders with InventoryCommonJsonDecoders
  trait InventoryJsonCodec
      extends SoftwareUpdateJsonCodec with InventoryCommonJsonCodec with InventoryJsonEncoders with InventoryJsonDecoders

  // We need another JSON data tree for older versions where some JSON are serialized in humanized form
  object implicits       extends InventoryJsonCodec
  object older_implicits extends InventoryJsonCodec with InventoryJsonEncodersHumanized

  // the update date is normalized in RFC3339, UTC, no millis
  def parseSoftwareUpdateDateTime(d: String): Either[String, DateTime] = {
    try {
      Right(JsonSerializers.softwareUpdateDateTimeFormat.parseDateTime(d))
    } catch {
      case e: IllegalArgumentException =>
        Left(s"Error when parsing date '${d}', we expect an RFC3339, UTC no millis format. Error: ${e.getMessage}")
    }
  }

}

trait SoftwareUpdateJsonEncoders {
  private object PrivateEncoders {
    implicit val encoderDateTime: JsonEncoder[DateTime] =
      JsonEncoder[String].contramap[DateTime](d => d.toString(JsonSerializers.softwareUpdateDateTimeFormat))
  }
  import PrivateEncoders.*

  implicit val encoderSoftwareUpdateKind:     JsonEncoder[SoftwareUpdateKind]     = JsonEncoder[String].contramap { k =>
    k match {
      case SoftwareUpdateKind.Other(v) => v
      case kind                        => kind.name
    }
  }
  implicit val encoderSoftwareUpdateSeverity: JsonEncoder[SoftwareUpdateSeverity] = JsonEncoder[String].contramap { k =>
    k match {
      case SoftwareUpdateSeverity.Other(v) => v
      case kind                            => kind.name
    }
  }

  implicit val encoderSoftwareUpdate: JsonEncoder[SoftwareUpdate] = DeriveJsonEncoder.gen
}

trait SoftwareUpdateJsonDecoders {
  // This is a trick to avoid the warning of unused encoder and avoid exposing encoders that should be in private scope
  private object PrivateDecoders {
    implicit val decoderDateTime: JsonDecoder[DateTime] =
      JsonDecoder[String].mapOrFail(d => JsonSerializers.parseSoftwareUpdateDateTime(d))
  }
  import PrivateDecoders.*

  implicit val decoderSoftwareUpdateKind:     JsonDecoder[SoftwareUpdateKind]     =
    JsonDecoder[String].map(s => SoftwareUpdateKind.parse(s).getOrElse(SoftwareUpdateKind.Other(s)))
  implicit val decoderSoftwareUpdateSeverity: JsonDecoder[SoftwareUpdateSeverity] =
    JsonDecoder[String].map(s => SoftwareUpdateSeverity.parse(s).getOrElse(SoftwareUpdateSeverity.Other(s)))

  implicit val decoderSoftwareUpdate: JsonDecoder[SoftwareUpdate] = DeriveJsonDecoder.gen

}

trait InventoryCommonJsonEncoders {
  implicit val encoderManufacturer:   JsonEncoder[Manufacturer]   = JsonEncoder[String].contramap(_.name)
  implicit val encoderMemorySize:     JsonEncoder[MemorySize]     = JsonEncoder[Long].contramap(_.size)
  implicit val encoderMachineUuid:    JsonEncoder[MachineUuid]    = JsonEncoder[String].contramap(_.value)
  implicit val encoderVersion:        JsonEncoder[Version]        = JsonEncoder[String].contramap(_.value)
  implicit val encoderSoftwareEditor: JsonEncoder[SoftwareEditor] = JsonEncoder[String].contramap(_.name)
  implicit val encoderInetAddress:    JsonEncoder[InetAddress]    =
    JsonEncoder[String].contramap(com.comcast.ip4s.IpAddress.fromInetAddress(_).toString)
}

trait InventoryCommonJsonDecoders {
  implicit val decoderManufacturer:   JsonDecoder[Manufacturer]   = JsonDecoder[String].map(Manufacturer(_))
  implicit val decoderMemorySize:     JsonDecoder[MemorySize]     = JsonDecoder[Long].map(MemorySize(_))
  implicit val decoderMachineUuid:    JsonDecoder[MachineUuid]    = JsonDecoder[String].map(MachineUuid(_))
  implicit val decoderVersion:        JsonDecoder[Version]        = JsonDecoder[String].map(new Version(_))
  implicit val decoderSoftwareEditor: JsonDecoder[SoftwareEditor] = JsonDecoder[String].map(SoftwareEditor(_))
  implicit val decoderInetAddress:    JsonDecoder[InetAddress]    = {
    JsonDecoder[String].mapOrFail(ip => {
      com.comcast.ip4s.IpAddress.fromString(ip) match {
        case None    => Left(s"Value '${ip}' can not be parsed as an IP address")
        case Some(x) => Right(x.toInetAddress)
      }
    })
  }
}

// The previous JSON schema had humanized version of MemorySize and Bios#releaseDate
trait InventoryJsonEncodersHumanized { self: InventoryCommonJsonEncoders with InventoryJsonEncoders =>
  private object PrivateHumanizedEncoders {
    implicit val datetimeEncoder: JsonEncoder[DateTime] = JsonEncoder[String].contramap(DateFormaterService.getDisplayDate)
  }
  import PrivateHumanizedEncoders.*

  // Bios needs to be overriden with the humanized Datetime encoder
  implicit override val encoderBios: JsonEncoder[Bios] = DeriveJsonEncoder.gen[Bios]

  // any datastructure depending on an encoder of MemorySize will need to be overriden
  implicit override val encoderMemorySize: JsonEncoder[MemorySize] = JsonEncoder[Long].contramap(MemorySize.sizeMb)

  implicit override val encoderFileSystem: JsonEncoder[FileSystem] = DeriveJsonEncoder.gen[FileSystem]
  implicit override val encoderMemorySlot: JsonEncoder[MemorySlot] = DeriveJsonEncoder.gen[MemorySlot]
  implicit override val encoderStorage:    JsonEncoder[Storage]    = DeriveJsonEncoder.gen[Storage]
  implicit override val encoderVideo:      JsonEncoder[Video]      = DeriveJsonEncoder.gen[Video]
}

// encoder from object to json string
trait InventoryJsonEncoders { self: InventoryCommonJsonEncoders =>
  import com.normation.utils.DateFormaterService.json.*
  implicit val encoderController:     JsonEncoder[Controller]     = DeriveJsonEncoder.gen
  implicit val encoderFileSystem:     JsonEncoder[FileSystem]     = DeriveJsonEncoder.gen
  implicit val encoderMemorySlot:     JsonEncoder[MemorySlot]     = DeriveJsonEncoder.gen
  implicit val encoderProcess:        JsonEncoder[Process]        = DeriveJsonEncoder.gen
  implicit val encoderPort:           JsonEncoder[Port]           = DeriveJsonEncoder.gen
  implicit val encoderVirtualMachine: JsonEncoder[VirtualMachine] = DeriveJsonEncoder.gen
  implicit val encoderStorage:        JsonEncoder[Storage]        = DeriveJsonEncoder.gen
  implicit val encoderBios:           JsonEncoder[Bios]           = DeriveJsonEncoder.gen
  implicit val encoderNetwork:        JsonEncoder[Network]        = DeriveJsonEncoder.gen
  implicit val encoderVideo:          JsonEncoder[Video]          = DeriveJsonEncoder.gen
  implicit val encoderProcessor:      JsonEncoder[Processor]      = DeriveJsonEncoder.gen
  implicit val encoderSlot:           JsonEncoder[Slot]           = DeriveJsonEncoder.gen
  implicit val encoderSound:          JsonEncoder[Sound]          = DeriveJsonEncoder.gen

}

trait InventoryJsonDecoders { self: InventoryCommonJsonDecoders =>
  import com.normation.utils.DateFormaterService.json.*
  implicit val decoderController:     JsonDecoder[Controller]     = DeriveJsonDecoder.gen
  implicit val decoderFileSystem:     JsonDecoder[FileSystem]     = DeriveJsonDecoder.gen
  implicit val decoderMemorySlot:     JsonDecoder[MemorySlot]     = DeriveJsonDecoder.gen
  implicit val decoderProcess:        JsonDecoder[Process]        = DeriveJsonDecoder.gen
  implicit val decoderPort:           JsonDecoder[Port]           = DeriveJsonDecoder.gen
  implicit val decoderVirtualMachine: JsonDecoder[VirtualMachine] = DeriveJsonDecoder.gen
  implicit val decoderStorage:        JsonDecoder[Storage]        = DeriveJsonDecoder.gen
  implicit val decoderBios:           JsonDecoder[Bios]           = DeriveJsonDecoder.gen
  implicit val decoderNetwork:        JsonDecoder[Network]        = DeriveJsonDecoder.gen
  implicit val decoderVideo:          JsonDecoder[Video]          = DeriveJsonDecoder.gen
  implicit val decoderProcessor:      JsonDecoder[Processor]      = DeriveJsonDecoder.gen
  implicit val decoderSlot:           JsonDecoder[Slot]           = DeriveJsonDecoder.gen
  implicit val decoderSound:          JsonDecoder[Sound]          = DeriveJsonDecoder.gen
}
