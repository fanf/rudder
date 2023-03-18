/*
 *************************************************************************************
 * Copyright 2023 Normation SAS
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

package com.normation.rudder.domain.nodes

import com.normation.inventory.domain._
import com.normation.inventory.domain.{Version => SVersion}
import com.normation.inventory.domain.JsonSerializers.implicits._
import com.normation.rudder.domain.policies.PolicyMode
import com.normation.rudder.domain.properties.GenericProperty
import com.normation.rudder.domain.properties.InheritMode
import com.normation.rudder.domain.properties.NodeProperty
import com.normation.rudder.domain.properties.PropertyProvider
import com.normation.rudder.reports._
import com.normation.rudder.repository.jdbc.ReportAndNodeMapping
import com.normation.utils.ParseVersion
import com.normation.utils.Version
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValue
import org.joda.time.DateTime
import zio.Chunk
import zio.json._
import zio.json.internal.Write

/**
 * A node fact is the node with all inventory info, settings, properties, etc, in a layout that is similar to what API
 * return about a node.
 * It only contains facts and not computed things about the node:
 * - no compliance scoring
 * - no resolved properties (for ex inherited ones)
 */
final case class Machine(
    id:           MachineUuid,
    provider:     MachineType,
    systemSerial: Option[String],
    manufacturer: Option[Manufacturer]
)
final case class IpAddress(inet: String)
final case class ManagementTechnology(
    name:         String,
    version:      Version,
    capabilities: Chunk[String],
    nodeKind:     NodeKind
)
final case class ManagementTechnologyDetails(
    cfengineKeys:               Chunk[String],
    cfengineUser:               String,
    keyStatus:                  KeyStatus,
    nodeReportingConfiguration: ReportingConfiguration
)

final case class RudderAgent(
    @jsonField("type") tpe: AgentType,
    user:                   String,
    version:                AgentVersion,
    securityToken:          SecurityToken,
    // agent capabilities are lower case string used as tags giving information about what agent can do
    capabilities:           Chunk[AgentCapability]
)
// rudder settings for that node
final case class RudderSettings(
    keyStatus:              KeyStatus,
    reportingConfiguration: ReportingConfiguration,
    kind:                   NodeKind,
    status:                 InventoryStatus,
    state:                  NodeState,
    policyMode:             Option[PolicyMode],
    policyServerId:         NodeId
)

final case class InputDevice(caption: String, description: String, @jsonField("type") tpe: String)
final case class LocalGroup(id: Int, name: String, members: Chunk[String])
final case class LocalUser(id: Int, name: String, login: String, home: String, shell: String)
final case class LogicalVolume(attr: String, lvName: String, lvUUID: String, segCount: String, size: Long, vgUUID: String)
final case class PhysicalVolume(
    attr:      String,
    device:    String,
    format:    String,
    free:      Long,
    peSize:    Long,
    pvPeCount: Long,
    pvUUID:    String,
    size:      Long,
    vgUUID:    String
)
final case class VolumeGroup(
    attr:         String,
    free:         Long,
    lvCount:      Long,
    pvCount:      Long,
    size:         Long,
    vgExtentsize: String,
    vgname:       String,
    vgUUID:       String
)

final case class SoftwareFact(
    name:           String,
    version:        Version,
    arch:           String,
    size:           Long,
    from:           String,
    publisher:      String,
    sourceName:     String,
    sourceVersion:  Version,
    systemCategory: String
)

final case class NodeFact(
    id:             NodeId,
    description:    Option[String],
    @jsonField("hostname")
    fqdn:           String,
    os:             OsDetails,
    rudderSettings: RudderSettings,
    rudderAgent:    RudderAgent,
    properties:     Chunk[NodeProperty],
// what the point ? Derive from RudderAgent ? At least details.
//    managementTechnology:        Chunk[ManagementTechnology],
//    managementTechnologyDetails: ManagementTechnologyDetails,

    // inventory information part of minimal node info (node create api

    lastInventoryDate:     DateTime,
    inventoryReceivedDate: DateTime,
    ipAddresses:           Chunk[IpAddress],
    timezone:              Option[NodeTimezone],
    machine:               Option[Machine],

    // inventory details, optional

    ram:                  Option[MemorySize],
    swap:                 Option[MemorySize],
    archDescription:      Option[String],
    accounts:             Chunk[String],
    bios:                 Chunk[Bios],
    controllers:          Chunk[Controller],
    customProperties:     Chunk[CustomProperty],
    environmentVariables: Chunk[(String, String)],
    fileSystems:          Chunk[FileSystem],
    inputs:               Chunk[InputDevice],
    localGroups:          Chunk[LocalGroup],
    localUsers:           Chunk[LocalUser],
    logicalVolumes:       Chunk[LogicalVolume],
    memories:             Chunk[MemorySlot],
    networks:             Chunk[Network],
    physicalVolumes:      Chunk[PhysicalVolume],
    ports:                Chunk[Port],
    processes:            Chunk[Process],
    processors:           Chunk[Processor],
    slots:                Chunk[MemorySlot],
    software:             Chunk[SoftwareFact],
    softwareUpdate:       Chunk[SoftwareUpdate],
    sounds:               Chunk[Sound],
    storages:             Chunk[Storage],
    videos:               Chunk[Video],
    vms:                  Chunk[VirtualMachine]
) {
  // todo
  def isPolicyServer: Boolean = rudderSettings.kind != NodeKind.Node
  def isSystem:       Boolean = isPolicyServer
}

final case class JsonOsDetails(
    @jsonField("type") osType: String, // this is "kernalName"
    name:                      String,
    version:                   String,
    fullName:                  String,
    kernelVersion:             String,
    servicePack:               Option[String],
    userDomain:                Option[String],
    registrationCompany:       Option[String],
    productKey:                Option[String],
    productId:                 Option[String]
)

final case class JsonAgentRunInterval(
    overrides:   String, // true, false, null (???)
    interval:    Int,
    startHour:   Int,
    startMinute: Int,
    splayHour:   Int,
    splayMinute: Int
)

final case class JSecurityToken(kind: String, token: String)

final case class JNodeProperty(name: String, value: ConfigValue, mode: Option[String], provider: Option[String])

object NodeFactSerialisation {

  implicit val codecNodeId = JsonCodec.string.transform[NodeId](NodeId(_), _.value)
  implicit val codecJsonOsDetails: JsonCodec[JsonOsDetails] = DeriveJsonCodec.gen

  implicit val decoderOsDetails: JsonDecoder[OsDetails] = JsonDecoder[JsonOsDetails].map { jod =>
    val tpe     = ParseOSType.getType(jod.osType, jod.name, jod.fullName)
    val details =
      ParseOSType.getDetails(tpe, jod.fullName, new SVersion(jod.version), jod.servicePack, new SVersion(jod.kernelVersion))
    details match {
      case w: Windows =>
        w.copy(
          userDomain = jod.userDomain,
          registrationCompany = jod.registrationCompany,
          productKey = jod.productKey,
          productId = jod.productId
        )
      case other => other
    }
  }

  implicit val encoderOsDetails: JsonEncoder[OsDetails] = JsonEncoder[JsonOsDetails].contramap { od =>
    val jod = {
      JsonOsDetails(
        od.os.kernelName,
        od.os.name,
        od.version.value,
        od.fullName,
        od.kernelVersion.value,
        od.servicePack,
        None,
        None,
        None,
        None
      )
    }
    od match {
      case w: Windows =>
        jod.copy(
          userDomain = w.userDomain,
          registrationCompany = w.registrationCompany,
          productKey = w.productKey,
          productId = w.productId
        )
      case _ => jod
    }
  }

  implicit val codecJsonAgentRunInterval:   JsonCodec[JsonAgentRunInterval]   = DeriveJsonCodec.gen
  implicit val codecAgentRunInterval:       JsonCodec[AgentRunInterval]       = JsonCodec(
    JsonEncoder[JsonAgentRunInterval].contramap[AgentRunInterval] { ari =>
      JsonAgentRunInterval(
        ari.overrides.map(_.toString()).getOrElse("default"),
        ari.interval,
        ari.startMinute,
        ari.startHour,
        ari.splaytime / 60,
        ari.splaytime % 60
      )
    },
    JsonDecoder[JsonAgentRunInterval].map[AgentRunInterval] { jari =>
      val o = jari.overrides match {
        case "true"  => Some(true)
        case "false" => Some(false)
        case _       => None
      }
      AgentRunInterval(o, jari.interval, jari.startMinute, jari.startHour, jari.splayHour * 60 + jari.splayMinute)
    }
  )
  implicit val codecAgentReportingProtocol = JsonCodec.string.transformOrFail[AgentReportingProtocol](
    s => AgentReportingProtocol.parse(s).left.map(_.fullMsg),
    _.value
  )
  implicit val codecHeartbeatConfiguration: JsonCodec[HeartbeatConfiguration] = DeriveJsonCodec.gen
  implicit val codecReportingConfiguration: JsonCodec[ReportingConfiguration] = DeriveJsonCodec.gen

  implicit val codecNodeKind = JsonCodec.string.transformOrFail[NodeKind](NodeKind.parse, _.name)

  implicit val encoderOptionPolicyMode: JsonEncoder[Option[PolicyMode]] = JsonEncoder.string.contramap {
    case None       => "default"
    case Some(mode) => mode.name
  }
  implicit val decoderOptionPolicyMode: JsonDecoder[Option[PolicyMode]] = JsonDecoder[Option[String]].mapOrFail(opt => {
    opt match {
      case None            => Right(None)
      // we need to be able to set "default", for example to reset in clone
      case Some("default") => Right(None)
      case Some(s)         => PolicyMode.parse(s).left.map(_.fullMsg).map(Some(_))
    }
  })

  implicit val codecKeyStatus = JsonCodec.string.transform[KeyStatus](
    _ match {
      case "certified" => CertifiedKey
      case _           => UndefinedKey
    },
    _.value
  )

  implicit val codecInventoryStatus = JsonCodec.string.transformOrFail[InventoryStatus](
    s => {
      InventoryStatus(s) match {
        case None     => Left(s"'${s}' is not recognized as a node status. Expected: 'pending', 'accepted'")
        case Some(is) => Right(is)
      }
    },
    _.name
  )

  implicit val codecNodeState = JsonCodec.string.transformOrFail[NodeState](NodeState.parse, _.name)
  implicit val codecRudderSettings: JsonCodec[RudderSettings] = DeriveJsonCodec.gen
  implicit val codecAgentType    = JsonCodec.string.transformOrFail[AgentType](s => AgentType.fromValue(s).left.map(_.fullMsg), _.id)
  implicit val codecAgentVersion = JsonCodec.string.transform[AgentVersion](AgentVersion(_), _.value)
  implicit val codecVersion      = JsonCodec.string.transformOrFail[Version](ParseVersion.parse, _.toVersionString)
  implicit val codecJSecurityToken: JsonCodec[JSecurityToken] = DeriveJsonCodec.gen

  implicit val codecSecturityToken: JsonCodec[SecurityToken] = JsonCodec(
    JsonEncoder[JSecurityToken].contramap[SecurityToken](st => JSecurityToken(SecurityToken.kind(st), st.key)),
    JsonDecoder[JSecurityToken].mapOrFail[SecurityToken](jst => SecurityToken.token(jst.kind, jst.token))
  )

  implicit val codecAgentCapability = JsonCodec.string.transform[AgentCapability](AgentCapability(_), _.value)

  implicit val codecConfigValue: JsonCodec[ConfigValue] = JsonCodec(
    new JsonEncoder[ConfigValue] {
      override def unsafeEncode(a: ConfigValue, indent: Option[Int], out: Write): Unit = {
        out.write(a.render(ConfigRenderOptions.defaults().setJson(true).setComments(false).setFormatted(indent.getOrElse(0) > 0)))
      }
    },
    JsonDecoder[ast.Json].map(json => GenericProperty.fromZioJson(json))
  )

  implicit val codecJNodeProperty: JsonCodec[JNodeProperty] = DeriveJsonCodec.gen

  implicit val codecNodeProperty: JsonCodec[NodeProperty] = JsonCodec(
    JsonEncoder[JNodeProperty].contramap[NodeProperty](p =>
      JNodeProperty(p.name, p.value, p.inheritMode.map(_.value), p.provider.map(_.value))
    ),
    JsonDecoder[JNodeProperty].mapOrFail[NodeProperty](jp => {
      jp.mode match {
        case None    => Right(NodeProperty(jp.name, jp.value, None, jp.provider.map(PropertyProvider(_))))
        case Some(p) =>
          InheritMode.parseString(p) match {
            case Left(err) => Left(err.fullMsg)
            case Right(x)  => Right(NodeProperty(jp.name, jp.value, Some(x), jp.provider.map(PropertyProvider(_))))
          }
      }
    })
  )

  implicit val codecRudderAgent:  JsonCodec[RudderAgent]  = DeriveJsonCodec.gen
  implicit val codecIpAddress:    JsonCodec[IpAddress]    = DeriveJsonCodec.gen
  implicit val codecNodeTimezone: JsonCodec[NodeTimezone] = DeriveJsonCodec.gen
  implicit val codecMachineUuid  = JsonCodec.string.transform[MachineUuid](MachineUuid(_), _.value)
  implicit val codecMachineType  = JsonCodec.string.transform[MachineType](
    _ match {
      case PhysicalMachineType.kind => PhysicalMachineType
      case x                        => VirtualMachineType(VmType.parse(x))
    },
    _.kind
  )
  implicit val codecManufacturer = JsonCodec.string.transform[Manufacturer](Manufacturer(_), _.name)
  implicit val codecMachine:    JsonCodec[Machine]    = DeriveJsonCodec.gen
  implicit val codecMemorySize: JsonCodec[MemorySize] = JsonCodec.long.transform[MemorySize](MemorySize(_), _.size)

  implicit val codecNodeFact: JsonCodec[NodeFact] = DeriveJsonCodec.gen

}
