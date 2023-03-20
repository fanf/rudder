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

package com.normation.rudder.domain.queries

import com.normation.errors._
import com.normation.inventory.domain.MachineType
import com.normation.inventory.domain.MemorySize
import com.normation.inventory.ldap.core.LDAPConstants._
import com.normation.inventory.ldap.core.LDAPConstants.A_PROCESS
import com.normation.rudder.domain.RudderLDAPConstants.A_NODE_GROUP_UUID
import com.normation.rudder.domain.RudderLDAPConstants.A_NODE_PROPERTY
import com.normation.rudder.domain.RudderLDAPConstants.A_STATE
import com.normation.rudder.domain.nodes.NodeState
import org.joda.time.DateTime
import scala.collection.SortedMap
import zio.Chunk

class NodeQueryCriteriaData(getGroups: () => IOResult[Chunk[SubGroupChoice]]) {

  implicit class IterableToChunk[A](it: Iterable[A]) {
    def toChunk: Chunk[A] = Chunk.fromIterable(it)
  }

  implicit class OptionToChunk[A](opt: Option[A]) {
    def toChunk: Chunk[A] = Chunk.fromIterable(opt)
  }

  //  private val licenseObjectCriterion = ObjectCriterion(
//    "licence",
//    Chunk(
//      Criterion[DateTime](A_LICENSE_EXP, DateComparator),
//      Criterion[String](A_LICENSE_NAME, StringComparator),
//      Criterion[String](A_LICENSE_PRODUCT_ID, StringComparator),
//      Criterion[String](A_LICENSE_PRODUCT_KEY, StringComparator)
//    )
//  )

  val criteria = Chunk(
    ObjectCriterion(
      OC_MACHINE,
      Chunk(
        Criterion[MachineType]("machineType", MachineComparator, _.machine.toChunk.map(_.provider)),
        Criterion[String](A_MACHINE_UUID, StringComparator, _.machine.toChunk.map(_.id.value)),
        Criterion[String](A_NAME, StringComparator, _ => Chunk.empty),
        Criterion[String](A_DESCRIPTION, StringComparator, _ => Chunk.empty),
        Criterion[String](A_MB_UUID, StringComparator, _ => Chunk.empty),
        Criterion[String](A_MANUFACTURER, StringComparator, _.machine.toChunk.flatMap(_.manufacturer.map(_.name))),
        Criterion[String](A_SERIAL_NUMBER, StringComparator, _.machine.toChunk.flatMap(_.systemSerial))
      )
    ),
    ObjectCriterion(
      OC_MEMORY,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator, _.memories.flatMap(_.description)),
        Criterion[Int](A_QUANTITY, LongComparator, _.memories.map(_.quantity)),
        Criterion[String](A_NAME, StringComparator, _.memories.flatMap(_.name)),
        Criterion[MemorySize](A_MEMORY_CAPACITY, MemoryComparator, _.memories.flatMap(_.capacity)),
        Criterion[String](A_MEMORY_CAPTION, StringComparator, _.memories.flatMap(_.caption)),
        Criterion[String](A_MEMORY_SPEED, LongComparator, _.memories.flatMap(_.speed)),
        Criterion[Int](
          A_MEMORY_SLOT_NUMBER,
          LongComparator,
          _.memories.map(m => {
            try (m.slotNumber.toInt)
            catch {
              case e: NumberFormatException => 0
            }
          })
        ),
        Criterion[String](A_MEMORY_TYPE, StringComparator, _.memories.flatMap(_.memType)),
        Criterion[String](A_SERIAL_NUMBER, StringComparator, _.memories.flatMap(_.serialNumber))
      )
    ),
    ObjectCriterion(
      OC_STORAGE,
      Chunk(
        Criterion[String](A_NAME, StringComparator, _.storages.map(_.name)),
        Criterion[String](A_DESCRIPTION, StringComparator, _.storages.flatMap(_.description)),
        Criterion[String](A_MODEL, StringComparator, _.storages.flatMap(_.model)),
        Criterion[String](A_SERIAL_NUMBER, StringComparator, _.storages.flatMap(_.serialNumber)),
        Criterion[String](A_FIRMWARE, StringComparator, _.storages.flatMap(_.firmware)),
        Criterion[Long](A_QUANTITY, LongComparator, _.storages.map(_.quantity)),
        Criterion[String](A_SME_TYPE, StringComparator, _.storages.flatMap(_.sType)),
        Criterion[String](A_MANUFACTURER, StringComparator, _.storages.flatMap(_.manufacturer.map(_.name))),
        Criterion[MemorySize](A_STORAGE_SIZE, MemoryComparator, _.storages.flatMap(_.size)),
        Criterion[String](A_STORAGE_FIRMWARE, StringComparator, _.storages.flatMap(_.firmware))
      )
    ),
    ObjectCriterion(
      OC_BIOS,
      Chunk(
        Criterion[String](A_BIOS_NAME, StringComparator, _.bios.map(_.name)),
        Criterion[String](A_DESCRIPTION, StringComparator, _.bios.flatMap(_.description)),
        Criterion[Int](A_QUANTITY, LongComparator, _.bios.map(_.quantity)),
        Criterion[String](A_SOFT_VERSION, StringComparator, _.bios.flatMap(_.version.map(_.value))),
        Criterion[DateTime](A_RELEASE_DATE, DateComparator, _.bios.flatMap(_.releaseDate)),
        Criterion[String](A_EDITOR, StringComparator, _.bios.flatMap(_.editor.map(_.name)))
      )
    ),
    ObjectCriterion(
      OC_CONTROLLER,
      Chunk(
        Criterion[String](A_CONTROLLER_NAME, StringComparator, _.controllers.map(_.name)),
        Criterion[String](A_DESCRIPTION, StringComparator, _.controllers.flatMap(_.description)),
        Criterion[String](A_SME_TYPE, StringComparator, _.controllers.flatMap(_.cType)),
        Criterion[String](A_MANUFACTURER, StringComparator, _.controllers.flatMap(_.manufacturer.map(_.name))),
        Criterion[Int](A_QUANTITY, LongComparator, _.controllers.map(_.quantity))
      )
    ),
    ObjectCriterion(
      OC_PORT,
      Chunk(
        Criterion[String](A_PORT_NAME, StringComparator, _.ports.map(_.name)),
        Criterion[String](A_DESCRIPTION, StringComparator, _.ports.flatMap(_.description)),
        Criterion[String](A_SME_TYPE, StringComparator, _.ports.flatMap(_.pType)),
        Criterion[Int](A_QUANTITY, LongComparator, _.ports.map(_.quantity))
      )
    ),
    ObjectCriterion(
      OC_PROCESSOR,
      Chunk(
        Criterion[String](A_PROCESSOR_NAME, StringComparator, _.processors.map(_.name)),
        Criterion[String](A_DESCRIPTION, StringComparator, _.processors.flatMap(_.description)),
        Criterion[Int](A_QUANTITY, LongComparator, _.processors.map(_.quantity)),
        Criterion[String](A_MODEL, StringComparator, _.processors.flatMap(_.model.map(_.toString))),
        Criterion[String](A_MANUFACTURER, StringComparator, _.processors.flatMap(_.manufacturer.map(_.name))),
        Criterion[Int](A_PROCESSOR_SPEED, LongComparator, _.processors.flatMap(_.speed)),
        Criterion[String](A_PROCESSOR_STEPPING, StringComparator, _.processors.flatMap(_.stepping.map(_.toString))),
        Criterion[String](A_PROCESSOR_FAMILLY, StringComparator, _.processors.flatMap(_.family.map(_.toString))),
        Criterion[String](A_PROCESSOR_FAMILY_NAME, StringComparator, _.processors.flatMap(_.familyName)),
        Criterion[String](A_THREAD, StringComparator, _.processors.flatMap(_.thread.map(_.toString))),
        Criterion[String](A_CORE, StringComparator, _.processors.flatMap(_.core.map(_.toString)))
      )
    ),
    ObjectCriterion(
      OC_SLOT,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator, _.slots.flatMap(_.description)),
        Criterion[Int](A_QUANTITY, LongComparator, _.slots.map(_.quantity)),
        Criterion[String](A_STATUS, StringComparator, _.slots.flatMap(_.status)),
        Criterion[String](A_SLOT_NAME, StringComparator, _.slots.map(_.name))
      )
    ),
    ObjectCriterion(
      OC_SOUND,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator, _.sounds.flatMap(_.description)),
        Criterion[Int](A_QUANTITY, LongComparator, _.sounds.map(_.quantity)),
        Criterion[String](A_SOUND_NAME, StringComparator, _.sounds.map(_.name))
      )
    ),
    ObjectCriterion(
      OC_VIDEO,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator, _.videos.flatMap(_.description)),
        Criterion[Int](A_QUANTITY, LongComparator, _.videos.map(_.quantity)),
        Criterion[String](A_VIDEO_NAME, StringComparator, _.videos.map(_.name)),
        Criterion[String](A_VIDEO_CHIPSET, StringComparator, _.videos.flatMap(_.chipset)),
        Criterion[String](A_VIDEO_RESOLUTION, StringComparator, _.videos.flatMap(_.resolution)),
        Criterion[MemorySize](A_MEMORY_CAPACITY, MemoryComparator, _.videos.flatMap(_.memory))
      )
    ),
    ObjectCriterion(
      OC_NODE,
      Chunk(
        Criterion[String]("OS", NodeOstypeComparator, n => Chunk(n.os.os.kernelName)),
        Criterion[String](A_NODE_UUID, StringComparator, n => Chunk(n.id.value)),
        Criterion[String](A_HOSTNAME, StringComparator, n => Chunk(n.fqdn)),
        Criterion[String](A_OS_NAME, NodeOsNameComparator, n => Chunk(n.os.os.name)),
        Criterion[String](A_OS_FULL_NAME, OrderedStringComparator, n => Chunk(n.os.fullName)),
        Criterion[String](A_OS_VERSION, OrderedStringComparator, n => Chunk(n.os.version.value)),
        Criterion[String](A_OS_SERVICE_PACK, OrderedStringComparator, _.os.servicePack.toChunk),
        Criterion[String](A_OS_KERNEL_VERSION, OrderedStringComparator, n => Chunk(n.os.kernelVersion.value)),
        Criterion[String](A_ARCH, StringComparator, _.archDescription.toChunk),
        Criterion[NodeState](A_STATE, NodeStateComparator, n => Chunk(n.rudderSettings.state)),
        Criterion[MemorySize](A_OS_RAM, MemoryComparator, _.ram.toChunk),
        Criterion[MemorySize](A_OS_SWAP, MemoryComparator, _.swap.toChunk),
        Criterion[String](A_AGENTS_NAME, AgentComparator, n => Chunk(n.rudderAgent.tpe.id)),
        Criterion[String](A_ACCOUNT, StringComparator, _.accounts),
        Criterion[String](A_LIST_OF_IP, NodeIpListComparator, _.ipAddresses.map(_.inet)),
        Criterion[String](A_ROOT_USER, StringComparator, n => Chunk(n.rudderAgent.user)),
        Criterion[DateTime](A_INVENTORY_DATE, DateComparator, n => Chunk(n.lastInventoryDate)),
        Criterion[String](A_POLICY_SERVER_UUID, StringComparator, n => Chunk(n.rudderSettings.policyServerId.value))
      )
    ),
    ObjectCriterion(
      OC_SOFTWARE,
      Chunk(
        Criterion[String](A_NAME, StringComparator, _.software.map(_.name)),
        Criterion[String](A_SOFT_VERSION, StringComparator, _.software.map(_.version.toVersionString)),
        Criterion(A_EDITOR, EditorComparator, _.software.flatMap(_.publisher)),
        Criterion[DateTime](A_LICENSE_EXP, DateComparator, _.software.flatMap(_.expirationDate)),
        Criterion[String](A_LICENSE_NAME, StringComparator, _.software.flatMap(_.licenseName)),
        Criterion[String](A_LICENSE_PRODUCT_ID, StringComparator, _.software.flatMap(_.productId)),
        Criterion[String](A_LICENSE_PRODUCT_KEY, StringComparator, _.software.flatMap(_.productKey))
      )
    ),
    ObjectCriterion(
      OC_NET_IF,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator, _.networks.flatMap(_.description)),
        Criterion[String](A_NETWORK_NAME, StringComparator, _.networks.map(_.name)),
        Criterion[String](A_NETIF_ADDRESS, StringComparator, _.networks.flatMap(_.ifAddresses.map(_.getHostAddress))),
        Criterion[String](A_NETIF_DHCP, StringComparator, _.networks.flatMap(_.ifDhcp.map(_.getHostAddress))),
        Criterion[String](A_NETIF_GATEWAY, StringComparator, _.networks.flatMap(_.ifGateway.map(_.getHostAddress))),
        Criterion[String](A_NETIF_MASK, StringComparator, _.networks.flatMap(_.ifMask.map(_.getHostAddress))),
        Criterion[String](A_NETIF_SUBNET, StringComparator, _.networks.flatMap(_.ifSubnet.map(_.getHostAddress))),
        Criterion[String](A_NETIF_MAC, StringComparator, _.networks.flatMap(_.macAddress)),
        Criterion[String](A_NETIF_TYPE, StringComparator, _.networks.flatMap(_.ifType)),
        Criterion[String](A_NETIF_TYPE_MIB, StringComparator, _.networks.flatMap(_.typeMib))
      )
    ),
    ObjectCriterion(
      OC_FS,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator, _.fileSystems.flatMap(_.description)),
        Criterion[String](A_NAME, StringComparator, _.fileSystems.flatMap(_.name)),
        Criterion[String](A_MOUNT_POINT, StringComparator, _.fileSystems.map(_.mountPoint)),
        Criterion[Int](A_FILE_COUNT, LongComparator, _.fileSystems.flatMap(_.fileCount)),
        Criterion[MemorySize](A_FREE_SPACE, MemoryComparator, _.fileSystems.flatMap(_.freeSpace)),
        Criterion[MemorySize](A_TOTAL_SPACE, MemoryComparator, _.fileSystems.flatMap(_.totalSpace))
      )
    ),
    ObjectCriterion(
      A_PROCESS,
      Chunk(
        Criterion[Int]("pid", LongComparator, _.processes.map(_.pid)),
        Criterion[String]("commandName", StringComparator, _.processes.flatMap(_.commandName)),
        Criterion[Float]("cpuUsage", StringComparator, _.processes.flatMap(_.cpuUsage)),
        Criterion[Float]("memory", LongComparator, _.processes.flatMap(_.memory)),
        Criterion[String]("tty", StringComparator, _.processes.flatMap(_.tty)),
        Criterion[Double]("virtualMemory", LongComparator, _.processes.flatMap(_.virtualMemory)),
        Criterion[String]("started", StringComparator, _.processes.flatMap(_.started)),
        Criterion[String]("user", StringComparator, _.processes.flatMap(_.user))
      )
    )
    ObjectCriterion(
      OC_VM_INFO,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator, _.vms.flatMap(_.description)),
        Criterion[String](A_VM_TYPE, StringComparator, _.vms.flatMap(_.vmtype)),
        Criterion[String](A_VM_OWNER, StringComparator, _.vms.flatMap(_.owner)),
        Criterion[String](A_VM_STATUS, StringComparator, _.vms.flatMap(_.status)),
        Criterion[Int](A_VM_CPU, LongComparator, _.vms.flatMap(_.vcpu)),
        Criterion[String](A_VM_MEMORY, LongComparator, _.vms.flatMap(_.memory)),
        Criterion[String](A_VM_ID, StringComparator, _.vms.flatMap(_.uuid.value)),
        Criterion[String](A_VM_SUBSYSTEM, StringComparator, _.vms.flatMap(_.subsystem)),
        Criterion[String](A_VM_NAME, StringComparator, _.vms.flatMap(_.name))
      )
    ),
    ObjectCriterion(
      A_EV,
      Chunk(
        Criterion("name.value", NameValueComparator(A_EV))
      )
    ),
    ObjectCriterion(
      A_NODE_PROPERTY,
      Chunk(
        Criterion("name.value", NodePropertyComparator(A_NODE_PROPERTY))
      )
    ),
    ObjectCriterion(
      "group",
      Chunk(
        Criterion(A_NODE_GROUP_UUID, new SubGroupComparator(getGroups))
      )
    )
  )

  val criteriaMap: SortedMap[String, ObjectCriterion] = SortedMap.from(criteria.map(c => (c.objectType, c)))
}
