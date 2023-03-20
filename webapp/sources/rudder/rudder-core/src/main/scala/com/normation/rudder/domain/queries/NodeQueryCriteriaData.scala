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
        Criterion[String](A_MANUFACTURER, StringComparator, _.machine.toChunk.flatMap(_.manufacturer)),
        Criterion[String](A_SERIAL_NUMBER, StringComparator, _.machine.toChunk.flatMap(_.systemSerial))
      )
    ),
    ObjectCriterion(
      OC_MEMORY,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator, _.memories.flatMap(_.description)),
        Criterion[String](A_MODEL, StringComparator, _.memories.flatMap(_.)),
        Criterion[String](A_SERIAL_NUMBER, StringComparator),
        Criterion[String](A_FIRMWARE, StringComparator),
        Criterion[Long](A_QUANTITY, LongComparator),
        Criterion[String](A_SME_TYPE, StringComparator),
        Criterion[Long](A_STATUS, LongComparator),
        Criterion[String](A_MANUFACTURER, StringComparator),
        Criterion[Long](A_MEMORY_SLOT_NUMBER, LongComparator),
        Criterion[String](A_NAME, StringComparator),
        Criterion[MemorySize](A_MEMORY_CAPACITY, MemoryComparator, _.memories.flatMap(_.capacity)),
        Criterion[String](A_MEMORY_CAPTION, StringComparator),
        Criterion[Long](A_MEMORY_SPEED, LongComparator),
        Criterion[String](A_MEMORY_TYPE, StringComparator)
      )
    ),
    ObjectCriterion(
      OC_STORAGE,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_MODEL, StringComparator),
        Criterion[String](A_SERIAL_NUMBER, StringComparator),
        Criterion[String](A_FIRMWARE, StringComparator),
        Criterion[Long](A_QUANTITY, LongComparator),
        Criterion[String](A_SME_TYPE, StringComparator),
        Criterion[Long](A_STATUS, LongComparator),
        Criterion[String](A_MANUFACTURER, StringComparator),
        Criterion[String](A_STORAGE_NAME, StringComparator),
        Criterion[MemorySize](A_STORAGE_SIZE, MemoryComparator),
        Criterion[String](A_STORAGE_FIRMWARE, StringComparator)
      )
    ),
    ObjectCriterion(
      OC_BIOS,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_MODEL, StringComparator),
        Criterion[String](A_SERIAL_NUMBER, StringComparator),
        Criterion[String](A_FIRMWARE, StringComparator),
        Criterion[Long](A_QUANTITY, LongComparator),
        Criterion[String](A_SME_TYPE, StringComparator),
        Criterion[Long](A_STATUS, LongComparator),
        Criterion[String](A_MANUFACTURER, StringComparator),
        Criterion[String](A_BIOS_NAME, StringComparator),
        Criterion[String](A_RELEASE_DATE, StringComparator),
        Criterion[String](A_EDITOR, StringComparator),
        Criterion[String](A_SOFT_VERSION, StringComparator)
      ) ++
      licenseObjectCriterion.criteria
    ),
    ObjectCriterion(
      OC_CONTROLLER,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_MODEL, StringComparator),
        Criterion[String](A_SERIAL_NUMBER, StringComparator),
        Criterion[String](A_FIRMWARE, StringComparator),
        Criterion[Long](A_QUANTITY, LongComparator),
        Criterion[String](A_SME_TYPE, StringComparator),
        Criterion[Long](A_STATUS, LongComparator),
        Criterion[String](A_MANUFACTURER, StringComparator),
        Criterion[String](A_CONTROLLER_NAME, StringComparator)
      )
    ),
    ObjectCriterion(
      OC_PORT,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_MODEL, StringComparator),
        Criterion[String](A_SERIAL_NUMBER, StringComparator),
        Criterion[String](A_FIRMWARE, StringComparator),
        Criterion[Long](A_QUANTITY, LongComparator),
        Criterion[String](A_SME_TYPE, StringComparator),
        Criterion[Long](A_STATUS, LongComparator),
        Criterion[String](A_MANUFACTURER, StringComparator),
        Criterion[String](A_PORT_NAME, StringComparator)
      )
    ),
    ObjectCriterion(
      OC_PROCESSOR,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_MODEL, StringComparator),
        Criterion[String](A_SERIAL_NUMBER, StringComparator),
        Criterion[String](A_FIRMWARE, StringComparator),
        Criterion[Long](A_QUANTITY, LongComparator),
        Criterion[String](A_SME_TYPE, StringComparator),
        Criterion[Long](A_STATUS, LongComparator),
        Criterion[String](A_MANUFACTURER, StringComparator),
        Criterion[String](A_PROCESSOR_NAME, StringComparator),
        Criterion[Long](A_PROCESSOR_SPEED, LongComparator),
        Criterion[String](A_PROCESSOR_STEPPING, StringComparator),
        Criterion[String](A_PROCESSOR_FAMILLY, StringComparator),
        Criterion[String](A_PROCESSOR_FAMILY_NAME, StringComparator),
        Criterion[String](A_THREAD, StringComparator),
        Criterion[String](A_CORE, StringComparator)
      )
    ),
    ObjectCriterion(
      OC_SLOT,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_MODEL, StringComparator),
        Criterion[String](A_SERIAL_NUMBER, StringComparator),
        Criterion[String](A_FIRMWARE, StringComparator),
        Criterion[Long](A_QUANTITY, LongComparator),
        Criterion[String](A_SME_TYPE, StringComparator),
        Criterion[Long](A_STATUS, LongComparator),
        Criterion[String](A_MANUFACTURER, StringComparator),
        Criterion[String](A_SLOT_NAME, StringComparator)
      )
    ),
    ObjectCriterion(
      OC_SOUND,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_MODEL, StringComparator),
        Criterion[String](A_SERIAL_NUMBER, StringComparator),
        Criterion[String](A_FIRMWARE, StringComparator),
        Criterion[Long](A_QUANTITY, LongComparator),
        Criterion[String](A_SME_TYPE, StringComparator),
        Criterion[Long](A_STATUS, LongComparator),
        Criterion[String](A_MANUFACTURER, StringComparator),
        Criterion[String](A_SOUND_NAME, StringComparator)
      )
    ),
    ObjectCriterion(
      OC_VIDEO,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_MODEL, StringComparator),
        Criterion[String](A_SERIAL_NUMBER, StringComparator),
        Criterion[String](A_FIRMWARE, StringComparator),
        Criterion[Long](A_QUANTITY, LongComparator),
        Criterion[String](A_SME_TYPE, StringComparator),
        Criterion[Long](A_STATUS, LongComparator),
        Criterion[String](A_MANUFACTURER, StringComparator),
        Criterion[String](A_VIDEO_NAME, StringComparator),
        Criterion[String](A_VIDEO_CHIPSET, StringComparator),
        Criterion[String](A_VIDEO_RESOLUTION, StringComparator),
        Criterion[MemorySize](A_MEMORY_CAPACITY, MemoryComparator)
      )
    ),
    ObjectCriterion(
      OC_NODE,
      Chunk(
        Criterion("OS", NodeOstypeComparator),
        Criterion(A_NODE_UUID, NodeStringComparator(node => node.node.id.value)),
        Criterion(A_HOSTNAME, NodeStringComparator(node => node.hostname)),
        Criterion(A_OS_NAME, NodeOsNameComparator),
        Criterion(A_OS_FULL_NAME, OrderedStringComparator),
        Criterion(A_OS_VERSION, OrderedStringComparator),
        Criterion(A_OS_SERVICE_PACK, OrderedStringComparator),
        Criterion(A_OS_KERNEL_VERSION, OrderedStringComparator),
        Criterion[String](A_ARCH, StringComparator),
        Criterion(A_STATE, NodeStateComparator, Some("rudderNode")),
        Criterion[MemorySize](A_OS_RAM, MemoryComparator),
        Criterion[MemorySize](A_OS_SWAP, MemoryComparator),
        Criterion(A_AGENTS_NAME, AgentComparator),
        Criterion[String](A_ACCOUNT, StringComparator),
        Criterion(A_LIST_OF_IP, NodeIpListComparator),
        Criterion(A_ROOT_USER, NodeStringComparator(node => node.localAdministratorAccountName)),
        Criterion[DateTime](A_INVENTORY_DATE, DateComparator),
        Criterion(A_POLICY_SERVER_UUID, NodeStringComparator(node => node.policyServerId.value))
      )
    ),
    ObjectCriterion(
      OC_SOFTWARE,
      Chunk(
        Criterion[String](A_NAME, StringComparator),
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_SOFT_VERSION, StringComparator),
        Criterion[DateTime](A_RELEASE_DATE, DateComparator),
        Criterion(A_EDITOR, EditorComparator)
      ) ++
      licenseObjectCriterion.criteria
    ),
    ObjectCriterion(
      OC_NET_IF,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_NETWORK_NAME, StringComparator),
        Criterion[String](A_NETIF_ADDRESS, StringComparator),
        Criterion[String](A_NETIF_DHCP, StringComparator),
        Criterion[String](A_NETIF_GATEWAY, StringComparator),
        Criterion[String](A_NETIF_MASK, StringComparator),
        Criterion[String](A_NETIF_SUBNET, StringComparator),
        Criterion[String](A_NETIF_MAC, StringComparator),
        Criterion[String](A_NETIF_TYPE, StringComparator),
        Criterion[String](A_NETIF_TYPE_MIB, StringComparator)
      )
    ),
    ObjectCriterion(
      OC_FS,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_NAME, StringComparator),
        Criterion[String](A_MOUNT_POINT, StringComparator),
        Criterion[String](A_FILE_COUNT, StringComparator),
        Criterion[MemorySize](A_FREE_SPACE, MemoryComparator),
        Criterion[MemorySize](A_TOTAL_SPACE, MemoryComparator)
      )
    ),
    ObjectCriterion(
      A_PROCESS,
      Chunk(
        Criterion[String]("pid", JsonFixedKeyComparator(A_PROCESS, "pid", false)),
        Criterion[String]("commandName", JsonFixedKeyComparator(A_PROCESS, "commandName", true)),
        Criterion[String]("cpuUsage", JsonFixedKeyComparator(A_PROCESS, "cpuUsage", false)),
        Criterion[String]("memory", JsonFixedKeyComparator(A_PROCESS, "memory", false)),
        Criterion[String]("tty", JsonFixedKeyComparator(A_PROCESS, "tty", true)),
        Criterion[String]("virtualMemory", JsonFixedKeyComparator(A_PROCESS, "virtualMemory", false)),
        Criterion[String]("started", JsonFixedKeyComparator(A_PROCESS, "started", true)),
        Criterion[String]("user", JsonFixedKeyComparator(A_PROCESS, "user", true))
      )
    ),
    ObjectCriterion(
      OC_VM_INFO,
      Chunk(
        Criterion[String](A_DESCRIPTION, StringComparator),
        Criterion[String](A_VM_TYPE, StringComparator),
        Criterion[String](A_VM_OWNER, StringComparator),
        Criterion[String](A_VM_STATUS, StringComparator),
        Criterion[Long](A_VM_CPU, LongComparator),
        Criterion[Long](A_VM_MEMORY, LongComparator),
        Criterion[String](A_VM_ID, StringComparator),
        Criterion[String](A_VM_SUBSYSTEM, StringComparator),
        Criterion[String](A_VM_NAME, StringComparator)
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
