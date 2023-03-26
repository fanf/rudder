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
import com.normation.inventory.ldap.core.LDAPConstants._
import com.normation.inventory.ldap.core.LDAPConstants.A_PROCESS
import com.normation.rudder.domain.RudderLDAPConstants.A_NODE_GROUP_UUID
import com.normation.rudder.domain.RudderLDAPConstants.A_NODE_PROPERTY
import com.normation.rudder.domain.RudderLDAPConstants.A_STATE
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
//      Criterion(A_LICENSE_EXP, DateComparator),
//      Criterion(A_LICENSE_NAME, StringComparator),
//      Criterion(A_LICENSE_PRODUCT_ID, StringComparator),
//      Criterion(A_LICENSE_PRODUCT_KEY, StringComparator)
//    )
//  )

  val criteria = Chunk(
    ObjectCriterion(
      OC_MACHINE,
      Chunk(
        Criterion("machineType", MachineComparator, NodeCriterionMatcherString(_.machine.toChunk.map(_.provider.kind))),
        Criterion(A_MACHINE_UUID, StringComparator, NodeCriterionMatcherString(_.machine.toChunk.map(_.id.value))),
        Criterion(A_NAME, StringComparator, AlwaysFalse),
        Criterion(A_DESCRIPTION, StringComparator, AlwaysFalse),
        Criterion(A_MB_UUID, StringComparator, AlwaysFalse),
        Criterion(
          A_MANUFACTURER,
          StringComparator,
          NodeCriterionMatcherString(_.machine.toChunk.flatMap(_.manufacturer.map(_.name)))
        ),
        Criterion(A_SERIAL_NUMBER, StringComparator, NodeCriterionMatcherString(_.machine.toChunk.flatMap(_.systemSerial)))
      )
    ),
    ObjectCriterion(
      OC_MEMORY,
      Chunk(
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.memories.flatMap(_.description))),
        Criterion(A_QUANTITY, LongComparator, NodeCriterionMatcherInt(_.memories.map(_.quantity))),
        Criterion(A_NAME, StringComparator, NodeCriterionMatcherString(_.memories.flatMap(_.name))),
        Criterion(A_MEMORY_CAPACITY, MemoryComparator, NodeCriterionMatcherLong(_.memories.flatMap(_.capacity.map(_.size)))),
        Criterion(A_MEMORY_CAPTION, StringComparator, NodeCriterionMatcherString(_.memories.flatMap(_.caption))),
        Criterion(A_MEMORY_SPEED, LongComparator, NodeCriterionMatcherString(_.memories.flatMap(_.speed))),
        Criterion(A_MEMORY_SLOT_NUMBER, LongComparator, NodeCriterionMatcherString(_.memories.map(_.slotNumber))),
        Criterion(A_MEMORY_TYPE, StringComparator, NodeCriterionMatcherString(_.memories.flatMap(_.memType))),
        Criterion(A_SERIAL_NUMBER, StringComparator, NodeCriterionMatcherString(_.memories.flatMap(_.serialNumber)))
      )
    ),
    ObjectCriterion(
      OC_STORAGE,
      Chunk(
        Criterion(A_NAME, StringComparator, NodeCriterionMatcherString(_.storages.map(_.name))),
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.storages.flatMap(_.description))),
        Criterion(A_MODEL, StringComparator, NodeCriterionMatcherString(_.storages.flatMap(_.model))),
        Criterion(A_SERIAL_NUMBER, StringComparator, NodeCriterionMatcherString(_.storages.flatMap(_.serialNumber))),
        Criterion(A_FIRMWARE, StringComparator, NodeCriterionMatcherString(_.storages.flatMap(_.firmware))),
        Criterion(A_QUANTITY, LongComparator, NodeCriterionMatcherInt(_.storages.map(_.quantity))),
        Criterion(A_SME_TYPE, StringComparator, NodeCriterionMatcherString(_.storages.flatMap(_.sType))),
        Criterion(
          A_MANUFACTURER,
          StringComparator,
          NodeCriterionMatcherString(_.storages.flatMap(_.manufacturer.map(_.name)))
        ),
        Criterion(A_STORAGE_SIZE, MemoryComparator, NodeCriterionMatcherLong(_.storages.flatMap(_.size.map(_.size)))),
        Criterion(A_STORAGE_FIRMWARE, StringComparator, NodeCriterionMatcherString(_.storages.flatMap(_.firmware)))
      )
    ),
    ObjectCriterion(
      OC_BIOS,
      Chunk(
        Criterion(A_BIOS_NAME, StringComparator, NodeCriterionMatcherString(_.bios.map(_.name))),
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.bios.flatMap(_.description))),
        Criterion(A_QUANTITY, LongComparator, NodeCriterionMatcherInt(_.bios.map(_.quantity))),
        Criterion(A_SOFT_VERSION, StringComparator, NodeCriterionMatcherString(_.bios.flatMap(_.version.map(_.value)))),
        Criterion(A_RELEASE_DATE, DateComparator, NodeCriterionMatcherDate(_.bios.flatMap(_.releaseDate))),
        Criterion(A_EDITOR, StringComparator, NodeCriterionMatcherString(_.bios.flatMap(_.editor.map(_.name))))
      )
    ),
    ObjectCriterion(
      OC_CONTROLLER,
      Chunk(
        Criterion(A_CONTROLLER_NAME, StringComparator, NodeCriterionMatcherString(_.controllers.map(_.name))),
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.controllers.flatMap(_.description))),
        Criterion(A_SME_TYPE, StringComparator, NodeCriterionMatcherString(_.controllers.flatMap(_.cType))),
        Criterion(
          A_MANUFACTURER,
          StringComparator,
          NodeCriterionMatcherString(_.controllers.flatMap(_.manufacturer.map(_.name)))
        ),
        Criterion(A_QUANTITY, LongComparator, NodeCriterionMatcherInt(_.controllers.map(_.quantity)))
      )
    ),
    ObjectCriterion(
      OC_PORT,
      Chunk(
        Criterion(A_PORT_NAME, StringComparator, NodeCriterionMatcherString(_.ports.map(_.name))),
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.ports.flatMap(_.description))),
        Criterion(A_SME_TYPE, StringComparator, NodeCriterionMatcherString(_.ports.flatMap(_.pType))),
        Criterion(A_QUANTITY, LongComparator, NodeCriterionMatcherInt(_.ports.map(_.quantity)))
      )
    ),
    ObjectCriterion(
      OC_PROCESSOR,
      Chunk(
        Criterion(A_PROCESSOR_NAME, StringComparator, NodeCriterionMatcherString(_.processors.map(_.name))),
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.processors.flatMap(_.description))),
        Criterion(A_QUANTITY, LongComparator, NodeCriterionMatcherInt(_.processors.map(_.quantity))),
        Criterion(A_MODEL, StringComparator, NodeCriterionMatcherString(_.processors.flatMap(_.model.map(_.toString)))),
        Criterion(
          A_MANUFACTURER,
          StringComparator,
          NodeCriterionMatcherString(_.processors.flatMap(_.manufacturer.map(_.name)))
        ),
        Criterion(A_PROCESSOR_SPEED, LongComparator, NodeCriterionMatcherInt(_.processors.flatMap(_.speed))),
        Criterion(
          A_PROCESSOR_STEPPING,
          StringComparator,
          NodeCriterionMatcherString(_.processors.flatMap(_.stepping.map(_.toString)))
        ),
        Criterion(
          A_PROCESSOR_FAMILLY,
          StringComparator,
          NodeCriterionMatcherString(_.processors.flatMap(_.family.map(_.toString)))
        ),
        Criterion(A_PROCESSOR_FAMILY_NAME, StringComparator, NodeCriterionMatcherString(_.processors.flatMap(_.familyName))),
        Criterion(A_THREAD, StringComparator, NodeCriterionMatcherString(_.processors.flatMap(_.thread.map(_.toString)))),
        Criterion(A_CORE, StringComparator, NodeCriterionMatcherString(_.processors.flatMap(_.core.map(_.toString))))
      )
    ),
    ObjectCriterion(
      OC_SLOT,
      Chunk(
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.slots.flatMap(_.description))),
        Criterion(A_QUANTITY, LongComparator, NodeCriterionMatcherInt(_.slots.map(_.quantity))),
        Criterion(A_STATUS, StringComparator, NodeCriterionMatcherString(_.slots.flatMap(_.status))),
        Criterion(A_SLOT_NAME, StringComparator, NodeCriterionMatcherString(_.slots.map(_.name)))
      )
    ),
    ObjectCriterion(
      OC_SOUND,
      Chunk(
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.sounds.flatMap(_.description))),
        Criterion(A_QUANTITY, LongComparator, NodeCriterionMatcherInt(_.sounds.map(_.quantity))),
        Criterion(A_SOUND_NAME, StringComparator, NodeCriterionMatcherString(_.sounds.map(_.name)))
      )
    ),
    ObjectCriterion(
      OC_VIDEO,
      Chunk(
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.videos.flatMap(_.description))),
        Criterion(A_QUANTITY, LongComparator, NodeCriterionMatcherInt(_.videos.map(_.quantity))),
        Criterion(A_VIDEO_NAME, StringComparator, NodeCriterionMatcherString(_.videos.map(_.name))),
        Criterion(A_VIDEO_CHIPSET, StringComparator, NodeCriterionMatcherString(_.videos.flatMap(_.chipset))),
        Criterion(A_VIDEO_RESOLUTION, StringComparator, NodeCriterionMatcherString(_.videos.flatMap(_.resolution))),
        Criterion(A_MEMORY_CAPACITY, MemoryComparator, NodeCriterionMatcherLong(_.videos.flatMap(_.memory.map(_.size))))
      )
    ),
    ObjectCriterion(
      OC_NODE,
      Chunk(
        Criterion("OS", NodeOstypeComparator, NodeCriterionMatcherString(n => Chunk(n.os.os.kernelName))),
        Criterion(A_NODE_UUID, StringComparator, NodeCriterionMatcherString(n => Chunk(n.id.value))),
        Criterion(A_HOSTNAME, StringComparator, NodeCriterionMatcherString(n => Chunk(n.fqdn))),
        Criterion(A_OS_NAME, NodeOsNameComparator, NodeCriterionMatcherString(n => Chunk(n.os.os.name))),
        Criterion(A_OS_FULL_NAME, OrderedStringComparator, NodeCriterionMatcherString(n => Chunk(n.os.fullName))),
        Criterion(A_OS_VERSION, OrderedStringComparator, NodeCriterionMatcherString(n => Chunk(n.os.version.value))),
        Criterion(A_OS_SERVICE_PACK, OrderedStringComparator, NodeCriterionMatcherString(_.os.servicePack.toChunk)),
        Criterion(A_OS_KERNEL_VERSION, OrderedStringComparator, NodeCriterionMatcherString(n => Chunk(n.os.kernelVersion.value))),
        Criterion(A_ARCH, StringComparator, NodeCriterionMatcherString(_.archDescription.toChunk)),
        Criterion(A_STATE, NodeStateComparator, NodeCriterionMatcherString(n => Chunk(n.rudderSettings.state.name))),
        Criterion(A_OS_RAM, MemoryComparator, NodeCriterionMatcherLong(_.ram.map(_.size).toChunk)),
        Criterion(A_OS_SWAP, MemoryComparator, NodeCriterionMatcherLong(_.swap.map(_.size).toChunk)),
        Criterion(A_AGENTS_NAME, AgentComparator, NodeCriterionMatcherString(n => Chunk(n.rudderAgent.tpe.id))),
        Criterion(A_ACCOUNT, StringComparator, NodeCriterionMatcherString(_.accounts)),
        Criterion(A_LIST_OF_IP, NodeIpListComparator, NodeCriterionMatcherString(_.ipAddresses.map(_.inet))),
        Criterion(A_ROOT_USER, StringComparator, NodeCriterionMatcherString(n => Chunk(n.rudderAgent.user))),
        Criterion(A_INVENTORY_DATE, DateComparator, NodeCriterionMatcherDate(n => Chunk(n.lastInventoryDate))),
        Criterion(
          A_POLICY_SERVER_UUID,
          StringComparator,
          NodeCriterionMatcherString(n => Chunk(n.rudderSettings.policyServerId.value))
        )
      )
    ),
    ObjectCriterion(
      OC_SOFTWARE,
      Chunk(
        Criterion(A_NAME, StringComparator, NodeCriterionMatcherString(_.software.map(_.name))),
        Criterion(A_SOFT_VERSION, StringComparator, NodeCriterionMatcherString(_.software.map(_.version.toVersionString))),
        Criterion(A_EDITOR, EditorComparator, NodeCriterionMatcherString(_.software.flatMap(_.publisher))),
        Criterion(A_LICENSE_EXP, DateComparator, NodeCriterionMatcherDate(_.software.flatMap(_.expirationDate))),
        Criterion(A_LICENSE_NAME, StringComparator, NodeCriterionMatcherString(_.software.flatMap(_.licenseName))),
        Criterion(A_LICENSE_PRODUCT_ID, StringComparator, NodeCriterionMatcherString(_.software.flatMap(_.productId))),
        Criterion(A_LICENSE_PRODUCT_KEY, StringComparator, NodeCriterionMatcherString(_.software.flatMap(_.productKey)))
      )
    ),
    ObjectCriterion(
      OC_NET_IF,
      Chunk(
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.networks.flatMap(_.description))),
        Criterion(A_NETWORK_NAME, StringComparator, NodeCriterionMatcherString(_.networks.map(_.name))),
        Criterion(
          A_NETIF_ADDRESS,
          StringComparator,
          NodeCriterionMatcherString(_.networks.flatMap(_.ifAddresses.map(_.getHostAddress)))
        ),
        Criterion(
          A_NETIF_DHCP,
          StringComparator,
          NodeCriterionMatcherString(_.networks.flatMap(_.ifDhcp.map(_.getHostAddress)))
        ),
        Criterion(
          A_NETIF_GATEWAY,
          StringComparator,
          NodeCriterionMatcherString(_.networks.flatMap(_.ifGateway.map(_.getHostAddress)))
        ),
        Criterion(
          A_NETIF_MASK,
          StringComparator,
          NodeCriterionMatcherString(_.networks.flatMap(_.ifMask.map(_.getHostAddress)))
        ),
        Criterion(
          A_NETIF_SUBNET,
          StringComparator,
          NodeCriterionMatcherString(_.networks.flatMap(_.ifSubnet.map(_.getHostAddress)))
        ),
        Criterion(A_NETIF_MAC, StringComparator, NodeCriterionMatcherString(_.networks.flatMap(_.macAddress))),
        Criterion(A_NETIF_TYPE, StringComparator, NodeCriterionMatcherString(_.networks.flatMap(_.ifType))),
        Criterion(A_NETIF_TYPE_MIB, StringComparator, NodeCriterionMatcherString(_.networks.flatMap(_.typeMib)))
      )
    ),
    ObjectCriterion(
      OC_FS,
      Chunk(
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.fileSystems.flatMap(_.description))),
        Criterion(A_NAME, StringComparator, NodeCriterionMatcherString(_.fileSystems.flatMap(_.name))),
        Criterion(A_MOUNT_POINT, StringComparator, NodeCriterionMatcherString(_.fileSystems.map(_.mountPoint))),
        Criterion(A_FILE_COUNT, LongComparator, NodeCriterionMatcherInt(_.fileSystems.flatMap(_.fileCount))),
        Criterion(A_FREE_SPACE, MemoryComparator, NodeCriterionMatcherLong(_.fileSystems.flatMap(_.freeSpace.map(_.size)))),
        Criterion(A_TOTAL_SPACE, MemoryComparator, NodeCriterionMatcherLong(_.fileSystems.flatMap(_.totalSpace.map(_.size))))
      )
    ),
    ObjectCriterion(
      A_PROCESS,
      Chunk(
        Criterion("pid", JsonFixedKeyComparator(A_PROCESS, "pid", false), NodeCriterionMatcherInt(_.processes.map(_.pid))),
        Criterion(
          "commandName",
          JsonFixedKeyComparator(A_PROCESS, "commandName", true),
          NodeCriterionMatcherString(_.processes.flatMap(_.commandName))
        ),
        Criterion(
          "cpuUsage",
          JsonFixedKeyComparator(A_PROCESS, "cpuUsage", false),
          NodeCriterionMatcherString(_.processes.flatMap(_.cpuUsage.map(_.toString)))
        ),
        Criterion(
          "memory",
          JsonFixedKeyComparator(A_PROCESS, "memory", false),
          NodeCriterionMatcherFloat(_.processes.flatMap(_.memory))
        ),
        Criterion("tty", JsonFixedKeyComparator(A_PROCESS, "tty", true), NodeCriterionMatcherString(_.processes.flatMap(_.tty))),
        Criterion(
          "virtualMemory",
          JsonFixedKeyComparator(A_PROCESS, "virtualMemory", false),
          NodeCriterionMatcherDouble(_.processes.flatMap(_.virtualMemory))
        ),
        Criterion(
          "started",
          JsonFixedKeyComparator(A_PROCESS, "started", true),
          NodeCriterionMatcherString(_.processes.flatMap(_.started))
        ),
        Criterion(
          "user",
          JsonFixedKeyComparator(A_PROCESS, "user", true),
          NodeCriterionMatcherString(_.processes.flatMap(_.user))
        )
      )
    ),
    ObjectCriterion(
      OC_VM_INFO,
      Chunk(
        Criterion(A_DESCRIPTION, StringComparator, NodeCriterionMatcherString(_.vms.flatMap(_.description))),
        Criterion(A_VM_TYPE, StringComparator, NodeCriterionMatcherString(_.vms.flatMap(_.vmtype))),
        Criterion(A_VM_OWNER, StringComparator, NodeCriterionMatcherString(_.vms.flatMap(_.owner))),
        Criterion(A_VM_STATUS, StringComparator, NodeCriterionMatcherString(_.vms.flatMap(_.status))),
        Criterion(A_VM_CPU, LongComparator, NodeCriterionMatcherInt(_.vms.flatMap(_.vcpu))),
        Criterion(A_VM_MEMORY, LongComparator, NodeCriterionMatcherString(_.vms.flatMap(_.memory))),
        Criterion(A_VM_ID, StringComparator, NodeCriterionMatcherString(_.vms.map(_.uuid.value))),
        Criterion(A_VM_SUBSYSTEM, StringComparator, NodeCriterionMatcherString(_.vms.flatMap(_.subsystem))),
        Criterion(A_VM_NAME, StringComparator, NodeCriterionMatcherString(_.vms.flatMap(_.name)))
      )
    ),
    ObjectCriterion(
      A_EV,
      Chunk(
        Criterion(
          "name.value",
          NameValueComparator(A_EV),
          // TODO: we can now provide a real alternative to that but keep it for compat
          NodeCriterionMatcherString(_.environmentVariables.map { case (k, v) => s""""name":"${k}","value":"${v}"""" })
        )
      )
    ),
    ObjectCriterion(
      A_NODE_PROPERTY,
      Chunk(
        Criterion("name.value", NodePropertyComparator(A_NODE_PROPERTY), NodePropertiesCriterionMatcher)
      )
    ),
    ObjectCriterion(
      "group",
      Chunk(
        Criterion(
          A_NODE_GROUP_UUID,
          new SubGroupComparator(getGroups),
          AlwaysFalse
        ) // TODO: check that this one is solved in another way
      )
    )
  )

  val criteriaMap: SortedMap[String, ObjectCriterion] = SortedMap.from(criteria.map(c => (c.objectType, c)))
}
