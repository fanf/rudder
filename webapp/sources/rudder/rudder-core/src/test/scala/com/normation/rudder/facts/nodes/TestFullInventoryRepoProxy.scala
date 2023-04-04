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

package com.normation.rudder.facts.nodes

import com.github.ghik.silencer.silent

import com.normation.errors._
import com.normation.inventory.domain._
import com.normation.inventory.services.core.FullInventoryRepository
import com.normation.inventory.services.core.MachineRepository
import com.normation.ldap.listener.InMemoryDsConnectionProvider
import com.normation.ldap.sdk.RoLDAPConnection
import com.normation.ldap.sdk.RwLDAPConnection

import com.normation.zio.ZioRuntime
import com.softwaremill.quicklens._
import com.unboundid.ldap.sdk.DN
import com.unboundid.ldap.sdk.Modification
import com.unboundid.ldap.sdk.ModificationType
import org.junit.runner._
import org.specs2.matcher.MatchResult
import org.specs2.mutable._
import org.specs2.runner._

import zio._

final case class SystemError(cause: Throwable) extends RudderError {
  def msg = "Error in test"
}

/**
 * A simple test class to check that the demo data file is up to date
 * with the schema (there may still be a desynchronization if both
 * demo-data, test data and test schema for UnboundID are not synchronized
 * with OpenLDAP Schema).
 */
@silent("a type was inferred to be `\\w+`; this may indicate a programming error.")
@RunWith(classOf[JUnitRunner])
class TestInventory extends Specification {

  implicit class RunThing[E, T](thing: ZIO[Any, E, T])      {
    def testRun = ZioRuntime.unsafeRun(thing.either)
  }
  implicit class RunOptThing[A](thing: IOResult[Option[A]]) {
    def testRunGet: A = ZioRuntime.unsafeRun(thing.either) match {
      case Right(Some(a)) => a
      case Right(None)    => throw new RuntimeException(s"Error in test: found None, expected Some")
      case Left(err)      => throw new RuntimeException(s"Error in test: ${err}")
    }
  }

  implicit class TestIsOK[E, T](thing: ZIO[Any, E, T]) {
    def isOK = thing.testRun must beRight
  }

  implicit class ForceGetE[E, A](opt: Either[E, A]) {
    def forceGet: A = opt match {
      case Right(x)  => x
      case Left(err) => throw new Exception(s"error in Test: ${err}")
    }
  }

  // needed because the in memory LDAP server is not used with connection pool
  sequential

  val allStatus = Seq(RemovedInventory, PendingInventory, AcceptedInventory)

  // shortcut to create a machine with the name has ID in the given status
  def machine(name: String, status: InventoryStatus)                                         = MachineInventory(
    MachineUuid(name),
    status,
    PhysicalMachineType,
    Some(s"name for ${name}"),
    None,
    None,
    None,
    None,
    None,
    Nil,
    Nil,
    Nil,
    Nil,
    Nil,
    Nil,
    Nil,
    Nil,
    Nil
  )
  // shortcut to create a node with the name has ID and the given machine, in the
  // given status, has container.
  def node(name: String, status: InventoryStatus, container: (MachineUuid, InventoryStatus)) = NodeInventory(
    NodeSummary(
      NodeId(name),
      status,
      "root",
      "localhost",
      Linux(
        Debian,
        "foo",
        new Version("1.0"),
        None,
        new Version("1.0")
      ),
      NodeId("root"),
      CertifiedKey
    ),
    machineId = Some(container)
  )

  def full(n: NodeInventory, m: MachineInventory) = FullInventory(n, Some(m))

  val repo: FullInventoryRepository[Unit] with MachineRepository[Unit] = ???

  "Saving, finding and moving machine around" should {

    "correctly save and find a machine based on it's id (when only on one place)" in {
      forall(allStatus) { status =>
        val m = machine("machine in " + status.name, status)
        repo.save(m).testRun

        val found = repo.getMachine(m.id).testRunGet

        (m === found) and {
          repo.delete(m.id).testRun
          val x = repo.getMachine(m.id).testRun
          x must beEqualTo(Right(None))
          ok
        }
      }
    }

    "correctly find the machine of top priority when on several branches" in {
      allStatus.foreach(status => repo.save(machine("m1", status)).testRun)

      val toFound = machine("m1", AcceptedInventory)
      val found   = repo.getMachine(toFound.id).testRunGet

      toFound === found

    }

    "correctly moved the machine from pending to accepted, then to removed" in {
      val m = machine("movingMachine", PendingInventory)

      repo.save(m).testRun

      (
        repo.move(m.id, AcceptedInventory).isOK
        and (repo.getMachine(m.id).testRunGet must beEqualTo(m.copy(status = AcceptedInventory)))
        and repo.move(m.id, RemovedInventory).isOK
        and (repo.getMachine(m.id).testRunGet must beEqualTo(m.copy(status = RemovedInventory)))
      )
    }

    ", when asked to move machine in removed inventory and a machine with the same id exists there, keep the one in removed and delete the one in accepted" in {
      val m1 = machine("keepingMachine", AcceptedInventory)
      val m2 = m1.copy(status = RemovedInventory, name = Some("modified"))

      (
        (repo.save(m1).isOK)
        and (repo.save(m2).isOK)
        and (repo.move(m1.id, RemovedInventory).isOK)
      )
    }
  }

  "Saving, finding and moving node" should {

    "find node for machine, whatever the presence or status of the machine" in {

      val mid = MachineUuid("foo")

      val n1 = node("acceptedNode", AcceptedInventory, (mid, AcceptedInventory))
      val n2 = node("pendingNode", PendingInventory, (mid, AcceptedInventory))
      val n3 = node("removedNode", RemovedInventory, (mid, AcceptedInventory))

      (
        repo.save(FullInventory(n1, None)).isOK
        and repo.save(FullInventory(n2, None)).isOK
        and repo.save(FullInventory(n3, None)).isOK
      )
    }

    "find back the machine after a move" in {
      val m = machine("findBackMachine", PendingInventory)
      val n = node("findBackNode", PendingInventory, (m.id, m.status))

      (
        repo.save(full(n, m)).isOK
        and repo.move(n.main.id, PendingInventory, AcceptedInventory).isOK
        and {
          val FullInventory(node, machine) = repo.get(n.main.id, AcceptedInventory).testRunGet

          (
            machine === Some(m.copy(status = AcceptedInventory)) and
            node === n
              .copyWithMain(main => main.copy(status = AcceptedInventory))
              .copy(machineId = Some((m.id, AcceptedInventory)))
          )
        }
      )
    }

    "accept to have a machine in a different status than the node" in {
      val m = machine("differentMachine", AcceptedInventory)
      val n = node("differentNode", PendingInventory, (m.id, AcceptedInventory))
      (
        repo.save(full(n, m)).isOK
        and {
          val FullInventory(node, machine) = repo.get(n.main.id, PendingInventory).testRunGet

          (
            node === n
            and machine === Some(m)
          )
        }
      )
    }

    "not find a machine if the container information has a bad status" in {
      val m = machine("invisibleMachine", PendingInventory)
      val n = node("invisibleNode", PendingInventory, (m.id, AcceptedInventory))
      (
        repo.save(full(n, m)).isOK
        and {
          val FullInventory(node, machine) = repo.get(n.main.id, PendingInventory).testRunGet

          (
            node === n
            and machine === None
          )
        }
      )
    }

    ", when moving from pending to accepted, moved back a machine from removed to accepted and correct other node container" in {
      val m  = machine("harcoreMachine", RemovedInventory)
      val n0 = node("h-n0", PendingInventory, (m.id, PendingInventory))
      val n1 = node("h-n1", PendingInventory, (m.id, PendingInventory))
      val n2 = node("h-n2", AcceptedInventory, (m.id, AcceptedInventory))
      val n3 = node("h-n3", RemovedInventory, (m.id, RemovedInventory))

      (
        repo.save(m).isOK and repo.save(FullInventory(n0, None)).isOK and repo.save(FullInventory(n1, None)).isOK and
        repo.save(FullInventory(n2, None)).isOK and repo.save(FullInventory(n3, None)).isOK
        and repo.move(n0.main.id, PendingInventory, AcceptedInventory).isOK
        and {
          val FullInventory(node0, m0) = repo.get(n0.main.id, AcceptedInventory).testRunGet
          val FullInventory(node1, m1) = repo.get(n1.main.id, PendingInventory).testRunGet
          val FullInventory(node2, m2) = repo.get(n2.main.id, AcceptedInventory).testRunGet
          val FullInventory(node3, m3) = repo.get(n3.main.id, RemovedInventory).testRunGet

          // expected machine value
          val machine = m.copy(status = AcceptedInventory)
          val ms      = Some((machine.id, machine.status))

          (
            m0 === Some(machine) and m1 === Some(machine) and m2 === Some(machine) and m3 === Some(machine) and
            node0 === n0
              .copyWithMain(main => main.copy(status = AcceptedInventory))
              .copy(machineId = Some((m.id, AcceptedInventory)))
            and node1 === n1.copy(machineId = ms)
            and node2 === n2.copy(machineId = ms)
            and node3 === n3.copy(machineId = ms)
          )
        }
      )
    }

  }

  "Trying to add specific Windows" should {

    "Allow to save and read it back" in {
      val nodeId = NodeId("windows-2012")

      val node = NodeInventory(
        NodeSummary(
          nodeId,
          AcceptedInventory,
          "administrator",
          "localhost",
          Windows(
            Windows2012,
            "foo",
            new Version("1.0"),
            None,
            new Version("1.0")
          ),
          NodeId("root"),
          UndefinedKey
        ),
        machineId = None
      )

      repo.save(FullInventory(node, None)).isOK and {
        val FullInventory(n, m) = repo.get(nodeId, AcceptedInventory).testRunGet
        n === node
      }
    }

  }

  "Software updates" should {

    "be correctly saved for a node" in {
      val dn  = "nodeId=node0,ou=Nodes,ou=Accepted Inventories,ou=Inventories,cn=rudder-configuration"
      val inv = repo.get(NodeId("node0")).testRunGet
      val su1 = inv.node.softwareUpdates
      val d0  = "2022-01-01T00:00:00Z"
      val dt0 = JsonSerializers.parseSoftwareUpdateDateTime(d0).toOption
      val id0 = "RHSA-2020-4566"
      val id1 = "CVE-2021-4034"

      val updates = List(
        SoftwareUpdate(
          "app1",
          Some("2.15.6~RC1"),
          Some("x86_64"),
          Some("yum"),
          SoftwareUpdateKind.Defect,
          None,
          Some("Some explanation"),
          Some(SoftwareUpdateSeverity.Critical),
          dt0,
          Some(List(id0, id1))
        ),
        SoftwareUpdate(
          "app2",
          Some("1-23-RELEASE-1"),
          Some("x86_64"),
          Some("apt"),
          SoftwareUpdateKind.None,
          Some("default-repo"),
          None,
          None,
          None,
          None
        ), // we can have several time the same app

        SoftwareUpdate(
          "app2",
          Some("1-24-RELEASE-64"),
          Some("x86_64"),
          Some("apt"),
          SoftwareUpdateKind.Security,
          Some("security-backports"),
          None,
          Some(SoftwareUpdateSeverity.Other("backport")),
          None,
          Some(List(id1))
        )
      )

      val ldapValues = List(
        s"""{"name":"app1","version":"2.15.6~RC1","arch":"x86_64","from":"yum","kind":"defect","description":"Some explanation","severity":"critical","date":"${d0}","ids":["${id0}","${id1}"]}""",
        s"""{"name":"app2","version":"1-23-RELEASE-1","arch":"x86_64","from":"apt","kind":"none","source":"default-repo"}""",
        s"""{"name":"app2","version":"1-24-RELEASE-64","arch":"x86_64","from":"apt","kind":"security","source":"security-backports","severity":"backport","ids":["${id1}"]}"""
      )

      (su1 === Nil) and (dt0.isEmpty must beFalse) and
      repo.save(inv.modify(_.node.softwareUpdates).setTo(updates)).isOK
     // (ldap.server.getEntry(dn).getAttribute("softwareUpdate").getValues.toList must containTheSameElementsAs(ldapValues))
    }
  }

  step {
    success
  }

}
