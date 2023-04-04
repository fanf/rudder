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
import com.normation.ldap.listener.InMemoryDsConnectionProvider
import com.normation.ldap.sdk.RoLDAPConnection
import com.normation.ldap.sdk.RwLDAPConnection
import com.normation.rudder.batch.GitGC
import com.normation.rudder.git.GitRepositoryProviderImpl

import com.normation.zio.ZioRuntime
import org.junit.runner._
import org.specs2.mutable._
import org.specs2.runner._
import org.specs2.specification.BeforeAfterAll
import better.files._
import cron4s.Cron
import org.apache.commons.io.FileUtils
import com.normation.inventory.ldap.provisioning._
import com.normation.inventory.provisioning.fusion.FusionInventoryParser
import com.normation.inventory.provisioning.fusion.PreInventoryParserCheckConsistency
import com.normation.inventory.services.provisioning.DefaultInventoryParser
import com.normation.inventory.services.provisioning.InventoryDigestServiceV1
import com.normation.inventory.services.provisioning.InventoryParser
import com.normation.utils.StringUuidGeneratorImpl

import zio._
import zio.syntax._
import zio.concurrent.ReentrantLock
import com.normation.zio._

/**
 *
 * Test the processing of new inventory:
 * - check that new, never seen nodes end into pending
 * - check that new, already pending nodes update pending
 * - check that accepted nodes are updated
 * - check that signature things work.
 *
 * That test does not check for the file observer, only save logic.
 */
@silent("a type was inferred to be `\\w+`; this may indicate a programming error.")
@RunWith(classOf[JUnitRunner])
class TestSaveInventory extends Specification with BeforeAfterAll {

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

  val basePath = "/tmp/test-rudder-inventory/"

  val INVENTORY_ROOT_DIR = basePath + "inventories"
  val INVENTORY_DIR_INCOMING = INVENTORY_ROOT_DIR + "/incoming"
  val INVENTORY_DIR_FAILED = INVENTORY_ROOT_DIR + "/failed"
  val INVENTORY_DIR_RECEIVED = INVENTORY_ROOT_DIR + "/received"
  val INVENTORY_DIR_UPDATE = INVENTORY_ROOT_DIR + "/accepted-nodes-updates"


  override def beforeAll(): Unit = {
    File(basePath).createDirectoryIfNotExists()
  }

  override def afterAll(): Unit = {
    if (java.lang.System.getProperty("tests.clean.tmp") != "false") {
      FileUtils.deleteDirectory(File(basePath).toJava)
    }
  }


  val cronSchedule = Cron.parse("0 42 3 * * ?").toOption
  val gitFactRepoProvider = GitRepositoryProviderImpl
    .make(basePath+"fact-repo")
    .runOrDie(err => new RuntimeException(s"Error when initializing git configuration repository: " + err.fullMsg))
  val gitFactRepoGC = new GitGC(gitFactRepoProvider, cronSchedule)
  gitFactRepoGC.start()

  val gitFactRepo = new GitNodeFactRepositoryImpl(gitFactRepoProvider, "rudder")
  gitFactRepo.checkInit().runOrDie(err => new RuntimeException(s"Error when checking fact repository init: " + err.fullMsg))

  // TODO WARNING POC: this can't work on a machine with lots of node
  val factRepo = {
    for {
      pending <- Ref.make(Map[NodeId, NodeFact]())
      accepted <- Ref.make(Map[NodeId, NodeFact]())
      lock <- ReentrantLock.make()
    } yield {
      new InMemoryNodeFactRepository(pending, accepted, gitFactRepo, lock)
    }
  }.runNow

  //  lazy val ldifInventoryLogger = new DefaultLDIFInventoryLogger(LDIF_TRACELOG_ROOT_DIR)
  lazy val inventorySaver = new NodeFactInventorySaver(
    factRepo,
    (
      CheckOsType
        :: new LastInventoryDate()
        :: AddIpValues
        :: Nil
      ),
    (
//      new PostCommitInventoryHooks[Unit](HOOKS_D, HOOKS_IGNORE_SUFFIXES)
         Nil
      )
  )
  lazy val pipelinedInventoryParser: InventoryParser = {
    val fusionReportParser = {
      new FusionInventoryParser(
        new StringUuidGeneratorImpl(),
        rootParsingExtensions = Nil,
        contentParsingExtensions = Nil,
        ignoreProcesses = false
      )
    }

    new DefaultInventoryParser(
      fusionReportParser,
      Seq(
        new PreInventoryParserCheckConsistency
      )
    )
  }

  lazy val inventoryProcessorInternal = {
    new InventoryProcessor(
      pipelinedInventoryParser,
      inventorySaver,
      4,
      new InventoryDigestServiceV1(fullInventoryRepository),
      () => true.succeed
    )
  }



  lazy val inventoryProcessor = {
    val mover = new InventoryMover(
      INVENTORY_DIR_RECEIVED,
      INVENTORY_DIR_FAILED,
      new InventoryFailedHook("/none","")
    )
    new DefaultProcessInventoryService(inventoryProcessorInternal, mover)
  }

  lazy val inventoryWatcher = {
    val fileProcessor = new ProcessFile(inventoryProcessor, INVENTORY_DIR_INCOMING)

    new InventoryFileWatcher(
      fileProcessor,
      INVENTORY_DIR_INCOMING,
      INVENTORY_DIR_UPDATE,
      3.days,
      1.minute
    )
  }

  lazy val cleanOldInventoryBatch = {
    new PurgeOldInventoryFiles(
      RUDDER_INVENTORIES_CLEAN_CRON,
      RUDDER_INVENTORIES_CLEAN_AGE,
      List(better.files.File(INVENTORY_DIR_FAILED), better.files.File(INVENTORY_DIR_RECEIVED))
    )
  }
  cleanOldInventoryBatch.start()


  sequential







  "Saving a new, unknown inventory" should {

    "correctly save the node in pending" in {

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
        and {
          val dn = inventoryDitService.getDit(AcceptedInventory).MACHINES.MACHINE.dn(m1.id)
          repo.getMachine(m1.id).testRunGet === m2 and ldap.server.entryExists(dn.toString) === false
        }
      )
    }
  }

  "Saving, finding and moving node" should {

    "find node for machine, whatever the presence or status of the machine" in {

      val mid = MachineUuid("foo")

      val n1 = node("acceptedNode", AcceptedInventory, (mid, AcceptedInventory))
      val n2 = node("pendingNode", PendingInventory, (mid, AcceptedInventory))
      val n3 = node("removedNode", RemovedInventory, (mid, AcceptedInventory))

      def toDN(n: NodeInventory) = inventoryDitService.getDit(n.main.status).NODES.NODE.dn(n.main.id.value)

      (
        repo.save(FullInventory(n1, None)).isOK
        and repo.save(FullInventory(n2, None)).isOK
        and repo.save(FullInventory(n3, None)).isOK
        and {
          val res = (for {
            con   <- ldap
            nodes <- repo.getNodesForMachine(con, mid)
          } yield {
            nodes.map { case (k, v) => (k, v.map(_.dn)) }
          })
          res.testRun.forceGet must havePairs(
            AcceptedInventory -> Set(toDN(n1)),
            PendingInventory  -> Set(toDN(n2)),
            RemovedInventory  -> Set(toDN(n3))
          )
        }
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

  "Softwares" should {
    "Find 2 software referenced by nodes with the repository" in {
      val softwares = readOnlySoftware.getSoftwaresForAllNodes().testRun
      softwares.map(_.size) must beEqualTo(Right(2))
    }

    "Find 3 software in ou=software with the repository" in {
      val softwares = readOnlySoftware.getAllSoftwareIds().testRun
      softwares.map(_.size) must beEqualTo(Right(3))
    }

    "Purge one unreferenced software with the SoftwareService" in {
      val purgedSoftwares = softwareService.deleteUnreferencedSoftware().testRun
      purgedSoftwares must beEqualTo(Right(1))
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
      repo.save(inv.modify(_.node.softwareUpdates).setTo(updates)).isOK and
      (ldap.server.getEntry(dn).getAttribute("softwareUpdate").getValues.toList must containTheSameElementsAs(ldapValues))
    }
  }

  step {
    ldap.close
    success
  }


}
