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

package com.normation.rudder.services.queries

import better.files.File
import better.files.Resource
import com.normation.errors.*
import com.normation.inventory.domain.AlmaLinux
import com.normation.inventory.domain.NodeId
import com.normation.inventory.ldap.core.ReadOnlySoftwareDAOImpl
import com.normation.rudder.domain.queries.*
import com.normation.rudder.facts.nodes.*
import com.normation.rudder.facts.nodes.NodeFactSerialisation.*
import com.normation.rudder.repository.ldap.RoLDAPNodeGroupRepository
import com.normation.rudder.repository.ldap.ZioTReentrantLock
import com.normation.rudder.tenants.DefaultTenantService
import com.normation.zio.*
import com.softwaremill.quicklens.*
import java.nio.charset.StandardCharsets
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import zio.*
import zio.json.*
import zio.syntax.*

/*
 * Identify performance problems in NodeFactRepository implementation;
 */

final case class ENV(
    ldap:           MockLdapFactStorageTemplate,
    nodeFactRepo:   CoreNodeFactRepository,
    queryProcessor: QueryProcessor & QueryChecker,
    queryParser:    CmdbQueryParser & DefaultStringQueryParser & JsonQueryLexer
)

object LoadTestQueryProcessor {
  val df      = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SZ")
  val logPath = s"/tmp/test-rudder-load/${ZonedDateTime.now().format(df)}"
  val logFile = File(logPath).createFileIfNotExists(createParents = true)

  def log(s: String) = {
    println(s)
    logFile.appendLine(s)(StandardCharsets.UTF_8)
  }

  def logPure(s: String): UIO[Unit] = effectUioUnit(log(s))

  def init: ENV = {
    log(s"Initializing load test...")

    /*
     * We want to set things as similar as what we have in the real rudder.
     * There will be a major difference in any case with the fact that the LDAP
     * server is in memory, now an openldap one.
     */

    val groupLibReadWriteMutex: ZioTReentrantLock = new ZioTReentrantLock("group-lock")
    object LDAP extends MockLdapFactStorageTemplate("ldap-data/inventory-sample-data.ldif" :: Nil)

    val roNodeGroupRepository: RoLDAPNodeGroupRepository = new RoLDAPNodeGroupRepository(
      LDAP.rudderDit,
      LDAP.ldapRo,
      LDAP.ldapMapper,
      groupLibReadWriteMutex
    )

    val subGroupComparatorRepo:     DefaultSubGroupComparatorRepository = new DefaultSubGroupComparatorRepository(
      roNodeGroupRepository
    )
    val queryData:                  NodeQueryCriteriaData               = new NodeQueryCriteriaData(() => subGroupComparatorRepo)
    val ditQueryData:               DitQueryData                        = new DitQueryData(LDAP.acceptedDIT, LDAP.nodeDit, LDAP.rudderDit, queryData)
    val internalLDAPQueryProcessor: InternalLDAPQueryProcessor          =
      new InternalLDAPQueryProcessor(LDAP.ldapRo, LDAP.acceptedDIT, LDAP.nodeDit, ditQueryData, LDAP.ldapMapper)

    val getNodeBySoftwareName = new SoftDaoGetNodesbySofwareName(
      new ReadOnlySoftwareDAOImpl(
        LDAP.inventoryDitService,
        LDAP.ldapRo,
        LDAP.inventoryMapper
      )
    )
    log(s"LDAP things init...")

    val nodeRepository: CoreNodeFactRepository = {
      val buildRepo = for {
        t <- DefaultTenantService.make(Nil)
        r <- CoreNodeFactRepository.make(LDAP.nodeFactStorage, getNodeBySoftwareName, t, Chunk.empty)
      } yield r

      buildRepo.runNow
    }

    log(s"Node fact repo init...")

    val queryProcessor = new NodeFactQueryProcessor(nodeRepository, subGroupComparatorRepo, internalLDAPQueryProcessor)

    val queryParser: CmdbQueryParser & DefaultStringQueryParser & JsonQueryLexer = new CmdbQueryParser
      with DefaultStringQueryParser with JsonQueryLexer {
      override val criterionObjects: Map[String, ObjectCriterion] = Map[String, ObjectCriterion]() ++ ditQueryData.criteriaMap
    }

    log(s"all init")
    ENV(LDAP, nodeRepository, queryProcessor, queryParser)
  }

  def time[A](hint: String)(a: IOResult[A]): IOResult[A] = {
    for {
      t0  <- currentTimeMillis
      res <- a
      t1  <- currentTimeMillis
      _   <- logPure(s"${hint}: ${t1 - t0} ms")
    } yield res
  }

  def createNodes(small: Boolean): IOResult[Chunk[NodeFact]] = {
    val numByOS = 50
    logPure(s"Create ${numByOS} nodes from ${if (small) "small" else "full"} template") *>
    time("time creating nodes: ")(for {
      alma   <- LoadNodeFact.generateNodesFrom(if (small) "alma9_1_small.json" else "alma9_1.json", 10000000, numByOS)
      debian <- LoadNodeFact.generateNodesFrom(if (small) "debian12_1_small.json" else "debian12_1.json", 20000000, numByOS)
      ubuntu <- LoadNodeFact.generateNodesFrom(if (small) "ubuntu22_1_small.json" else "ubuntu22_1.json", 30000000, numByOS)
    } yield alma ++ debian ++ ubuntu)
  }

  def saveNodes(nodes: Chunk[NodeFact])(implicit env: ENV): IOResult[Unit] = {
    implicit val cc: ChangeContext = ChangeContext.newForRudder()
    time(s"saving ${nodes.size} node")(
      ZIO.foreachParDiscard(nodes)(n => env.nodeFactRepo.unsafeSave(n, checkCertificate = false)).withParallelism(4)
    )
  }

  /*
   * Comparison between selecting OS from query or LDAP, repeat N times
   */
  def selectOS(env: ENV, nodes: Chunk[NodeFact], repeat: Int): IOResult[Unit] = {
    val query = """
      { "select":"node", "where":[
        {"objectType":"node","attribute":"osName","comparator":"eq","value":"AlmaLinux"}
      ] }
      """

    for {
      _ <- logPure(s"selecting OS with ${repeat} repetitions")
      q <- env.queryParser(query).toIO
      _ <- time(s"process query")(env.queryProcessor.processPure(q)).repeatN(repeat)
      _ <- time(s"collect nodes")(nodes.collect { case n if (n.os.os == AlmaLinux) => n }.succeed).repeatN(repeat)
    } yield ()
  }

  def prog(implicit env: ENV): IOResult[Unit] = {
    for {
      nodes <- createNodes(small = true)
      _     <- saveNodes(nodes)
// if a full dump of ldif is needed
//      _     <- IOResult.attempt(env.ldap.testServer.exportToLDIF("/tmp/rudder-load.ldif", true, true))
      _     <- selectOS(env, nodes, 50)
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    implicit val env: ENV = init

    (for {
      _ <- logPure(s"Starting test...")
      _ <- prog
      _ <- logPure(s"... done.")
    } yield ()).runNow

    java.lang.System.exit(0)
  }
}

object LoadNodeFact {

  val base = "load-test/"

  def id(i: Int): NodeId = NodeId(i.toString + "-0000-0000-0000-000000000000")

  /*
   * Load a JSON file from src/test/resources/load-test by name
   */
  def unser(name: String): IOResult[NodeFact] = {
    val path = base + name
    for {
      json <- IOResult.attempt(s"error getting: ${path}")(Resource.getAsString(path))
      node <- json.fromJson[NodeFact].toIO
    } yield node
  }

  /*
   * Generate N node facts from the template, varying only the ID.
   * UUUIds will looks like: 00000000-0000-0000-0000-000000000000
   * We only increment the first 8  digits, so you need to give a number with 8 digits
   * and manage by your-self the upper bound.
   * Ex: start: 1000000000
   * number: 1000
   * will lead to nodes with ID:
   *   10000000-0000-0000-0000-000000000000
   *   10000001-0000-0000-0000-000000000000
   *   10000002-0000-0000-0000-000000000000
   *   ....
   *   10000999-0000-0000-0000-000000000000
   */
  def generateNodes(base: NodeFact, start: Int, number: Int): Chunk[NodeFact] = {
    val size = number.toString.length
    Chunk.fromIterable(
      (0 until number).map(i =>
        base.modify(_.id).setTo(id(start + i)).modify(_.fqdn).using(x => x.replaceAll("XX", String.format("%0" + size + "d", i)))
      )
    )
  }

  def generateNodesFrom(name: String, start: Int, number: Int): IOResult[Chunk[NodeFact]] = {
    unser(name).map(n => generateNodes(n, start, number))
  }
}
