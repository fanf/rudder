/*
 *************************************************************************************
 * Copyright 2021 Normation SAS
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

import NodeFactSerialisation._
import better.files.File
import com.normation.errors._
import com.normation.errors.IOResult
import com.normation.inventory.domain._
import com.normation.rudder.domain.logger.NodeLogger
import com.normation.rudder.git.GitItemRepository
import com.normation.rudder.git.GitRepositoryProvider
import com.softwaremill.quicklens._
import java.nio.charset.StandardCharsets
import org.eclipse.jgit.lib.PersonIdent
import zio._
import zio.json._
import zio.stream.ZStream
import zio.syntax._

/*
 * This file contains the base to persist facts into a git repository. There is a lot of question
 * remaining, so don't take current traits/classes as an API, it *WILL* change. The basic questions to answer are:
 * - do we want one bit "FactRepo" that knows about all kind of facts and is able to persis any of them ? In that case,
 *   we will need some kind of parametrization of `persist` with a type class to teach that repo how to serialize and
 *   persist each case
 * - do we prefer lots of small repos, one by entity, which knows how to persist only that entity ?
 * - plus, we want to have some latitude on the serialization part, and be able to use both liftjson and zio-json
 *   (because the complete migration toward zio-json won't be finish immediately)
 *
 * The "one big" repo feels more like it is what we need, since it's really just one big git repo with sub-cases,
 * with shared tools and specialisation by entity. But I'm not sure how to build the capacities with type class
 * until I have several examples.
 * The small repos (one by entity) is what we used to do, so we are in known territory (see archive of configuration
 * entities), even if it is not a very satisfying one. Its advantage is that it's very simple, but it leads to a lot
 * of code duplication and maintenance is complicated (and adding a new entity is basically "copy that 100 lines of
 * code, and sed things", while we would like it to be "implement just that interface")
 *
 * Finally, we some coupling between serialization and repos as they are written for now: the path can't be known
 * without some part of the entity, but we don't know which part exactly (for node, it's its uuid and status, but
 * perhaps it's an exception, and for all other it's just an ID).
 *
 * With all these unknowns, I prefer to let parametrisation as simple as possible:
 * - no abstraction for repo, we just have a "node repo" with all the concrete types. It's likely to become a:
 *   ```
 *     trait FactRepo { def persist[E](e: E)(implicit Serialize[E]): IOResult[Unit])
 *   ```
 *   Or something alike, but we just don't know.
 *
 * - some abstraction for serialisation, but just to put in plain sight the fact that there a characteristic of
 *   the entity that is not the whole entity, and more then its ID, that is needed to build where the entity
 *   will be saved.
 *
 * - a simple implementation for nodes, that will need to be refactored depending of the chosen final arch.
 *
 * And finally, to complexity a bit more the picture, we see that there is events (observations?) linked to facts
 * that can update the previous fact partially. For nodes, it's "change the status" (which is, perhaps by luck,
 * the same subpart of the entity than the one used in the more-than-just-an-id parameter of serialization).
 * I don't know for now if it's a general truth, or if it's just an happenstance, and if there is a general
 * capability (like "partialUpdate[SomeSubParOfE => E]") to define (in a pure events-tore, we would save that
 * event as if, but well we want to have readable text files for users in our git repos)
 */

/*
 * Serialize a fact type (to/from JSON), for example nodes.
 * The format is versioned so that we are able to unserialize old files into newer domain representation.
 *
 * We store a fileFormat and the serialized object type.
 * To let more space for evolution, file format will be a string even if it should be parsed as an int.
 *
 * There's two parameter, one minimal (A) that allows to identify where the fact should be store (typically, it's a
 * kind of ID), and (B) which the whole fact to serialize. There should exists a constraint of derivability from B to A,
 * but it's not modeled.
 */
trait SerializeFacts[A, B] {

  def fileFormat: String
  def entity:     String

  def toJson(data: B): IOResult[String]

  // this is just a relative path from a virtual root, for example for node it will be: "accepted/node-uuid.json"
  def getEntityPath(id: A): String

}

trait NodeFactStorage {

  /*
   * Save node fact in the status given in the corresponding attribute.
   * No check will be done.
   */
  def save(nodeFact: NodeFact)(implicit attrs: SelectFacts = SelectFacts.all): IOResult[Unit]

  /*
   * Change the status of the node with given id to given status.
   * - if the node is not found, an error is raised apart if target status is "delete"
   * - if the target status is the current one, this function does nothing
   * - if target status is "removed", persisted inventory is deleted
   */
  def changeStatus(nodeId: NodeId, status: InventoryStatus): IOResult[Unit]

  /*
   * Delete the node. Storage need to loop for any status and delete
   * any reference to that node.
   */
  def delete(nodeId: NodeId): IOResult[Unit]

  def getPending(nodeId:  NodeId)(implicit attrs: SelectFacts = SelectFacts.default): IOResult[Option[NodeFact]]
  def getAccepted(nodeId: NodeId)(implicit attrs: SelectFacts = SelectFacts.default): IOResult[Option[NodeFact]]

  def getAllPending()(implicit attrs:  SelectFacts = SelectFacts.default): IOStream[NodeFact]
  def getAllAccepted()(implicit attrs: SelectFacts = SelectFacts.default): IOStream[NodeFact]
}

/*
 * Implementation that store nothing and that can be used in tests or when a pure
 * in-memory version of the nodeFactRepos is needed.
 */
object NoopFactStorage extends NodeFactStorage {
  override def save(nodeFact: NodeFact)(implicit attrs: SelectFacts = SelectFacts.default): IOResult[Unit]             = ZIO.unit
  override def changeStatus(nodeId: NodeId, status: InventoryStatus):                       IOResult[Unit]             = ZIO.unit
  override def delete(nodeId: NodeId):                                                      IOResult[Unit]             = ZIO.unit
  override def getAllPending()(implicit attrs: SelectFacts = SelectFacts.default):          IOStream[NodeFact]         = ZStream.empty
  override def getAllAccepted()(implicit attrs: SelectFacts = SelectFacts.default):         IOStream[NodeFact]         = ZStream.empty
  override def getPending(nodeId: NodeId)(implicit attrs: SelectFacts):                     IOResult[Option[NodeFact]] = None.succeed
  override def getAccepted(nodeId: NodeId)(implicit attrs: SelectFacts):                    IOResult[Option[NodeFact]] = None.succeed
}

/*
 * We have only one git for all fact repositories. This is the one managing semaphore, init, etc.
 * All fact repositories will be a subfolder on it:
 * - /var/rudder/fact-repository/nodes
 * - /var/rudder/fact-repository/rudder-config
 * - /var/rudder/fact-repository/groups
 * etc
 */

object GitNodeFactStorageImpl {

  final case class NodeFactArchive(
      entity:     String,
      fileFormat: String,
      node:       NodeFact
  )

  implicit val codecNodeFactArchive: JsonCodec[NodeFactArchive] = DeriveJsonCodec.gen
}

/*
 * Nodes are stored in the git facts repo under the relative path "nodes".
 * They are then stored:
 * - under nodes/pending or nodes/accepted given their status (which means that changing status of a node is
 *   a special operation)
 */
class GitNodeFactStorageImpl(
    override val gitRepo: GitRepositoryProvider,
    groupOwner:           String,
    actuallyCommit:       Boolean
) extends NodeFactStorage with GitItemRepository with SerializeFacts[(NodeId, InventoryStatus), NodeFact] {

  override val relativePath = "nodes"
  override val entity:     String = "node"
  override val fileFormat: String = "10"
  val committer = new PersonIdent("rudder-fact", "email not set")

  if (actuallyCommit) {
    NodeLogger.info(s"Nodes changes will be historized in Git in ${gitRepo.rootDirectory.pathAsString}/nodes")
  } else {
    NodeLogger.info(
      s"Nodes changes won't be historized in Git, only last state is stored in ${gitRepo.rootDirectory.pathAsString}/nodes"
    )
  }

  if (actuallyCommit) {
    NodeLogger.info(s"Nodes changes will be historized in Git in ${gitRepo.rootDirectory.pathAsString}/nodes")
  } else {
    NodeLogger.info(
      s"Nodes changes won't be historized in Git, only last state is stored in ${gitRepo.rootDirectory.pathAsString}/nodes"
    )
  }

  override def getEntityPath(id: (NodeId, InventoryStatus)): String = {
    s"${id._2.name}/${id._1.value}.json"
  }

  def getFile(id: NodeId, status: InventoryStatus): File = {
    gitRepo.rootDirectory / relativePath / getEntityPath((id, status))
  }

  /*
   * serialize the inventory into a normalized JSON string.
   * As we want it to be human readable and searchable, we will use an indented format.
   */
  def toJson(nodeFact: NodeFact): IOResult[String] = {
    import GitNodeFactStorageImpl._
    val node = nodeFact
      .modify(_.accounts)
      .using(_.sorted)
      .modify(_.properties)
      .using(_.sortBy(_.name))
      .modify(_.environmentVariables)
      .using(_.sortBy(_._1))
      .modify(_.fileSystems)
      .using(_.sortBy(_.name))
      .modify(_.networks)
      .using(_.sortBy(_.name))
      .modify(_.processes)
      .using(_.sortBy(_.commandName))
      .modify(_.bios)
      .using(_.sortBy(_.name))
      .modify(_.controllers)
      .using(_.sortBy(_.name))
      .modify(_.memories)
      .using(_.sortBy(_.name))
      .modify(_.ports)
      .using(_.sortBy(_.name))
      .modify(_.processors)
      .using(_.sortBy(_.name))
      .modify(_.slots)
      .using(_.sortBy(_.name))
      .modify(_.sounds)
      .using(_.sortBy(_.name))
      .modify(_.storages)
      .using(_.sortBy(_.name))
      .modify(_.videos)
      .using(_.sortBy(_.name))

    NodeFactArchive(entity, fileFormat, node).toJsonPretty.succeed
  }

  // we don't want to wite the codec to "not unserialize" given SelectFacts right now, so we are
  // just masking
  private[nodes] def fileToNode(f: File)(implicit attrs: SelectFacts): IOResult[NodeFact] = {
    f
      .contentAsString(StandardCharsets.UTF_8)
      .fromJson[NodeFact]
      .toIO
      .chainError(s"Error when decoding ${f.pathAsString}")
      .map(_.maskWith(attrs))
  }

  private[nodes] def get(nodeId: NodeId, status: InventoryStatus)(implicit attrs: SelectFacts): IOResult[Option[NodeFact]] = {
    val f = getFile(nodeId, status)
    if (f.exists) fileToNode(f).map(Some(_))
    else None.succeed
  }

  private[nodes] def getAll(base: File)(implicit attrs: SelectFacts): IOStream[NodeFact] = {
    // TODO should be from git head, not from file directory
    val stream = ZStream.fromIterator(base.collectChildren(_.extension(includeDot = true, includeAll = true) == Some(".json")))
    stream
      .mapError(ex => SystemError("Error when reading node fact persisted file", ex))
      .mapZIO(f => fileToNode(f))
  }

  override def getPending(nodeId: NodeId)(implicit attrs: SelectFacts): IOResult[Option[NodeFact]] = {
    get(nodeId, PendingInventory)
  }

  override def getAccepted(nodeId: NodeId)(implicit attrs: SelectFacts): IOResult[Option[NodeFact]] = {
    get(nodeId, AcceptedInventory)
  }

  override def getAllPending()(implicit attrs: SelectFacts):  IOStream[NodeFact] = getAll(
    gitRepo.rootDirectory / relativePath / PendingInventory.name
  )
  override def getAllAccepted()(implicit attrs: SelectFacts): IOStream[NodeFact] = getAll(
    gitRepo.rootDirectory / relativePath / AcceptedInventory.name
  )

  // We saving, we must ignore attrs that are "ignored" - ie in that case, if the source list is empty, we take the existing one
  // Save does not know about status change. If it's called with a status change, this leads to duplicated data.
  override def save(nodeFact: NodeFact)(implicit attrs: SelectFacts): IOResult[Unit] = {
    if (nodeFact.rudderSettings.status == RemovedInventory) {
      InventoryDataLogger.info(
        s"Not persisting deleted node '${nodeFact.fqdn}' [${nodeFact.id.value}]: it has removed inventory status"
      ) *>
      ZIO.unit
    } else {
      val file = getFile(nodeFact.id, nodeFact.rudderSettings.status)
      for {
        old   <- fileToNode(file).map(Some(_)).catchAll(_ => None.succeed)
        merged = SelectFacts.merge(nodeFact, old)
        json  <- toJson(merged)
        _     <- IOResult.attempt(file.write(json))
        _     <- IOResult.attempt(file.setGroup(groupOwner))
        _     <- ZIO.when(actuallyCommit) {
                   commitAddFile(
                     committer,
                     toGitPath(file.toJava),
                     s"Save inventory facts for ${merged.rudderSettings.status.name} node '${merged.fqdn}' (${merged.id.value})"
                   )
                 }
      } yield ()
    }
  }

  // when we delete, we check for all path to also remove possible left-over
  // we may need to recreate pending/accepted directory, because git delete
  // empty directories.
  override def delete(nodeId: NodeId) = {
    ZIO.foreach(List(PendingInventory, AcceptedInventory)) { s =>
      val file = getFile(nodeId, s)
      ZIO.whenZIO(IOResult.attempt(file.exists)) {
        if (actuallyCommit) {
          commitRmFile(committer, toGitPath(file.toJava), s"Updating facts for node '${nodeId.value}': deleted")
        } else {
          IOResult.attempt(file.delete())
        }
      }
    } *> checkInit()
  }

  override def changeStatus(nodeId: NodeId, toStatus: InventoryStatus): IOResult[Unit] = {
    // pending and accepted are symmetric, utility function for the two cases
    def move(to: InventoryStatus) = {
      val from = if (to == AcceptedInventory) PendingInventory else AcceptedInventory

      val fromFile = getFile(nodeId, from)
      val toFile   = getFile(nodeId, to)
      // check if fact already where it should
      ZIO.ifZIO(IOResult.attempt(fromFile.exists))(
        // however toFile exists, move, because if present it may be because a deletion didn't work and
        // we need to overwrite
        IOResult.attempt(fromFile.moveTo(toFile)(File.CopyOptions(overwrite = true))) *>
        ZIO.when(actuallyCommit) {
          commitMvDirectory(
            committer,
            toGitPath(fromFile.toJava),
            toGitPath(toFile.toJava),
            s"Updating facts for node '${nodeId.value}' to status: ${to.name}"
          )
        },
        // if source file does not exist, check if dest is present. If present, assume it's ok, else error
        ZIO.whenZIO(IOResult.attempt(!toFile.exists)) {
          Inconsistency(
            s"Error when trying to move fact for node '${nodeId.value}' from '${fromFile.pathAsString}' to '${toFile.pathAsString}': missing files"
          ).fail
        }
      )
    }

    (toStatus match {
      case RemovedInventory => delete(nodeId)
      case x                => move(x)
    }).unit
  }

  /*
   * check that everything is ok for that repo entities (typically: subfolder created, perm ok, etc)
   */
  def checkInit(): IOResult[Unit] = {
    val dirs = List(AcceptedInventory.name, PendingInventory.name)
    dirs.accumulate { dir =>
      val d = gitRepo.rootDirectory / relativePath / dir
      for {
        _ <- ZIO
               .whenZIO(IOResult.attempt(d.notExists)) {
                 IOResult.attempt {
                   d.createDirectories()
                   d.setGroup(groupOwner)
                 }
               }
               .chainError(s"Error when creating directory '${d.pathAsString}' for historising inventories: ${}")
        _ <- ZIO.whenZIO(IOResult.attempt(!d.isOwnerWritable)) {
               Inconsistency(
                 s"Error, directory '${d.pathAsString}' must be a writable directory to allow inventory historisation"
               ).fail
             }
      } yield ()
    }.unit
  }
}

/*
 * An LDAP implementation for NodeFactStorage.
 * It takes most of its code from old FullLdapInventory/NodeInfoService
 */

class LdapNodeFactStorage(
    nodeDit:             NodeDit,
    acceptedDit:         InventoryDit,
    inventoryDitService: InventoryDitService,
    nodeMapper:          LDAPEntityMapper,
    inventoryMapper:     InventoryMapper,
    ldap:                LDAPConnectionProvider[RwLDAPConnection],
    actionLogger:        EventLogRepository,
    nodeLibMutex:        ScalaReadWriteLock // that's a scala-level mutex to have some kind of consistency with LDAP

) extends NodeFactStorage {
  override def save(nodeFact: NodeFact)(implicit attrs: SelectFacts): IOResult[Unit] = {
    val entry = mapper.nodeToEntry(node)
    nodeLibMutex.writeLock(for {
      con <- ldap
      _   <- con.save(entry).chainError(s"Cannot save node with id '${node.id.value}' in LDAP")
    } yield ())
  }

  override def changeStatus(nodeId: NodeId, status: InventoryStatus): IOResult[Unit] = ???

  override def delete(nodeId: NodeId): IOResult[Unit] = {
    val entry = mapper.nodeToEntry(node)
    nodeLibMutex.writeLock(for {
      con <- ldap
      _   <- con.delete(entry.dn).chainError(s"Error when trying to delete node '${node.id.value}'")
    } yield {
      node
    })

  }

  override def getPending(nodeId: NodeId)(implicit attrs: SelectFacts): IOResult[Option[NodeFact]] = ???

  override def getAccepted(nodeId: NodeId)(implicit attrs: SelectFacts): IOResult[Option[NodeFact]] = ???

  override def getAllPending()(implicit attrs: SelectFacts): IOStream[NodeFact] = ???

  override def getAllAccepted()(implicit attrs: SelectFacts): IOStream[NodeFact] = ???
}
