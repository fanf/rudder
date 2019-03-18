/*
*************************************************************************************
* Copyright 2011 Normation SAS
*************************************************************************************
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
*************************************************************************************
*/

package com.normation.ldap.sdk

import cats.data.NonEmptyList
import com.normation.NamedZioLogger
import com.normation.errors.RudderError
import com.normation.ldap.ldif.LDIFFileLogger
import com.normation.ldap.ldif.LDIFNoopChangeRecord
import com.normation.ldap.sdk.LdapResult._
import com.unboundid.ldap.sdk.ResultCode._
import com.unboundid.ldap.sdk.AddRequest
import com.unboundid.ldap.sdk.DN
import com.unboundid.ldap.sdk.DeleteRequest
import com.unboundid.ldap.sdk.Filter
import com.unboundid.ldap.sdk.LDAPException
import com.unboundid.ldap.sdk.LDAPSearchException
import com.unboundid.ldap.sdk.Modification
import com.unboundid.ldap.sdk.ModifyDNRequest
import com.unboundid.ldap.sdk.ModifyRequest
import com.unboundid.ldap.sdk.RDN
import com.unboundid.ldap.sdk.ReadOnlyLDAPRequest
import com.unboundid.ldap.sdk.ResultCode
import com.unboundid.ldap.sdk.SearchRequest
import com.unboundid.ldif.LDIFChangeRecord
import scalaz.zio.blocking.Blocking
import scalaz.zio._
import scalaz.zio.syntax._
import com.normation.zio._

import scala.collection.JavaConverters._
import org.slf4j.LoggerFactory

/*
 * Logger for LDAP connection related information.
 */
object LDAPConnectionLogger extends NamedZioLogger("ldap-connection")

sealed trait LdapResultRudderError extends RudderError {
  def msg: String
}

object LdapResultRudderError {
  // errors due to some LDAPException
  final case class BackendException(msg: String, cause: Throwable)  extends LdapResultRudderError
  // errors where there is a result, but result is not SUCCESS
  final case class FailureResult(msg: String, result: LDAPResult)   extends LdapResultRudderError

  final case class Consistancy(msg: String)                         extends LdapResultRudderError
  // accumulated errors from multiple independent action
  final case class Accumulated(errors: NonEmptyList[RudderError])   extends LdapResultRudderError {
    def msg = s"Several errors encountered: ${errors.toList.map(_.msg).mkString("; ")}"
  }
}

object LdapResult{
  type LdapResult[T] = IO[LdapResultRudderError, T]

  // transform an Option[T] into an error
  implicit class StrictOption[T](opt: LdapResult[Option[T]]) {
    def notOptional(msg: String) = IO.require[LdapResultRudderError, T](LdapResultRudderError.Consistancy(msg))(opt)
  }

  // same than above for a Rudder error from a string
  implicit class ToFailureMsg(e: String) {
    def fail = IO.fail(LdapResultRudderError.Consistancy(e))
  }
  // for compat
  implicit class ToBox[T](res: LdapResult[T]) {
    import net.liftweb.common._
    def toBox: Box[T] = ZioRuntime.unsafeRun(res.fold(e => Failure(e.msg), s => Full(s)))
  }

  implicit class ValidatedToLdapError[T](res: ZIO[Any, NonEmptyList[LdapResultRudderError], List[T]]) {
    def toLdapResult: LdapResult[List[T]] = res.mapError(errors => LdapResultRudderError.Accumulated(errors))
  }

  implicit class EitherToLdapError[T](res: Either[RudderError, T]) {
    def toLdapResult: LdapResult[T] = {
      res match {
        case Left(error) => LdapResultRudderError.Consistancy(error.msg).fail
        case Right(x)    => x.succeed
      }
    }
  }

}

trait ReadOnlyEntryLDAPConnection {

  /**
   * Most generic search request, which allows to use controls
   * and other advanced operation.
   * @param sr
   *   SearchRequest object which define the search operation
   *   to send to LDAP directory
   * @return
   *   The sequence of entries matching SearchRequest.
   */
  def search(sr:SearchRequest): LdapResult[Seq[LDAPEntry]]

  /**
   * Retrieve entry with given 'dn', optionally restricting
   * entry's attribute set to attribute with name in the
   * 'attributes' parameter.
   * @param dn
   *   DN of the entry to retrieve
   * @param attributes
   *   Only retrieve attributes on that list in the entry.
   *   Let empty to retrieve all attributes.
   * @return
   *   Full(entry) if the entry exists,
   *   Empty if no such entry exists
   *   Failure(message) if something bad happened
   */
  def get(dn:DN, attributes:String*) : LdapResult[Option[LDAPEntry]]

  /**
   * A search with commonly used parameters
   * @param baseDn
   *   The base DN from which the search has to be started
   * @param scope
   *   Scope of the search (base, one level, subtree)
   * @param filter
   *   Filter to use to decide if an entry should be returned
   * @param attributes
   *   If non-empty, for each returned entry, only retrieve
   *   attributes in the list.
   *   Otherwise, retrieve all attributes of all entries.
   * @return
   *   The sequence of entries matching search request parameters.
   */
  def search(baseDn:DN, scope:SearchScope, filter:Filter, attributes:String*) : LdapResult[Seq[LDAPEntry]] = {
    search(new SearchRequest(baseDn.toString, scope, filter, attributes:_*))
  }

  /**
   * Search for one entry which is:
   * - a direct children of a base DN
   * - match the filter
   *
   * Only one entry is returned at max. If the
   * filter match several entries under base DN,
   * one will be pick at random.
   *
   * @param baseDn
   *   Root of the search: we are looking for one of
   *   its children
   * @param filter
   *   Filter to use to choose from children of base DN entry
   * @param attributes
   *   If non empty, only retrieve attribute from that list.
   *   Else, retrieve all attributes.
   * @return
   *   Full(entry) if an entry matching filter is found in
   *     base DN entry's children
   *   Failure(message) if something goes wrong
   *   Empty otherwise
   */
  def get(baseDn:DN, filter:Filter, attributes:String*) : LdapResult[Option[LDAPEntry]] = {
    searchOne(baseDn, filter, attributes:_*).map {
      case buf if(buf.isEmpty) => None
      case buf                 => Some(buf(0))
    }
  }

  /**
   * Test existence of the given entry.
   * Of course, as LDAP is not a transactionnal datasource,
   * result is only valid for the time when directory
   * gave the answer.
   *
   * @param dn
   *   DN of the entry to test for existence
   * @return
   *   True if the entry exists, false otherwise.
   */
  def exists(dn:DN) : LdapResult[Boolean] = get(dn, "1.1").map( _.isDefined )

  /**
   * Search method restricted to scope = One level
   * @see search
   * @param baseDn
   *   The base DN from which the search has to be started
   * @param filter
   *   Filter to use to decide if an entry should be returned
   * @param attributes
   *   If non-empty, for each returned entry, only retrieve
   *   attributes in the list.
   *   Otherwise, retrieve all attributes of all entries.
   * @return
   *   The sequence of entries matching search request parameters.
   */
  def searchOne(baseDn:DN,filter:Filter, attributes:String*) : LdapResult[Seq[LDAPEntry]] = search(baseDn,One,filter,attributes:_*)

  /**
   * Search method restricted to scope = SubTree
   * @see search
   * @param baseDn
   *   The base DN from which the search has to be started
   * @param filter
   *   Filter to use to decide if an entry should be returned
   * @param attributes
   *   If non-empty, for each returned entry, only retrieve
   *   attributes in the list.
   *   Otherwise, retrieve all attributes of all entries.
   * @return
   *   The sequence of entries matching search request parameters.
   */
  def searchSub(baseDn:DN,filter:Filter, attributes:String*) : LdapResult[Seq[LDAPEntry]] = search(baseDn,Sub,filter,attributes:_*)
}

trait WriteOnlyEntryLDAPConnection {

  /**
   * Execute a plain modification.
   * Return the actual modification executed if success,
   * the error in other case.
   */
  def modify(dn:DN, modifications:Modification*) : LdapResult[LDIFChangeRecord]

  /**
   * Move entry with given dn to new parent.
   * @param dn
   *   Entry's DN to move
   * @param newParentDn
   *   New parent's DN
   * @param newRDN
   *   Optionnaly change the RDN of the entry.
   * @return
   *   Full[Seq(ldifChangeRecord)] if the operation is successful
   *   Empty or Failure if an error occurred.
   */
  def move(dn:DN,newParentDn:DN, newRDN:Option[RDN] = None) : LdapResult[LDIFChangeRecord]

  /**
   * Save an LDAP entry.
   * The semantic of a save is complex:
   * - by default, it only update attributes in LDAP entry.
   *   That means that if entry in the directory has (a,b,c) attribute, and entry has only (a,b),
   *   then c won't be remove nor updated in LDAP
   * - attribute with no values are removed
   *   That means that if entry has attribute 'a' with no value, attribute 'a' will be removed in LDAP directory
   * - if "removeMissing" is set to true, then missing attribute in entry are marked to be removd (most of the time,
   *   it's not what you want).
   *   WARNING: the RDN attribute is always ignored. You can only change it with the <code>move</move> method
   * - if "removeMissing" is set to true, you can still keep some attribute enumerated here. If removeMissing is false,
   *   that parameter is ignored.
   */
  def save(entry : LDAPEntry, removeMissingAttributes:Boolean=false, forceKeepMissingAttributes:Seq[String] = Seq()) : LdapResult[LDIFChangeRecord]

  /**
   * Delete entry at the given DN
   * If recurse is set to true (default), delete all entry's children before
   * deleting entry.
   * If recurse is set to false, the entry must have zero child to be
   * allowed to be deleted.
   *
   * If no entry has the given DN, nothing is done.
   */
  def delete(dn:DN, recurse:Boolean = true) : LdapResult[Seq[LDIFChangeRecord]]

}

trait ReadOnlyTreeLDAPConnection {
  /**
   * Retrieve the full sub-tree of entries where the root
   * entry is the one with given 'dn'
   * All entries of the subtree are retrieved, and for
   * each of them, all attributes are retrieved.
   * BE CAREFULL: the result may be HUGE.
   * @param dn
   *   DN of the root entry for the sub-tree to retrieve
   * @return
   *   Full(LDAPTree) if the root entry exists and the command
   *     succeeded
   *   Empty if no entry has the given DN
   *   Failure(message) if something goes wrong.
   */
  def getTree(dn:DN) : LdapResult[Option[LDAPTree]]
}

trait WriteOnlyTreeLDAPConnection {
  /**
   * Save the full LDAPTree given in argument.
   *
   * TODO: specify behaviour.
   *
   * @param tree
   * @param deleteRemoved
   * @return
   */
  def saveTree(tree:LDAPTree, deleteRemoved:Boolean=false) : LdapResult[Seq[LDIFChangeRecord]]
}

/**
 * Trait that specify that the LDAPConnection is
 * backed by an UnboundID LDAPConnection object.
 * This object may be used to access to methods
 * not supported by LDAPConnection Scala API
 */
trait UnboundidBackendLDAPConnection {
  /**
   * Access to the backed UnboundID LDAPConnection object,
   * if one need to do operation not covered by Scala API.
   *
   * @return
   *   LDAPConnection object used in back-end.
   */
  def backed : UnboundidLDAPConnection

  /**
   * Close that LDAPConnection
   */
  def close() : Unit = backed.close()

}

object RoLDAPConnection {
  import ResultCode._
  /**
   * Default error on which we don't want to throw an exception
   * but only log a message for Search operation
   */
  def onlyReportOnSearch(errorCode:ResultCode) : Boolean = {
    errorCode match {
      case TIME_LIMIT_EXCEEDED |
        SIZE_LIMIT_EXCEEDED => true
      case _ => false
    }
  }
}


sealed class RoLDAPConnection(
    override val backed : UnboundidLDAPConnection
  , val ldifFileLogger : LDIFFileLogger
  , val onlyReportOnSearch: ResultCode => Boolean = RoLDAPConnection.onlyReportOnSearch
  , val blockingModule: Blocking
) extends
  UnboundidBackendLDAPConnection with
  ReadOnlyEntryLDAPConnection with
  ReadOnlyTreeLDAPConnection
{

  def blocking[A](effect: => A): Task[A] = blockingModule.blocking.blocking( IO.effect(effect) )

  val logger = LoggerFactory.getLogger(classOf[RoLDAPConnection])

  /*
   * //////////////////////////////////////////////////////////////////
   * // Read
   * //////////////////////////////////////////////////////////////////
   */

  override def search(sr:SearchRequest) : LdapResult[Seq[LDAPEntry]] = {
    blocking {
      backed.search(sr).getSearchEntries.asScala.map(e => LDAPEntry(e.getDN, e.getAttributes.asScala))
    } catchAll {
      case e:LDAPSearchException if(onlyReportOnSearch(e.getResultCode)) =>
        logger.error("Ignored execption (configured to be ignored)", e)
        e.getSearchEntries.asScala.map(e => LDAPEntry(e.getDN, e.getAttributes.asScala)).succeed
      case ex: LDAPException =>
        LdapResultRudderError.BackendException(s"Error during search: ${ex.getDiagnosticMessage}", ex).fail
      // catchAll is a lie, but if other kind of exception happens, we want to crash
      case ex => throw ex
    }
  }

  override def get(dn:DN, attributes:String*) : LdapResult[Option[LDAPEntry]] = {
    blocking {
      val e = if(attributes.size == 0) backed.getEntry(dn.toString)
              else backed.getEntry(dn.toString, attributes:_*)
      e match {
        case null => None
        case r    => Some(LDAPEntry(r.getDN, r.getAttributes.asScala))
      }
    } catchAll {
      case ex: LDAPException =>
        LdapResultRudderError.BackendException(s"Error when getting enty '${dn.toNormalizedString}': ${ex.getDiagnosticMessage}", ex).fail
      // catchAll is a lie, but if other kind of exception happens, we want to crash
      case ex => throw ex
    }
  }

  /*
   * //////////////////////////////////////////////////////////////////
   * // Read Tree
   * //////////////////////////////////////////////////////////////////
   */

  override def getTree(dn:DN) : LdapResult[Option[LDAPTree]] = {
    blocking {
      backed.search(dn.toString,Sub,BuildFilter.ALL)
    } flatMap { all =>
      if(all.getEntryCount() > 0) {
        //build the tree
        LDAPTree(all.getSearchEntries.asScala.map(x => LDAPEntry(x))).map(Some(_))
      } else None.succeed
    } catchAll {
      //a no such object error simply means that the required LDAP tree is not in the directory
      case e:LDAPSearchException if(NO_SUCH_OBJECT == e.getResultCode) => None.succeed
      case e:LDAPException => LdapResultRudderError.BackendException(s"Can not get tree '${dn}': ${e.getDiagnosticMessage}", e).fail
    }
  }
}

object RwLDAPConnection {
  import ResultCode._

  /**
   * Default error on which we don't want to throw an exception
   * but only log a message for Add operation
   */
  def onlyReportOnAdd(errorCode:ResultCode) : Boolean = {
    errorCode match {
      case NO_SUCH_ATTRIBUTE |
           UNDEFINED_ATTRIBUTE_TYPE |
           ATTRIBUTE_OR_VALUE_EXISTS |
           INVALID_ATTRIBUTE_SYNTAX |
           NO_SUCH_OBJECT |
           INVALID_DN_SYNTAX |
           ENTRY_ALREADY_EXISTS |
           ENCODING_ERROR => true
      case _ => false
    }
  }

  /**
   * Default error on which we don't want to throw an exception
   * but only log a message for Delete operation
   */
  def onlyReportOnDelete(errorCode:ResultCode) : Boolean = onlyReportOnAdd(errorCode)

  /**
   * Default error on which we don't want to throw an exception
   * but only log a message for Modify operation
   */
  def onlyReportOnModify(errorCode:ResultCode) : Boolean = {
    errorCode match {
      case NO_SUCH_ATTRIBUTE |
           UNDEFINED_ATTRIBUTE_TYPE |
           ATTRIBUTE_OR_VALUE_EXISTS |
           INVALID_ATTRIBUTE_SYNTAX |
           INVALID_DN_SYNTAX |
           ENTRY_ALREADY_EXISTS |
           ENCODING_ERROR => true
      case _ => false
    }
  }

  /**
   * Default error on which we don't want to throw an exception
   * but only log a message for ModifyDN operation
   */
  def onlyReportOnModifyDN(errorCode:ResultCode) : Boolean = onlyReportOnModify(errorCode)
}

/**
 *
 * LDAPConnection is the media to talk with the
 * LDAP directory.
 *
 * It's not here that connection properties and creation are
 * deals with. For that, look to <code>LDAPConnectionProvider</code>
 *
 * Main interaction are:
 * - get : optionally retrieve an entry
 * - search : retrieve entries based on a search request
 * - save : modify entry attributes (not the dn/rdn one)
 * - delete : delete a tree (or only one entry)
 * - getTree : retrieve a subtree
 * - move : change the dn of an entry
 *
 * @param backed
 *   UnboundID LDAPConnection to use to actually execute commands
 *
 * @param onlyReportOn*
 *   Methods that decide if such an error ResultCode should
 *   throw an exception (and probably kill the connection) or
 *   if the error only has to be logged.
 *   Typically, you want to throw an exception on error like
 *   "the directory is not available", and only get an error
 *   message (and report it to the user) on "the attribute value
 *   you tried to save is not valid for that entry".
 */
class RwLDAPConnection(
    override val backed              : UnboundidLDAPConnection
  , override val ldifFileLogger      : LDIFFileLogger
  , override val blockingModule      : Blocking
  ,              onlyReportOnAdd     : ResultCode => Boolean = RwLDAPConnection.onlyReportOnAdd
  ,              onlyReportOnModify  : ResultCode => Boolean = RwLDAPConnection.onlyReportOnModify
  ,              onlyReportOnModifyDN: ResultCode => Boolean = RwLDAPConnection.onlyReportOnModifyDN
  ,              onlyReportOnDelete  : ResultCode => Boolean = RwLDAPConnection.onlyReportOnDelete
  , override val onlyReportOnSearch  : ResultCode => Boolean = RoLDAPConnection.onlyReportOnSearch
) extends
  RoLDAPConnection(backed, ldifFileLogger, onlyReportOnSearch, blockingModule) with
  WriteOnlyEntryLDAPConnection with
  WriteOnlyTreeLDAPConnection
{

  override val logger = LoggerFactory.getLogger(classOf[RwLDAPConnection])

  /**
   * Ask the directory if it knows how to
   * delete full sub-tree in one command.
   */
  private lazy val canDeleteTree : Boolean = {
    try {
      backed.getRootDSE.supportsControl(com.unboundid.ldap.sdk.controls.SubtreeDeleteRequestControl.SUBTREE_DELETE_REQUEST_OID)
    } catch {
      case e: LDAPException =>
        logger.debug("Can not know if the LDAP server support recursive subtree delete request control, supposing not. Exception was: " + e.getMessage())
        false
    }
  }



  /*
   * //////////////////////////////////////////////////////////////////
   * // Write
   * //////////////////////////////////////////////////////////////////
   */

  /**
   * Generic method that apply a sequence of modification to a directory.
   * It can trace the full list of queries and handle result code.
   * @param MOD
   *   The modification request type
   * @param MOD => LDIFChangeRecord
   *   The method to call to transform a modification request of type MOD into
   *   an LDIFChangeRecord
   * @param MOD => LDAPResult
   *   The method to call on the backend UnboundidLDAPConnection to actually
   *   execute the modification request.
   * @param Seq[MOD]
   *   the list of modification to apply.
   */
  private def applyMods[MOD <: ReadOnlyLDAPRequest](modName: String, toLDIFChangeRecord:MOD => LDIFChangeRecord, backendAction: MOD => LDAPResult, onlyReportThat: ResultCode => Boolean)(reqs: List[MOD]) : LdapResult[Seq[LDIFChangeRecord]] = {
    if(reqs.size < 1) IO.succeed(Seq())
    else {
      ldifFileLogger.records(reqs map ( toLDIFChangeRecord (_) ))
      IO.foreach(reqs) { req =>
        applyMod(modName, toLDIFChangeRecord, backendAction, onlyReportThat)(req)
      }
    }
  }

  /**
   * Try to execute the given modification. In case of SUCCESS, return the corresponding change record.
   * In case of error, check if the error should be ignored. In such case, we assume that no modification were
   * actually done in the server: return a success with the corresponding "no change record" content.
   *
   * TODO: we most likely want to execute at most one change at a time and wait for its completion before starting
   * an other change => something like a queue of changes. But not sure it's the correct idea: openldap is certainly
   * better than us for orchestrating its changes.
   *
   */
  private def applyMod[MOD <: ReadOnlyLDAPRequest](modName: String, toLDIFChangeRecord:MOD => LDIFChangeRecord, backendAction: MOD => LDAPResult, onlyReportThat: ResultCode => Boolean)(req:MOD) : LdapResult[LDIFChangeRecord] = {
    val record = toLDIFChangeRecord(req)
    blocking {
      ldifFileLogger.records(Seq(record)) // ignore return value
      backendAction(req)
    } flatMap { res =>
      if(res.getResultCode == SUCCESS) {
        record.succeed
      } else if(onlyReportThat(res.getResultCode)) {
        LDIFNoopChangeRecord(record.getParsedDN).succeed
      } else {
        LdapResultRudderError.FailureResult(s"Error when doing action '${modName}' with and LDIF change request: ${res.getDiagnosticMessage}", res).fail
      }
    } catchAll {
      case ex:LDAPException =>
        if(onlyReportThat(ex.getResultCode)) {
          logIgnoredException(record.getDN, modName, ex)
          LDIFNoopChangeRecord(record.getParsedDN).succeed
        } else {
          LdapResultRudderError.BackendException(s"Error when doing action '${modName}' with and LDIF change request: ${ex.getDiagnosticMessage}", ex).fail
        }
      // catchAll is still a lie, and we want to crash on an other exception
      case ex:Throwable => throw ex
    }
  }


  private[this] def logIgnoredException(dn: => String, action: String, e:Throwable) : Unit = {
      val message = s"Exception ignored (by configuration) when trying to $action entry '$dn'.  Reported exception was: ${e.getMessage}"
      logger.error(message,e)
  }

  /**
   * Specialized version of applyMods for DeleteRequest modification type
   */
  private val applyDeletes = applyMods[DeleteRequest](
      "delete"
    , {req:DeleteRequest => req.toLDIFChangeRecord}
    , {req:DeleteRequest => backed.delete(req)}
    , res => NO_SUCH_OBJECT == res || onlyReportOnDelete(res) // no such object only says it's already deleted
  ) _

  /**
   * Specialized version of applyMods for AddRequest modification type
   */
  private val applyAdds = applyMods[AddRequest](
      "adds"
    , {req:AddRequest => req.toLDIFChangeRecord}
    , {req:AddRequest => backed.add(req)}
    , onlyReportOnAdd
  ) _

  private val applyAdd = applyMod[AddRequest](
      "add"
    , {req:AddRequest => req.toLDIFChangeRecord}
    , {req:AddRequest => backed.add(req)}
    , onlyReportOnAdd
  ) _

  /**
   * Specialized version of applyMods for ModifyRequest modification type
   */
  private val applyModify = applyMod[ModifyRequest](
      "modify"
    , {req:ModifyRequest => req.toLDIFChangeRecord}
    , {req:ModifyRequest => backed.modify(req)}
    , onlyReportOnModify
  ) _

  /**
   * Execute a plain modification.
   * Return the actual modification executed if success,
   * the error in other case.
   */
  override def modify(dn:DN, modifications:Modification*) : LdapResult[LDIFChangeRecord] = {
    applyModify(new ModifyRequest(dn.toString,modifications:_*))
  }

  override def move(dn:DN, newParentDn:DN, newRDN:Option[RDN] = None) : LdapResult[LDIFChangeRecord] = {
    if(
        dn.getParent == newParentDn && (
            newRDN match {
              case None => true
              case Some(rdn) => dn.getRDN == rdn
            }
        )
    ) {
      LDIFNoopChangeRecord(dn).succeed
    } else {
      applyMod[ModifyDNRequest](
          "modify DN"
        , {req:ModifyDNRequest => req.toLDIFChangeRecord}
        , {req:ModifyDNRequest => backed.modifyDN(req)}
        , onlyReportOnModify
      ) (new ModifyDNRequest(dn.toString, newRDN.getOrElse(dn.getRDN).toString, newRDN.isDefined, newParentDn.toString))
    }
  }

  override def save(entry : LDAPEntry, removeMissingAttributes:Boolean=false, forceKeepMissingAttributes:Seq[String] = Seq()) : LdapResult[LDIFChangeRecord] = {
    synchronized {
      get(entry.dn) flatMap {  //TODO if removeMissing is false, only get attribute in entry (we don't care of others)
        case None =>
          applyAdd(new AddRequest(entry.backed))
        case Some(existing) =>
          val mods = LDAPEntry.merge(existing,entry, false, removeMissingAttributes, forceKeepMissingAttributes)
          if(!mods.isEmpty) {
            applyModify(new ModifyRequest(entry.dn.toString, mods.asJava))
          } else LDIFNoopChangeRecord(entry.dn).succeed
      }
    }
  }

  override def delete(dn:DN, recurse:Boolean = true) : LdapResult[Seq[LDIFChangeRecord]] = {
    if(recurse) {
      if(canDeleteTree) {
        import com.unboundid.ldap.sdk.controls.SubtreeDeleteRequestControl
        import com.unboundid.ldap.sdk.Control
        import com.unboundid.ldap.sdk.DeleteRequest
        applyDeletes(List(new DeleteRequest(dn, Array(new SubtreeDeleteRequestControl()):Array[Control])))
      } else {
        searchSub(dn,BuildFilter.ALL,"dn").flatMap { seq =>
          val dns = seq.map(_.dn).toList.sortWith( (a,b) => a.compareTo(b) > 0)
          applyDeletes(dns.map { dn => new DeleteRequest(dn.toString) })
        }
      }
    } else {
      applyDeletes(List(new DeleteRequest(dn.toString)))
    }
  }

  /*
   * //////////////////////////////////////////////////////////////////
   * // Write Tree
   * //////////////////////////////////////////////////////////////////
   */

  protected def addTree(tree:LDAPTree) : LdapResult[Seq[LDIFChangeRecord]] = {
    applyAdds(tree.toSeq.toList.map {e => new AddRequest(e.backed) })
  }

  override def saveTree(tree:LDAPTree, deleteRemoved:Boolean=false) : LdapResult[Seq[LDIFChangeRecord]] = {
    //compose the result of unit modification
    def doSave(tree:Tree[TreeModification]) : LdapResult[Seq[LDIFChangeRecord]] = {
      //utility method to process the good method given the type of modification
      def applyTreeModification(mod:TreeModification) : LdapResult[Seq[LDIFChangeRecord]] = {
        mod match {
          case NoMod => Seq().succeed //OK
          case Add(tree) => addTree(tree)
          case Delete(tree) =>
            if(deleteRemoved) delete(tree.root.toString, true) //TODO : do we want to actually only try to delete these entry and not cut the full subtree ? likely to be error prone
            else Seq().succeed
          case Replace((dn,mods)) => IO.foreach(mods) { mod => modify(dn,mod) }
        }
      }
      ((Seq().succeed:LdapResult[Seq[LDIFChangeRecord]])/:tree.toSeq) { (records, mod) =>
        records.flatMap { seq =>
          applyTreeModification(mod).map { newRecords =>
            (seq ++ newRecords)
          }
        }
      }
    }

    for {
      _   <- blocking(ldifFileLogger.tree(tree)) mapError (e => LdapResultRudderError.BackendException(s"Error when loggin operation on LDAP tree: '${tree.parentDn}'", e))
             //process mofications
      now <- getTree(tree.root.dn.toString)
      res <- (now match {
               case None => addTree(tree)
               case Some(t) => LDAPTree.diff(t, tree, deleteRemoved) match {
                 case Some(treeMod) => treeMod.root match {
                   case x:TreeModification => doSave(treeMod.asInstanceOf[Tree[TreeModification]])
                   case x => s"Was hopping for a Tree[TreeModification] and get a Tree with root element: ${x}}".fail
                 }
                 case None => Seq().succeed
               }
             }):LdapResult[Seq[com.unboundid.ldif.LDIFChangeRecord]]
    } yield {
      res
    }
  }

}
