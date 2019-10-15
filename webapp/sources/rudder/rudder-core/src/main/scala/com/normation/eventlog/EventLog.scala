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

package com.normation.eventlog

import org.joda.time.DateTime
import scala.xml._


final case class EventActor(name:String) extends AnyVal

/**
 * A type that describe on what category an event belongs to.
 */
trait EventLogCategory

private[eventlog] final case object UnknownLogCategory extends EventLogCategory

/**
 * Define the event log type, that will be serialized
 * the event class name minus "EventLog" is OK
 * It is a PartialFunction so the pattern matching are not a bottleneck anymore
 * (too much match ina  pattern matching usually fail)
 */
trait EventLogType extends PartialFunction[String, EventLogType] {
  def serialize : String

  def canRollback : Boolean

  override  def isDefinedAt(x : String) : Boolean = {
    serialize == x
  }

  def apply(x : String) = this

}

trait RollbackEventLogType extends EventLogType {
  val canRollback = true
}

trait NoRollbackEventLogType extends EventLogType {
  val canRollback = false
}


/**
 * This case class holds all the important information
 * about the EventLog.
 *
 * NOTE: EventLogDetails was introduced as a "simpler" event log,
 * and perhaps some refactoring should be done to not
 * keep the two classes.
 */
final case class EventLogDetails(
   val id            : Option[Int] = None
 , val modificationId: Option[ModificationId]
 , val principal     : EventActor
 , val creationDate  : DateTime = DateTime.now()
 , val cause         : Option[Int] = None
 , val severity      : Int = 100
 , val reason        : Option[String]
 , val details       : Elem
)

trait EventLogFilter extends PartialFunction[(EventLogType, EventLogDetails) , EventLog] {
  /**
   * An EventLogType used as identifier for that type of event.
   * Must be unique among all events.
   * Most of the time, the event class name plus Type is OK.
   */
  def eventType : EventLogType


  override  def isDefinedAt(x : (EventLogType, EventLogDetails)) : Boolean = {
    eventType == x._1
  }

  /**
   * This is used to simply build object from
   */
  def apply(x : (EventLogType, EventLogDetails)) : EventLog

}



/**
 * An EventLog is an object tracing activities on an entity.
 * It has an id (generated by the serialisation method), a type, a creation date,
 * a principal (the actor doing the action), a cause, a severity (like in syslog) and some raw data
 */
trait EventLog  {
  def eventDetails : EventLogDetails

  def id : Option[Int] = eventDetails.id // autogenerated id, by the serialization system

  def principal : EventActor = eventDetails.principal

  def creationDate : DateTime = eventDetails.creationDate

  /**
   * When we create the EventLog, it usually shouldn't have an id, so the cause cannot be set
   * That why we have the EventLogTree that holds the hierarchy of EventLogs, the cause being used only when deserializing the object
   */
  def cause : Option[Int] = eventDetails.cause


  def severity : Int = eventDetails.severity

  /**
   * Some more (technical) details about the event, in a semi-structured
   * format (XML).
   *
   * Usually, the rawData will be computed from the fields when serializing,
   * and be used to fill the fields when deserializing
   */
  def details : Elem = eventDetails.details

  /**
   * The modification id linked to that event log.
   * Not all event log must have that id, but most should.
   */
  def modificationId : Option[ModificationId] = eventDetails.modificationId

  //// not in details

  //event log type is given by the implementation class.
  //we only precise the category.
  /**
   * Big category of the event
   */
  def eventLogCategory : EventLogCategory

  /**
   * An EventLogType used as identifier for that type of event.
   * Must be unique among all events.
   * Most of the time, the event class name plus Type is OK.
   */
  def eventType : EventLogType

  def canRollBack : Boolean = eventType.canRollback
}

/**
 * The unspecialized Event Log. Used as a container when unserializing data, to be specialized later by the EventLogSpecializers
 */
final case class UnspecializedEventLog(
    override val eventDetails : EventLogDetails
) extends EventLog {
  override val eventType = UnspecializedEventLog.eventType
  override val eventLogCategory = UnknownLogCategory
}

object UnspecializedEventLog extends EventLogFilter {
  override val eventType = UnknownEventLogType

  override def apply(x : (EventLogType, EventLogDetails)) : UnspecializedEventLog = UnspecializedEventLog(x._2)
}

object EventLog {
  def withContent(nodes:NodeSeq) = <entry>{nodes}</entry>
  val emptyDetails = withContent(NodeSeq.Empty)
}

final case object UnknownEventLogType extends NoRollbackEventLogType {
  def serialize = "UnknownType"
}

