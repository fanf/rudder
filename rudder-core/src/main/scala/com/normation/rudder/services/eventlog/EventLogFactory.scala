package com.normation.rudder.services.eventlog
import com.normation.rudder.domain.policies.ModifyRuleDiff
import com.normation.rudder.domain.policies.ModifyTechniqueDiff
import com.normation.rudder.domain.policies.DeleteTechniqueDiff
import com.normation.rudder.domain.eventlog._
import com.normation.rudder.domain.policies.AddRuleDiff
import com.normation.eventlog.EventActor
import com.normation.rudder.domain.policies.DeleteRuleDiff
import com.normation.rudder.services.marshalling.RuleSerialisation
import org.joda.time.DateTime
import com.normation.eventlog.EventLog
import com.normation.rudder.domain.Constants
import com.normation.rudder.domain.policies.SimpleDiff
import net.liftweb.util.Helpers._
import com.normation.rudder.domain.policies.RuleTarget
import scala.xml.Text
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.policies.AddDirectiveDiff
import com.normation.cfclerk.domain.SectionSpec
import com.normation.rudder.domain.policies.ModifyDirectiveDiff
import com.normation.rudder.domain.policies.DeleteDirectiveDiff
import com.normation.rudder.services.marshalling.DirectiveSerialisation
import com.normation.cfclerk.domain.TechniqueVersion
import com.normation.rudder.domain.policies.SectionVal
import com.normation.rudder.domain.nodes.AddNodeGroupDiff
import com.normation.rudder.domain.nodes.ModifyNodeGroupDiff
import com.normation.rudder.domain.nodes.DeleteNodeGroupDiff
import com.normation.rudder.services.marshalling.NodeGroupSerialisation
import com.normation.rudder.services.marshalling.ActiveTechniqueSerialisation
import com.normation.rudder.domain.queries.Query
import com.normation.inventory.domain.NodeId
import com.normation.eventlog.EventLogDetails
import scala.xml._
import com.normation.eventlog.ModificationId
import com.normation.eventlog.EventLogDetails
import com.normation.rudder.domain.workflows.WorkflowStepChange
import com.normation.rudder.domain.parameters._
import com.normation.rudder.services.marshalling.GlobalParameterSerialisation
import com.normation.rudder.services.marshalling.APIAccountSerialisation
import com.normation.rudder.api._

trait EventLogFactory {

  def getAddRuleFromDiff(
      id             : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal      : EventActor
    , addDiff        : AddRuleDiff
    , creationDate   : DateTime = DateTime.now()
    , severity       : Int = 100
    , reason         : Option[String]
  ) : AddRule


  def getDeleteRuleFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , deleteDiff  : DeleteRuleDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : DeleteRule

  def getModifyRuleFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , modifyDiff  : ModifyRuleDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : ModifyRule

  def getAddDirectiveFromDiff(
      id                 : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal          : EventActor
    , addDiff            : AddDirectiveDiff
    , varsRootSectionSpec: SectionSpec
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : AddDirective

  def getDeleteDirectiveFromDiff(
      id                 : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal          : EventActor
    , deleteDiff         : DeleteDirectiveDiff
    , varsRootSectionSpec: SectionSpec
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : DeleteDirective

  def getModifyDirectiveFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , modifyDiff  : ModifyDirectiveDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : ModifyDirective

  def getAddNodeGroupFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , addDiff     : AddNodeGroupDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : AddNodeGroup

  def getDeleteNodeGroupFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , deleteDiff  : DeleteNodeGroupDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : DeleteNodeGroup

  def getModifyNodeGroupFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , modifyDiff  : ModifyNodeGroupDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : ModifyNodeGroup

  def getModifyTechniqueFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , modifyDiff  : ModifyTechniqueDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : ModifyTechnique

  def getDeleteTechniqueFromDiff(
      id                 : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal          : EventActor
    , deleteDiff         : DeleteTechniqueDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : DeleteTechnique

  def getAddGlobalParameterFromDiff(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , addDiff            : AddGlobalParameterDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : AddGlobalParameter

  def getDeleteGlobalParameterFromDiff(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , deleteDiff         : DeleteGlobalParameterDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : DeleteGlobalParameter

  def getModifyGlobalParameterFromDiff(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , modifyDiff         : ModifyGlobalParameterDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : ModifyGlobalParameter

  def getChangeRequestFromDiff(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , diff               : ChangeRequestDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : ChangeRequestEventLog

  def getWorkFlowEventFromStepChange(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , step               : WorkflowStepChange
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : WorkflowStepChanged

  def getCreateApiAccountFromDiff(
      id             : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal      : EventActor
    , addDiff        : AddApiAccountDiff
    , creationDate   : DateTime = DateTime.now()
    , severity       : Int = 100
    , reason         : Option[String]
  ) : CreateAPIAccountEventLog

  def getModifyApiAccountFromDiff(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , modifyDiff         : ModifyApiAccountDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
 ) : ModifyAPIAccountEventLog

 def getDeleteApiAccountFromDiff(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , deleteDiff         : DeleteApiAccountDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : DeleteAPIAccountEventLog
}

class EventLogFactoryImpl(
    crToXml         : RuleSerialisation
  , piToXml         : DirectiveSerialisation
  , groutToXml      : NodeGroupSerialisation
  , techniqueToXml  : ActiveTechniqueSerialisation
  , parameterToXml  : GlobalParameterSerialisation
  , ApiAccountToXml : APIAccountSerialisation
) extends EventLogFactory {

  /////
  ///// rules /////
  /////

  override def getAddRuleFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , addDiff     : AddRuleDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : AddRule= {
    val details = EventLog.withContent(crToXml.serialise(addDiff.rule) % ("changeType" -> "add"))
    AddRule(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }


  override def getDeleteRuleFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , deleteDiff  : DeleteRuleDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : DeleteRule= {
    val details = EventLog.withContent(crToXml.serialise(deleteDiff.rule) % ("changeType" -> "delete"))
    DeleteRule(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getModifyRuleFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , modifyDiff  : ModifyRuleDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : ModifyRule = {
    val details = EventLog.withContent {
      scala.xml.Utility.trim(
        <rule changeType="modify" fileFormat={Constants.XML_CURRENT_FILE_FORMAT.toString}>
          <id>{modifyDiff.id.value}</id>
          <displayName>{modifyDiff.name}</displayName>{
            modifyDiff.modName.map(x => SimpleDiff.stringToXml(<name/>, x) ) ++
            modifyDiff.modSerial.map(x => SimpleDiff.intToXml(<serial/>, x ) ) ++
            modifyDiff.modTarget.map(x =>
              SimpleDiff.toXml[Set[RuleTarget]](<targets/>, x){ targets =>
                targets.toSeq.map { t => <target>{t.target}</target> }
              }
            ) ++
            modifyDiff.modDirectiveIds.map(x =>
              SimpleDiff.toXml[Set[DirectiveId]](<directiveIds/>, x){ ids =>
                ids.toSeq.map { id => <id>{id.value}</id> }
              }
            ) ++
            modifyDiff.modShortDescription.map(x =>
              SimpleDiff.stringToXml(<shortDescription/>, x ) ) ++
            modifyDiff.modLongDescription.map(x =>
              SimpleDiff.stringToXml(<longDescription/>, x ) ) ++
            modifyDiff.modIsActivatedStatus.map(x =>
              SimpleDiff.booleanToXml(<isEnabled/>, x ) ) ++
            modifyDiff.modIsSystem.map(x =>
              SimpleDiff.booleanToXml(<isSystem/>, x ) )
          }
        </rule>
      )
    }
    ModifyRule(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  /////
  ///// directive /////
  /////

  override def getAddDirectiveFromDiff(
      id                 : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal          : EventActor
    , addDiff            : AddDirectiveDiff
    , varsRootSectionSpec: SectionSpec
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) = {
    val details = EventLog.withContent(piToXml.serialise(addDiff.techniqueName, varsRootSectionSpec, addDiff.directive) % ("changeType" -> "add"))
    AddDirective(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getDeleteDirectiveFromDiff(
      id                 : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal          : EventActor
    , deleteDiff         : DeleteDirectiveDiff
    , varsRootSectionSpec: SectionSpec
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) = {
    val details = EventLog.withContent(piToXml.serialise(deleteDiff.techniqueName, varsRootSectionSpec, deleteDiff.directive) % ("changeType" -> "delete"))
    DeleteDirective(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getModifyDirectiveFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , modifyDiff  : ModifyDirectiveDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) = {
    val details = EventLog.withContent{
      scala.xml.Utility.trim(
        <directive changeType="modify" fileFormat={Constants.XML_CURRENT_FILE_FORMAT.toString}>
          <id>{modifyDiff.id.value}</id>
          <techniqueName>{modifyDiff.techniqueName.value}</techniqueName>
          <displayName>{modifyDiff.name}</displayName>{
            modifyDiff.modName.map(x => SimpleDiff.stringToXml(<name/>, x) ) ++
            modifyDiff.modTechniqueVersion.map(x => SimpleDiff.toXml[TechniqueVersion](<techniqueVersion/>, x){ v =>
              Text(v.toString)
            } ) ++
            modifyDiff.modParameters.map(x => SimpleDiff.toXml[SectionVal](<parameters/>, x){ sv =>
              SectionVal.toXml(sv)
            } ) ++
            modifyDiff.modShortDescription.map(x => SimpleDiff.stringToXml(<shortDescription/>, x ) ) ++
            modifyDiff.modLongDescription.map(x => SimpleDiff.stringToXml(<longDescription/>, x ) ) ++
            modifyDiff.modPriority.map(x => SimpleDiff.intToXml(<priority/>, x ) ) ++
            modifyDiff.modIsActivated.map(x => SimpleDiff.booleanToXml(<isEnabled/>, x ) ) ++
            modifyDiff.modIsSystem.map(x => SimpleDiff.booleanToXml(<isSystem/>, x ) )
          }
        </directive>
      )
    }
    ModifyDirective(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }


  override def getAddNodeGroupFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , addDiff     : AddNodeGroupDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : AddNodeGroup = {
    val details = EventLog.withContent(groutToXml.serialise(addDiff.group) % ("changeType" -> "add"))
    AddNodeGroup(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getDeleteNodeGroupFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , deleteDiff  : DeleteNodeGroupDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : DeleteNodeGroup = {
    val details = EventLog.withContent(groutToXml.serialise(deleteDiff.group) % ("changeType" -> "delete"))
    DeleteNodeGroup(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getModifyNodeGroupFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , modifyDiff  : ModifyNodeGroupDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : ModifyNodeGroup = {
    val details = EventLog.withContent{
      scala.xml.Utility.trim(<nodeGroup changeType="modify" fileFormat={Constants.XML_CURRENT_FILE_FORMAT.toString}>
        <id>{modifyDiff.id.value}</id>
        <displayName>{modifyDiff.name}</displayName>{
          modifyDiff.modName.map(x => SimpleDiff.stringToXml(<name/>, x) ) ++
          modifyDiff.modDescription.map(x => SimpleDiff.stringToXml(<description/>, x ) ) ++
          modifyDiff.modQuery.map(x => SimpleDiff.toXml[Option[Query]](<query/>, x){ t =>
            t match {
              case None => <none/>
              case Some(y) => Text(y.toJSONString)
            }
          } ) ++
          modifyDiff.modIsDynamic.map(x => SimpleDiff.booleanToXml(<isDynamic/>, x ) ) ++
          modifyDiff.modNodeList.map(x => SimpleDiff.toXml[Set[NodeId]](<nodeIds/>, x){ ids =>
              ids.toSeq.map { id => <id>{id.value}</id> }
            } ) ++
          modifyDiff.modIsActivated.map(x => SimpleDiff.booleanToXml(<isEnabled/>, x ) ) ++
          modifyDiff.modIsSystem.map(x => SimpleDiff.booleanToXml(<isSystem/>, x ) )
        }
      </nodeGroup>)
    }
    ModifyNodeGroup(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getModifyTechniqueFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , modifyDiff  : ModifyTechniqueDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : ModifyTechnique = {
    val details = EventLog.withContent{
      scala.xml.Utility.trim(<activeTechnique changeType="modify" fileFormat={Constants.XML_CURRENT_FILE_FORMAT.toString}>
        <id>{modifyDiff.id.value}</id>
        <techniqueName>{modifyDiff.name}</techniqueName>{
          modifyDiff.modIsEnabled.map(x => SimpleDiff.booleanToXml(<isEnabled/>, x )).toSeq
        }
      </activeTechnique>)
    }
    ModifyTechnique(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getDeleteTechniqueFromDiff(
      id          : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal   : EventActor
    , deleteDiff  : DeleteTechniqueDiff
    , creationDate: DateTime = DateTime.now()
    , severity    : Int = 100
    , reason      : Option[String]
  ) : DeleteTechnique = {
    val details = EventLog.withContent(techniqueToXml.serialise(deleteDiff.technique) % ("changeType" -> "delete"))
    DeleteTechnique(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getAddGlobalParameterFromDiff(
      id             : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal      : EventActor
    , addDiff        : AddGlobalParameterDiff
    , creationDate   : DateTime = DateTime.now()
    , severity       : Int = 100
    , reason         : Option[String]
  ) : AddGlobalParameter = {
    val details = EventLog.withContent(parameterToXml.serialise(addDiff.parameter) % ("changeType" -> "add"))
    AddGlobalParameter(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getDeleteGlobalParameterFromDiff(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , deleteDiff         : DeleteGlobalParameterDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : DeleteGlobalParameter = {
    val details = EventLog.withContent(parameterToXml.serialise(deleteDiff.parameter) % ("changeType" -> "delete"))
    DeleteGlobalParameter(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getModifyGlobalParameterFromDiff(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , modifyDiff         : ModifyGlobalParameterDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : ModifyGlobalParameter = {
    val details = EventLog.withContent{
      scala.xml.Utility.trim(<globalParameter changeType="modify" fileFormat={Constants.XML_CURRENT_FILE_FORMAT.toString}>
        <name>{modifyDiff.name.value}</name>{
          modifyDiff.modValue.map(x => SimpleDiff.stringToXml(<value/>, x) ) ++
          modifyDiff.modDescription.map(x => SimpleDiff.stringToXml(<description/>, x ) ) ++
          modifyDiff.modOverridable.map(x => SimpleDiff.booleanToXml(<overridable/>, x ) )
        }
      </globalParameter>)
    }
    ModifyGlobalParameter(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getChangeRequestFromDiff(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , diff               : ChangeRequestDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) = {

    def eventlogDetails(xml:Elem):EventLogDetails = {
      EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = xml
      , creationDate = creationDate
      , reason = reason
      , severity = severity)
    }
    val diffName = diff.diffName.map(x => SimpleDiff.stringToXml(<diffName/>, x )).getOrElse(NodeSeq.Empty)
    val diffDesc = diff.diffDescription.map(x => SimpleDiff.stringToXml(<diffDescription/>, x )).getOrElse(NodeSeq.Empty)
    val xml = <changeRequest>
                <id>{diff.changeRequest.id}</id>
                <name>{diff.changeRequest.info.name}</name>
                <description>{diff.changeRequest.info.description}</description>
                {(diffName ++ diffDesc)}
              </changeRequest>

    diff match {
      case _:AddChangeRequestDiff      =>
        val details = EventLog.withContent( scala.xml.Utility.trim(xml % ("changeType" -> "add")))
        AddChangeRequest(eventlogDetails(details))
      case _:DeleteChangeRequestDiff   =>
        val details = EventLog.withContent( scala.xml.Utility.trim(xml % ("changeType" -> "delete")))
        DeleteChangeRequest(eventlogDetails(details))
      case mod:ModifyToChangeRequestDiff =>
        val details = EventLog.withContent((xml))
        ModifyChangeRequest(eventlogDetails(details))
    }
  }

  def getWorkFlowEventFromStepChange(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , step               : WorkflowStepChange
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) = {
    val xml =
      <workflowStep>
        <changeRequestId>{step.id}</changeRequestId>
        <from>{step.from}</from>
        <to>{step.to}</to>
      </workflowStep>
    val details = EventLog.withContent( scala.xml.Utility.trim(xml))
    val data = EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity
      )
    WorkflowStepChanged(data)
  }

  // API Account

  override def getCreateApiAccountFromDiff(
      id             : Option[Int] = None
    , modificationId : Option[ModificationId] = None
    , principal      : EventActor
    , addDiff        : AddApiAccountDiff
    , creationDate   : DateTime = DateTime.now()
    , severity       : Int = 100
    , reason         : Option[String]
  ) : CreateAPIAccountEventLog = {
    val details = EventLog.withContent(ApiAccountToXml.serialise(addDiff.apiAccount) % ("changeType" -> "add"))
    CreateAPIAccountEventLog(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

 def getModifyApiAccountFromDiff(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , diff               : ModifyApiAccountDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
 ) = {
   val details = EventLog.withContent{
      scala.xml.Utility.trim(<apiAccount changeType="modify" fileFormat={Constants.XML_CURRENT_FILE_FORMAT.toString}>
        <id>{diff.id.value}</id>{
          diff.modName.map(x => SimpleDiff.stringToXml(<name/>, x) ) ++
          diff.modToken.map(x => SimpleDiff.stringToXml(<token/>, x) ) ++
          diff.modDescription.map(x => SimpleDiff.stringToXml(<description/>, x ) ) ++
          diff.modIsEnabled.map(x => SimpleDiff.booleanToXml(<enabled/>, x ) )
        }
      </apiAccount>)
    }
    ModifyAPIAccountEventLog(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }

  override def getDeleteApiAccountFromDiff(
      id                 : Option[Int] = None
    , modificationId     : Option[ModificationId] = None
    , principal          : EventActor
    , deleteDiff         : DeleteApiAccountDiff
    , creationDate       : DateTime = DateTime.now()
    , severity           : Int = 100
    , reason             : Option[String]
  ) : DeleteAPIAccountEventLog = {
    val details = EventLog.withContent(ApiAccountToXml.serialise(deleteDiff.apiAccount) % ("changeType" -> "delete"))
    DeleteAPIAccountEventLog(EventLogDetails(
        id = id
      , modificationId = modificationId
      , principal = principal
      , details = details
      , creationDate = creationDate
      , reason = reason
      , severity = severity))
  }
}


