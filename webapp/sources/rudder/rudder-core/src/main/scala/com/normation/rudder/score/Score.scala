package com.normation.rudder.score

import com.normation.errors.IOResult
import com.normation.inventory.domain.Inventory
import com.normation.inventory.domain.InventoryError.Inconsistency
import com.normation.inventory.domain.NodeId
import com.normation.inventory.domain.SoftwareUpdateKind
import com.normation.rudder.campaigns.CampaignLogger
import com.normation.rudder.domain.logger.ReportLoggerPure
import com.normation.rudder.domain.reports.CompliancePercent
import com.normation.rudder.domain.reports.ComplianceSerializable
import com.normation.rudder.score.ScoreValue.A
import com.normation.rudder.score.ScoreValue.B
import com.normation.rudder.score.ScoreValue.C
import com.normation.rudder.score.ScoreValue.D
import com.normation.rudder.score.ScoreValue.E
import com.normation.zio._
import zio.Ref
import zio.ZIO
import zio.json.ast.Json
import zio.syntax._

sealed trait ScoreValue {
  def value: String
}

object ScoreValue {
  case object A extends ScoreValue { val value = "A" }
  case object B extends ScoreValue { val value = "B" }
  case object C extends ScoreValue { val value = "C" }
  case object D extends ScoreValue { val value = "D" }
  case object E extends ScoreValue { val value = "E" }

  val allValues: Set[ScoreValue] = ca.mrvisser.sealerate.values

  def fromString(s: String) = allValues.find(_.value == s.toUpperCase()) match {
    case None    => Left(s"${s} is not valid status value, accepted values are ${allValues.map(_.value).mkString(", ")}")
    case Some(v) => Right(v)
  }
}

trait Score[T] {
  def name:    String
  def value:   ScoreValue
  def message: String
  def details: T
}

case class NoDetailsScore(name: String, value: ScoreValue, message: String)
case class JsonScore(value: ScoreValue, name: String, message: String, details: Json) extends Score[Json]
case class ComplianceScore(value: ScoreValue, message: String, details: ComplianceSerializable)
    extends Score[ComplianceSerializable] {
  override val name = "compliance"
}

case class SystemUpdateStats(
    nbPackages:  Int,
    security:    Option[Int],
    updates:     Option[Int],
    defect:      Option[Int],
    enhancement: Option[Int],
    other:       Option[Int]
)

case class SystemUpdateScore(value: ScoreValue, message: String, details: SystemUpdateStats) extends Score[SystemUpdateStats] {
  override val name = "system-update"
}
case class GlobalScore(value: ScoreValue, message: String, details: List[NoDetailsScore])

object GlobalScoreService {
  def computeGlobalScore(oldScore: List[NoDetailsScore], scores: List[Score[_]]): GlobalScore = {

    val correctScores = scores.foldRight(oldScore) {
      case (newScore, acc) =>
        NoDetailsScore(newScore.name, newScore.value, newScore.message) :: acc.filterNot(_.name == newScore.name)
    }
    import ScoreValue._
    val score         = if (correctScores.exists(_.value == E)) { E }
    else if (correctScores.exists(_.value == D)) { D }
    else if (correctScores.exists(_.value == C)) {
      C
    } else if (correctScores.exists(_.value == B)) {
      B
    } else A
    GlobalScore(score, s"There is at least a Score with ${score.value}", correctScores)
  }
}

trait ScoreEvent

case class InventoryScoreEvent(nodeId: NodeId, inventory: Inventory)                  extends ScoreEvent
case class ComplianceScoreEvent(nodeId: NodeId, compliancePercent: CompliancePercent) extends ScoreEvent

trait ScoreEventHandler {
  def handle(event: ScoreEvent): IOResult[List[(NodeId, List[Score[_]])]]
}

object ComplianceScoreEventHandler extends ScoreEventHandler {
  def handle(event: ScoreEvent): IOResult[List[(NodeId, List[Score[_]])]] = {

    event match {
      case ComplianceScoreEvent(n, percent) =>
        val p     = ComplianceSerializable.fromPercent(percent)
        val score = if (percent.compliance >= 100) {
          ComplianceScore(A, "Node is compliant at 100%", p)
        } else if (percent.compliance >= 75) {
          ComplianceScore(B, "Node is compliant at least at 75%", p)
        } else if (percent.compliance >= 50) {
          ComplianceSerializable.fromPercent(percent)
          ComplianceScore(C, "Node is compliant at least at 50%", p)
        } else if (percent.compliance >= 25) {
          ComplianceScore(D, "Node is compliant at least at 25%", p)
        } else {
          ComplianceScore(E, "Node is compliant at less then 25%", p)
        }
        ((n, score :: Nil) :: Nil).succeed
      case _                                => Nil.succeed
    }
  }
}

object InventoryEventHandler extends ScoreEventHandler {
  def handle(event: ScoreEvent): IOResult[List[(NodeId, List[Score[_]])]] = {

    event match {
      case InventoryScoreEvent(n, inventory) =>
        val sum         = inventory.node.softwareUpdates.size
        val security    = inventory.node.softwareUpdates.filter(_.kind == SoftwareUpdateKind.Security).size
        val patch       = inventory.node.softwareUpdates.filter(_.kind == SoftwareUpdateKind.None).size
        val defect      = inventory.node.softwareUpdates.filter(_.kind == SoftwareUpdateKind.Defect).size
        val enhancement = inventory.node.softwareUpdates.filter(_.kind == SoftwareUpdateKind.Enhancement).size
        val other       = inventory.node.softwareUpdates.filter { s =>
          s.kind match {
            case SoftwareUpdateKind.Other(_) => true
            case _                           => false
          }
        }.size
        val stats       = SystemUpdateStats(
          sum,
          if (security > 0) Some(security) else None,
          if (patch > 0) Some(patch) else None,
          if (defect > 0) Some(defect) else None,
          if (enhancement > 0) Some(enhancement) else None,
          if (other > 0) Some(other) else None
        )
        val score       = if (security == 0 && sum < 50) {
          SystemUpdateScore(A, "Node has no security updates and less than 50 updates available", stats)
        } else if (security < 5) {
          SystemUpdateScore(B, s"Node has ${security} security updates available (less than 5)", stats)
        } else if (security < 20) {
          SystemUpdateScore(C, s"Node has ${security} security updates available (less than 20)", stats)
        } else if (security < 50) {
          SystemUpdateScore(D, s"Node has ${security} security updates available (less than 50)", stats)
        } else {
          SystemUpdateScore(E, s"Node has ${security} security updates available (more than 50)", stats)
        }
        ((n, score :: Nil) :: Nil).succeed
      case _                                 => Nil.succeed
    }
  }
}

class ScoreService(globalScoreRepository: GlobalScoreRepository, scoreRepository: ScoreRepository) {
  private[this] val cache:      Ref[Map[NodeId, GlobalScore]]    = globalScoreRepository.getAll().flatMap(Ref.make(_)).runNow
  private[this] val scoreCache: Ref[Map[NodeId, List[Score[_]]]] = scoreRepository.getAll().flatMap(Ref.make(_)).runNow

  def getAll():                       IOResult[Map[NodeId, GlobalScore]] = cache.get
  def getGlobalScore(nodeId: NodeId): IOResult[GlobalScore]              = {
    for {
      c   <- cache.get
      res <-
        c.get(nodeId) match {
          case Some(g) => g.succeed
          case None    => Inconsistency("No global score for node").fail
        }
    } yield {
      res
    }
  }

  def getScoreDetails(nodeId: NodeId): IOResult[List[Score[_]]] = {
    for {
      c   <- scoreCache.get
      res <-
        c.get(nodeId) match {
          case Some(g) => g.succeed
          case None    => Inconsistency("No score for node").fail
        }
    } yield {
      res
    }
  }

  def cleanScore(name: String) = {
    for {
      _ <- cache.update(_.map { case (id, gscore) => (id, gscore.copy(details = gscore.details.filterNot(_.name == name))) })
    } yield {}
  }

  def update(newScores: Map[NodeId, List[Score[_]]]) = {
    for {
      c           <- cache.get
      // sc <- scoreCache.get
      updatedValue = (for {
                       (nodeId, newScores) <- newScores
                     } yield {
                       val oldScores = c.get(nodeId) match {
                         case None           => Nil
                         case Some(oldScore) => oldScore.details
                       }
                       (nodeId, GlobalScoreService.computeGlobalScore(oldScores, newScores))
                     })

      updateScoreCache <- ZIO.foreach(newScores.toList) {
                            case (nodeId, scores) =>
                              ZIO.foreach(scores)(score => {
                                scoreRepository.saveScore(nodeId, score) *>
                                scoreCache.update(sc =>
                                  sc + ((nodeId, score :: sc.get(nodeId).getOrElse(Nil).filter(_.name != score.name)))
                                )
                              })

                          }
      updatedCache     <- ZIO.foreach(updatedValue.toList) {
                            case (nodeId, score) => globalScoreRepository.save(nodeId, score) *> cache.update(_.+((nodeId, score)))
                          }
    } yield {}

  }
}

class ScoreServiceManager(readScore: ScoreService) {

  val handlers: Ref[List[ScoreEventHandler]] =
    Ref.make(ComplianceScoreEventHandler :: InventoryEventHandler :: List.empty[ScoreEventHandler]).runNow

  def registerHandler(handler: ScoreEventHandler) = {
    handlers.update(handler :: _)
  }

  def handleEvent(scoreEvent: ScoreEvent) = {
    (for {
      h       <- handlers.get
      _ <- ReportLoggerPure.info(s"new event ${scoreEvent}")
      handled <- ZIO.foreach(h)(_.handle(scoreEvent))
      _ <- ReportLoggerPure.info(s"new score ${handled}")
      newScore = handled.flatMap(_.groupMapReduce(_._1)(_._2)(_ ++ _)).toMap
      _       <- readScore.update(newScore)
    } yield {}).catchAll(err => CampaignLogger.error(s"${err.fullMsg}"))
  }
}
