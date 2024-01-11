package com.normation.rudder.score

import com.normation.errors._
import com.normation.errors.IOResult
import com.normation.rudder.domain.reports.ComplianceSerializable
import com.normation.zio._
import zio.Ref
import zio.ZIO
import zio.json._
import zio.syntax._

object ScoreSerializer {

  implicit val scoreValueEncoder: JsonEncoder[ScoreValue] = JsonEncoder[String].contramap(_.value)
  implicit val scoreValueDecoder: JsonDecoder[ScoreValue] = JsonDecoder[String].mapOrFail(ScoreValue.fromString)

  implicit val noDetailsScoreEncoder: JsonEncoder[NoDetailsScore] = DeriveJsonEncoder.gen
  implicit val noDetailsScoreDecoder: JsonDecoder[NoDetailsScore] = DeriveJsonDecoder.gen

  implicit val globalScoreEncoder: JsonEncoder[GlobalScore] = DeriveJsonEncoder.gen
  implicit val globalScoreDecoder: JsonDecoder[GlobalScore] = DeriveJsonDecoder.gen

  implicit val jsonScoreEncoder: JsonEncoder[JsonScore] = DeriveJsonEncoder.gen
  implicit val jsonScoreDecoder: JsonDecoder[JsonScore] = DeriveJsonDecoder.gen
}

trait ScoreSerializerService {
  def parse(score:         StringScore): IOResult[Option[Score]]
  def toStringScore(score: Score):       Option[StringScore]
  def toJson(score:        Score):       IOResult[Option[JsonScore]]
}

case object ComplianceScoreSerializerService extends ScoreSerializerService {
  implicit val compliancePercentDecoder: JsonDecoder[ComplianceSerializable] = DeriveJsonDecoder.gen
  implicit val compliancePercentEncoder: JsonEncoder[ComplianceSerializable] = DeriveJsonEncoder.gen

  override def parse(score: StringScore): IOResult[Option[ComplianceScore]] = {

    if (score.name == "compliance") {
      score.details.fromJson[ComplianceSerializable].toIO.map(ComplianceScore(score.value, score.message, _)).map(Some(_))
    } else {
      None.succeed
    }
  }

  override def toStringScore(score: Score): Option[StringScore] = {
    score match {
      case complianceScore: ComplianceScore =>
        Some(StringScore(score.value, score.name, score.message, complianceScore.details.toJson))
      case _ => None
    }
  }

  override def toJson(score: Score): IOResult[Option[JsonScore]] = {
    score match {
      case complianceScore: ComplianceScore =>
        complianceScore.details.toJsonAST.toIO.map(JsonScore(score.value, score.name, score.message, _)).map(Some(_))
      case _ => None.succeed
    }
  }
}

case object SystemUpdateScoreSerializerService extends ScoreSerializerService {
  implicit val compliancePercentDecoder: JsonDecoder[SystemUpdateStats] = DeriveJsonDecoder.gen
  implicit val compliancePercentEncoder: JsonEncoder[SystemUpdateStats] = DeriveJsonEncoder.gen

  override def parse(score: StringScore): IOResult[Option[SystemUpdateScore]] = {

    if (score.name == "system-updates") {
      score.details.fromJson[SystemUpdateStats].toIO.map(SystemUpdateScore(score.value, score.message, _)).map(Some(_))
    } else {
      None.succeed
    }
  }

  override def toStringScore(score: Score): Option[StringScore] = {
    score match {
      case systemScore: SystemUpdateScore =>
        Some(StringScore(score.value, score.name, score.message, systemScore.details.toJson))
      case _ => None
    }
  }

  override def toJson(score: Score): IOResult[Option[JsonScore]] = {
    score match {
      case systemScore: SystemUpdateScore =>
        systemScore.details.toJsonAST.toIO.map(JsonScore(score.value, score.name, score.message, _)).map(Some(_))
      case _ => None.succeed
    }
  }
}

class ScoreSerializer {

  val serializers: Ref[List[ScoreSerializerService]] =
    Ref.make(ComplianceScoreSerializerService :: SystemUpdateScoreSerializerService :: List.empty[ScoreSerializerService]).runNow

  def registerSerializer(handler: ScoreSerializerService) = {
    serializers.update(handler :: _)
  }

  def parse(score: StringScore): IOResult[Score] = {
    for {
      sers            <- serializers.get
      acc             <- ZIO.partition(sers)(_.parse(score))
      (errors, scores) = acc
      res             <- if (scores.isEmpty) { errors.head.fail }
                         else { scores.flatten.headOption.getOrElse(score).succeed }
    } yield {
      res
    }
  }

  def toStringScore(score: Score): IOResult[StringScore] = {
    for {
      sers <- serializers.get
      enc   = sers.view.map(_.toStringScore(score)).collectFirst { case Some(enc) => enc }
      res  <- enc match {
                case Some(score) => score.succeed
                case None        => Inconsistency(s"No encoder found for score ${score}").fail
              }
    } yield {
      res
    }
  }

  def toJson(score: Score): IOResult[JsonScore] = {
    for {
      sers            <- serializers.get
      acc             <- ZIO.partition(sers)(_.toJson(score))
      (errors, scores) = acc
      res             <- scores.flatten.headOption match {
                           case Some(score) => score.succeed
                           case None        => errors.head.fail
                         }

    } yield {
      res
    }
  }
}
