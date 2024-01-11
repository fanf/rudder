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
}

trait ScoreSerializerService {
  def parse(score:         StringScore): IOResult[Option[Score]]
  def toJson(score:        Score):       IOResult[Option[StringScore]]
  def toJsonEncoder(score: Score):       Option[JsonEncoder[Score]]
}

case object ComplianceScoreSerializerService extends ScoreSerializerService {
  import ScoreSerializer._
  implicit val compliancePercentDecoder: JsonDecoder[ComplianceSerializable] = DeriveJsonDecoder.gen
  implicit val compliancePercentEncoder: JsonEncoder[ComplianceSerializable] = DeriveJsonEncoder.gen
  implicit val encoderComplianceScore:   JsonEncoder[ComplianceScore]        = DeriveJsonEncoder.gen

  override def parse(score: StringScore): IOResult[Option[ComplianceScore]] = {

    if (score.name == "compliance") {
      score.details.fromJson[ComplianceSerializable].toIO.map(ComplianceScore(score.value, score.message, _)).map(Some(_))
    } else {
      None.succeed
    }
  }

  override def toJson(score: Score): IOResult[Option[StringScore]] = {
    score match {
      case complianceScore: ComplianceScore =>
        Some(StringScore(score.value, score.name, score.message, complianceScore.details.toJson)).succeed
      case _ => None.succeed
    }
  }

  override def toJsonEncoder(score: Score): Option[JsonEncoder[Score]] = {
    score match {
      case _: ComplianceScore =>
        Some(encoderComplianceScore.asInstanceOf[JsonEncoder[Score]]) // perhaps we can do better than that
      case _ => None
    }
  }
}

case object SystemUpdateScoreSerializerService extends ScoreSerializerService {
  import ScoreSerializer._
  implicit val compliancePercentDecoder: JsonDecoder[SystemUpdateStats] = DeriveJsonDecoder.gen
  implicit val compliancePercentEncoder: JsonEncoder[SystemUpdateStats] = DeriveJsonEncoder.gen
  implicit val encoderSystemUpdateScore: JsonEncoder[SystemUpdateScore] = DeriveJsonEncoder.gen

  override def parse(score: StringScore): IOResult[Option[SystemUpdateScore]] = {

    if (score.name == "system-updates") {
      score.details.fromJson[SystemUpdateStats].toIO.map(SystemUpdateScore(score.value, score.message, _)).map(Some(_))
    } else {
      None.succeed
    }
  }

  override def toJson(score: Score): IOResult[Option[StringScore]] = {
    score match {
      case systemScore: SystemUpdateScore =>
        Some(StringScore(score.value, score.name, score.message, systemScore.details.toJson)).succeed
      case _ => None.succeed
    }
  }

  override def toJsonEncoder(score: Score): Option[JsonEncoder[Score]] = {
    score match {
      case _: SystemUpdateScore => Some(encoderSystemUpdateScore.asInstanceOf[JsonEncoder[Score]])
      case _ => None
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

  def toJson(score: Score): IOResult[StringScore] = {
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

  def getJsonEncoder(score: Score): IOResult[JsonEncoder[Score]] = {
    for {
      sers  <- serializers.get
      optEnc = sers.collectFirst {
                 case s if (s.toJsonEncoder(score).isDefined) => s.toJsonEncoder(score)
               } // can do better here too
      res   <- optEnc match {
                 case Some(Some(enc)) => enc.succeed
                 case _               => Inconsistency(s"Error: no encoder exist for the score: ${score}").fail
               }

    } yield {
      res
    }

  }

}
