package com.normation.rudder.score

import com.normation.errors._
import com.normation.errors.IOResult
import com.normation.rudder.domain.reports.ComplianceSerializable
import zio.{Ref, ZIO}
import zio.json._
import zio.json.ast.Json
import zio.syntax._
import com.normation.zio._

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
  def parse(score:  Score[Json]): IOResult[Option[Score[_]]]
  def toJson(score: Score[_]):    IOResult[Option[Score[Json]]]
}

case object ComplianceScoreSerializerService extends ScoreSerializerService {
  implicit val compliancePercentDecoder: JsonDecoder[ComplianceSerializable] = DeriveJsonDecoder.gen
  implicit val compliancePercentEncoder: JsonEncoder[ComplianceSerializable] = DeriveJsonEncoder.gen

  override def parse(score: Score[Json]): IOResult[Option[ComplianceScore]] = {

    if (score.name == "compliance") {
      score.details.as.toIO.map(ComplianceScore(score.value, score.message, _)).map(Some(_))
    } else {
      None.succeed
    }
  }

  override def toJson(score: Score[_]): IOResult[Option[Score[Json]]] = {
    score.details match {
      case details: ComplianceSerializable =>
        details.toJsonAST.toIO.map(JsonScore(score.value, score.name, score.message, _)).map(Some(_))
      case _ => None.succeed
    }
  }
}

case object SystemUpdateScoreSerializerService extends ScoreSerializerService {
  implicit val compliancePercentDecoder: JsonDecoder[SystemUpdateStats] = DeriveJsonDecoder.gen
  implicit val compliancePercentEncoder: JsonEncoder[SystemUpdateStats] = DeriveJsonEncoder.gen

  override def parse(score: Score[Json]): IOResult[Option[SystemUpdateScore]] = {

    if (score.name == "system-updates") {
      score.details.as.toIO.map(SystemUpdateScore(score.value, score.message, _)).map(Some(_))
    } else {
      None.succeed
    }
  }

  override def toJson(score: Score[_]): IOResult[Option[Score[Json]]] = {
    score.details match {
      case details: SystemUpdateStats =>
        details.toJsonAST.toIO.map(JsonScore(score.value, score.name, score.message, _)).map(Some(_))
      case _ => None.succeed
    }
  }
}

class ScoreSerializer {

  val serializers: Ref[List[ScoreSerializerService]] = Ref.make(ComplianceScoreSerializerService :: SystemUpdateScoreSerializerService :: List.empty[ScoreSerializerService]).runNow

  def registerSerializer(handler: ScoreSerializerService) = {
    serializers.update(handler :: _)
  }

  def parse(score: Score[Json]): IOResult[Score[_]] = {
    for {
      sers <- serializers.get
      acc             <- ZIO.partition(sers)(_.parse(score))
      (errors, scores) = acc
      res             <- if (scores.isEmpty) { errors.head.fail }
                         else { scores.flatten.headOption.getOrElse(score).succeed }
    } yield {
      res
    }
  }

  def toJson(score: Score[_]): IOResult[Score[Json]] = {
    for {
      sers <- serializers.get
      acc <- ZIO.partition(sers)(_.toJson(score))
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
