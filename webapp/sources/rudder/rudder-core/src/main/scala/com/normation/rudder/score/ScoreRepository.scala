package com.normation.rudder.score

import com.normation.errors.IOResult
import com.normation.inventory.domain.NodeId
import com.normation.rudder.db.Doobie
import doobie.Fragments
import doobie.Get
import doobie.Put
import doobie.Read
import doobie.Write
import doobie.implicits._
import doobie.implicits.toSqlInterpolator
import zio.ZIO
import zio.interop.catz._
import zio.json.ast.Json

trait ScoreRepository {

  def getAll(): IOResult[Map[NodeId, List[Score[_]]]]
  def getScore(nodeId:    NodeId, name:  Option[String]): IOResult[List[Score[_]]]
  def getOneScore(nodeId: NodeId, name:  String):         IOResult[Score[_]]
  def saveScore(nodeId:   NodeId, score: Score[_]):       IOResult[Unit]
  def deleteScore(nodeId: NodeId, name:  Option[String]): IOResult[Unit]

}

class ScoreRepositoryImpl(doobie: Doobie, scoreSerializer: ScoreSerializer) extends ScoreRepository {

  import com.normation.rudder.db.json.implicits._
  implicit val getScoreValue: Get[ScoreValue] = Get[String].temap(ScoreValue.fromString)
  implicit val putScoreValue: Put[ScoreValue] = Put[String].contramap(_.value)

  // implicit val stateWrite: Meta[Score[_]] = new Meta(pgDecoderGet, pgEncoderPut)

  implicit val scoreWrite: Write[(NodeId, Score[Json])] = {
    Write[(String, ScoreValue, String, String, Json)].contramap {
      case (nodeId: NodeId, score: Score[Json]) =>
        (nodeId.value, score.value, score.name, score.message, score.details)
    }
  }

  implicit val scoreRead:       Read[Score[Json]]           = {
    Read[(ScoreValue, String, String, Json)].map { d: (ScoreValue, String, String, Json) => JsonScore(d._1, d._2, d._3, d._4) }
  }
  implicit val scoreWithIdRead: Read[(NodeId, Score[Json])] = {
    Read[(String, ScoreValue, String, String, Json)].map { d: (String, ScoreValue, String, String, Json) =>
      (NodeId(d._1), JsonScore(d._2, d._3, d._4, d._5))
    }
  }

  import doobie._
  override def getAll(): IOResult[Map[NodeId, List[Score[_]]]] = {
    val q = sql"select nodeId, score, name, message, details from scoreDetails "
    transactIOResult(s"error when getting scores for node")(xa => q.query[(NodeId, Score[Json])].to[List].transact(xa))
      .flatMap(c => ZIO.foreach(c)(v => scoreSerializer.parse(v._2).map((v._1, _))))
      .map(_.groupMap(_._1)(_._2))
  }

  override def getScore(nodeId: NodeId, name: Option[String]): IOResult[List[Score[_]]] = {

    val whereNode = Some(fr"nodeId = ${nodeId.value}")
    val whereName = name.map(n => fr"name = ${n}")
    val where     = Fragments.whereAndOpt(whereNode, whereName)
    val q         = sql"select score, name, message, details from scoreDetails " ++ where
    transactIOResult(s"error when getting scores for node")(xa => q.query[Score[Json]].to[List].transact(xa))
      .flatMap(c => ZIO.foreach(c)(v => scoreSerializer.parse(v)))

  }

  override def getOneScore(nodeId: NodeId, name: String): IOResult[Score[_]] = {
    val whereNode = fr"nodeId = ${nodeId.value}"
    val whereName = fr"name = ${name}"
    val where     = Fragments.whereAnd(whereNode, whereName)
    val q         = sql"select score, name, message, details from scoreDetails " ++ where
    transactIOResult(s"error when getting scores for node")(xa => q.query[Score[Json]].unique.transact(xa))
      .flatMap(scoreSerializer.parse(_))
  }

  override def saveScore(nodeId: NodeId, score: Score[_]): IOResult[Unit] = {
    for {
      jsonScore <- scoreSerializer.toJson(score)
    } yield {
      val query = {
        sql"""insert into scoreDetails (nodeId, name, score, message, details) values (${(nodeId, jsonScore)})
             |  ON CONFLICT (nodeId, name) DO UPDATE
             |  SET score = ${score.value}, message = ${score.message}, details = ${jsonScore.details} ; """.stripMargin
      }

      transactIOResult(s"error when inserting global score for node '${nodeId.value}''")(xa => query.update.run.transact(xa)).unit
    }
  }

  override def deleteScore(nodeId: NodeId, name: Option[String]): IOResult[Unit] = {
    val whereNode = Some(fr"nodeId = ${nodeId.value}")
    val whereName = name.map(n => fr"name = ${n}")
    val where     = Fragments.whereAndOpt(whereNode, whereName)
    val q         = sql"delete from scoreDetails " ++ where
    transactIOResult(s"error when getting global score for node ${nodeId.value}")(xa => q.update.run.transact(xa).unit)
  }
}
