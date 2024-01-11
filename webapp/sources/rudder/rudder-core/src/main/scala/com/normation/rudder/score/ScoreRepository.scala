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

trait ScoreRepository {

  def getAll(): IOResult[Map[NodeId, List[Score]]]
  def getScore(nodeId:    NodeId, name:  Option[String]): IOResult[List[Score]]
  def getOneScore(nodeId: NodeId, name:  String):         IOResult[Score]
  def saveScore(nodeId:   NodeId, score: Score):       IOResult[Unit]
  def deleteScore(nodeId: NodeId, name:  Option[String]): IOResult[Unit]

}

class ScoreRepositoryImpl(doobie: Doobie, scoreSerializer: ScoreSerializer) extends ScoreRepository {

  implicit val getScoreValue: Get[ScoreValue] = Get[String].temap(ScoreValue.fromString)
  implicit val putScoreValue: Put[ScoreValue] = Put[String].contramap(_.value)

  // implicit val stateWrite: Meta[Score] = new Meta(pgDecoderGet, pgEncoderPut)

  implicit val scoreWrite: Write[(NodeId, StringScore)] = {
    Write[(String, ScoreValue, String, String, String)].contramap {
      case (nodeId: NodeId, score: StringScore) =>
        (nodeId.value, score.value, score.name, score.message, score.details)
    }
  }

  implicit val scoreRead:       Read[StringScore]           = {
    Read[(ScoreValue, String, String, String)].map { d: (ScoreValue, String, String, String) => StringScore(d._1, d._2, d._3, d._4) }
  }
  implicit val scoreWithIdRead: Read[(NodeId, StringScore)] = {
    Read[(String, ScoreValue, String, String, String)].map { d: (String, ScoreValue, String, String, String) =>
      (NodeId(d._1), StringScore(d._2, d._3, d._4, d._5))
    }
  }

  import doobie._
  override def getAll(): IOResult[Map[NodeId, List[Score]]] = {
    val q = sql"select nodeId, score, name, message, details from scoreDetails "
    transactIOResult(s"error when getting scores for node")(xa => q.query[(NodeId, StringScore)].to[List].transact(xa))
      .flatMap(c => ZIO.foreach(c)(v => scoreSerializer.parse(v._2).map((v._1, _))))
      .map(_.groupMap(_._1)(_._2))
  }

  override def getScore(nodeId: NodeId, name: Option[String]): IOResult[List[Score]] = {

    val whereNode = Some(fr"nodeId = ${nodeId.value}")
    val whereName = name.map(n => fr"name = ${n}")
    val where     = Fragments.whereAndOpt(whereNode, whereName)
    val q         = sql"select score, name, message, details from scoreDetails " ++ where
    transactIOResult(s"error when getting scores for node")(xa => q.query[StringScore].to[List].transact(xa))
      .flatMap(c => ZIO.foreach(c)(v => scoreSerializer.parse(v)))

  }

  override def getOneScore(nodeId: NodeId, name: String): IOResult[Score] = {
    val whereNode = fr"nodeId = ${nodeId.value}"
    val whereName = fr"name = ${name}"
    val where     = Fragments.whereAnd(whereNode, whereName)
    val q         = sql"select score, name, message, details from scoreDetails " ++ where
    transactIOResult(s"error when getting scores for node")(xa => q.query[StringScore].unique.transact(xa))
      .flatMap(scoreSerializer.parse(_))
  }

  override def saveScore(nodeId: NodeId, score: Score): IOResult[Unit] = {
    for {
      StringScore <- scoreSerializer.toStringScore(score)
    } yield {
      val query = {
        sql"""insert into scoreDetails (nodeId, name, score, message, details) values (${(nodeId, StringScore)})
             |  ON CONFLICT (nodeId, name) DO UPDATE
             |  SET score = ${score.value}, message = ${score.message}, details = ${StringScore.details} ; """.stripMargin
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
