package clover.tsp.front.specs2

import java.nio.file.Paths

import cats.data.OptionT
import clover.tsp.front.{ DBItem, HTTPSpec2 }
import clover.tsp.front.http.DBService
import clover.tsp.front.repository.Repository
import clover.tsp.front.repository.Repository.DBInfoRepository
import io.circe.literal._
import io.circe.generic.auto._
import io.circe.parser._
import org.http4s.{ Method, Request, Response, Status }
import org.http4s.dsl.Http4sDsl
import zio.{ DefaultRuntime, Ref, Task, UIO, ZIO }
import org.specs2._
import org.specs2.specification.core.SpecStructure
import clover.tsp.front.{ DBItem, HTTPSpec }
import clover.tsp.front.repository.Repository
import clover.tsp.front.repository.Repository.DBInfoRepository
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl.Http4sDsl
import zio.{ DefaultRuntime, Ref, UIO, ZIO }
import zio.interop.catz._
import zio.DefaultRuntime
import cats.effect.Sync
import cats.implicits._
import org.http4s.{ EntityDecoder, Method, Request, Response, Status, Uri }
import cats.effect.Sync
import cats.implicits._
import org.http4s._
import org.scalatest.Assertion
import org.specs2._
import org.specs2.matcher.MatchResult

import scala.io.Source
import scala.io.Source

class TSPOtherSpec extends HTTPSpec2 {
  import TSPOtherSpec._
  import TSPOtherSpec.dbInfoService._

  val app                        = dbInfoService.service.orNotFound
  val dsl: Http4sDsl[TSPTaskDTO] = Http4sDsl[TSPTaskDTO]

  val currentPath = Paths.get(".").toAbsolutePath
  val filePath    = s"$currentPath/assets/json/req0.txt"

  override def is: SpecStructure =
    s2"""

        TSP REST Service should
          retrieve info about DB              
          retrieve info about DB improved     $t2

      """

  def t1 = {

    // val currentPath = Paths.get(".").toAbsolutePath
    // val filePath    = s"$currentPath/assets/json/req0.txt"
    val buffer   = Source.fromFile(filePath)
    val jsonData = buffer.mkString
    buffer.close

    // val expectedBody = Some(DBItem("some data"))
    //  var cond: MatchResult[Any] = null

    parse(jsonData) match {
      case Left(_) => {
        println("Invalid JSON :(")
        true must_== false
      }
      case Right(json) => {
        val req = request[TSPTaskDTO](Method.GET, "/").withEntity(json"""$json""")
        val res = runWithEnv(app.run(req))
        // var cond: MatchResult[Any] = null

//        res.as[DBItem].map(x => {
//          cond = x must_== expectedBody.get
//        })

        res.status must_== Status.Ok
      }
    }
    // cond
  }

  def t2 =
    unsafeRun(
      for {

        // This should be inside a bracket
        buffer   <- ZIO.effect(Source.fromFile(filePath)).mapError(_ => new Throwable("Fail to open the file"))
        jsonData = buffer.toString
        _        <- ZIO.effect(buffer.close).mapError(_ => new Throwable("Fail to close the file"))

        // Parse input data
        // parseResult <- ZIO.effect(parse(jsonData)).either
        parseResult <- ZIO.effect(parse(jsonData)).mapError(_ => new Throwable("JSON parse failed"))
        req         = request[TSPTaskDTO](Method.GET, "/").withEntity(json"""${parseResult.toString}""")

        // Run HTTP effect
        res    <- ZIO.effect(app.run(req)).mapError(_ => new Throwable("HTTP effect failed"))
        status = res.map(_.status)

      } yield status must_== Status.Ok
    )
}

object TSPOtherSpec extends DefaultRuntime {

  val dbInfoService: DBService[Repository] = DBService[Repository]("")

  val mkEnv: UIO[Repository] =
    for {
      store   <- Ref.make(DBItem("some data"))
      counter <- Ref.make(0L)
      repo    = DBInfoRepository(store, counter)
      env = new Repository {
        override val dbInfoRepository: Repository.SimpleService[Any] = repo
        override val todoRepository: Repository.Service[Any]         = null
      }
    } yield env

  def runWithEnv[E, A](task: ZIO[Repository, E, A]): A =
    unsafeRun[E, A](mkEnv.flatMap(env => task.provide(env)))

}
