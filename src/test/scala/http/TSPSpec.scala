package clover.tsp.front.http

import java.nio.file.Paths

import clover.tsp.front.{DBInfoForm, DBInfoItem, HTTPSpec, Source => TSPSource, RowSchema, Rule, Sink, TSPTask}
import clover.tsp.front.repository.Repository
import clover.tsp.front.repository.Repository.DBInfoRepository
import io.circe.literal._
import io.circe.generic.auto._
import io.circe._
import io.circe.parser._
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl.Http4sDsl
import zio.{DefaultRuntime, Ref, UIO, ZIO}
import zio.interop.catz._

import scala.io.Source

class TSPSpec extends HTTPSpec{

  import TSPSpec._
  import TSPSpec.dbInfoService._

  val app = dbInfoService.service.orNotFound

  val dsl: Http4sDsl[TSPTaskDTO] = Http4sDsl[TSPTaskDTO]

  describe("DB Service"){
    it("should retrieve object") {

      val currentPath = Paths.get(".").toAbsolutePath
      val filePath = s"$currentPath/assets/json/req0.txt"
      val buffer = Source.fromFile(filePath)
      val jsonData = buffer.mkString
      buffer.close

      var reqBody = ""
      var expectedBody = None : Option[TSPTask]

      parse(jsonData) match{
        case Left(failure) => println("Invalid JSON :(")
        case Right(json) =>
          println(s"TSP - запрос распарсен! $json")

          // нужно как-то убирать слэши из строки
          //reqBody = json.toString()
//                        .replace("\n", "")
//                        .replace(" ", "")
//                        .replace("\\", "")

          // вот это не работает из-за того, что в примере в source.query есть кавычки с \
          // пока непонятно,как их обрабатывать
          /*expectedBody = decode[TSPTask](reqBody) match {
            case Left(failure) =>
              println(s"Что-то пошло не так: $failure")
              System.exit(1)
              None
            case Right(resp) => Some(resp)
          }*/
      }

      val req = request[TSPTaskDTO](Method.GET, "/").withEntity(reqBody)

//      runWithEnv(
//        check(
//          app.run(req),
//          Status.Ok,
//          expectedBody
//        )
//      )

    }

  }

}

object TSPSpec extends DefaultRuntime{

  val dbInfoService: DBService[Repository] = DBService[Repository]("")

  val mkEnv: UIO[Repository] =
    for {
       store <- Ref.make(TSPTask(
         Sink("", "", "", "", "", 1, 1, RowSchema("", "", "", "", "", List(""), "", List(""))),
         "",
         TSPSource("", "", "", 1, "", "", 1, "", 1, List(""), 1, 1, 1),
         List(Rule("", Map("" -> ""), "", List("")))
       ))
       counter  <- Ref.make(0L)
       repo = DBInfoRepository(store, counter)
       env = new Repository {
         override val dbInfoRepository: Repository.SimpleService[Any] = repo
         override val todoRepository: Repository.Service[Any] = null
       }
    } yield env

  def runWithEnv[E, A](task: ZIO[Repository, E, A]): A =
    unsafeRun[E, A](mkEnv.flatMap(env => task.provide(env)))

}
