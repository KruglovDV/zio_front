package clover.tsp.front.specs2

import java.nio.file.Paths

import clover.tsp.front.http.DBService
import io.circe.literal._
import io.circe.parser._
import org.specs2.specification.core.SpecStructure
import clover.tsp.front.{DBItem, HTTPSpec2}
import clover.tsp.front.repository.Repository
import clover.tsp.front.repository.Repository.DBInfoRepository
import org.http4s.implicits._
import org.http4s.dsl.Http4sDsl
import zio.{Ref, Task, UIO, ZIO}
import zio.interop.catz._
import zio.DefaultRuntime
import org.http4s.{Method, Request, Status}
import io.circe.syntax._

import scala.io.Source
import java.io.{File, FileInputStream}
import java.nio.charset.StandardCharsets

import zio.Exit.{Failure, Success}


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
          retrieve info about DB              $t1
          retrieve info about DB improved     $t2
      """

  def closeStream(is: FileInputStream) = UIO(is.close())

  def readAll(fis: FileInputStream, len: Long): Array[Byte] = {
    val content: Array[Byte] = Array.ofDim(len.toInt)
    fis.read(content)
    content
  }

  def convertBytes(is: FileInputStream, len: Long): Task[String] =
    Task.effect(new String(readAll(is, len), StandardCharsets.UTF_8))

  def t1 = {

    val buffer   = Source.fromFile(filePath)
    val jsonData = buffer.mkString
    buffer.close

    parse(jsonData) match {
      case Left(_) => {
        println("Invalid JSON :(")
        true must_== false
      }
      case Right(json) => {
        val req = request[TSPTaskDTO](Method.GET, "/").withEntity(json"""$json""")
        val res = runWithEnv(app.run(req))

        res.status must_== Status.Ok
      }
    }
  }


  def t2 =
    unsafeRun(
      for {

        file <- Task(new File(filePath))
        len = file.length

        buffer   <- ZIO.effect(Source.fromFile(filePath)).mapError(_ => new Throwable("Fail to open the file"))
        //jsonData = Task(new FileInputStream(file)).bracket(closeStream)(convertBytes(_, len))
        jsonData = buffer.mkString
        _        <- ZIO.effect(buffer.close).mapError(_ => new Throwable("Fail to close the file"))

        // Parse input data
        // parseResult <- ZIO.effect(parse(jsonData)).either
        parseResult <- ZIO.effect(parse(jsonData)).mapError(_ => new Throwable("JSON parse failed"))
        json        = parseResult.fold(_ => "left", res => res.toString)
        req         = request[TSPTaskDTO](Method.GET, "/").withEntity(json"""$json""")
        // Run HTTP effect
        res    <- ZIO.effect(app.run(req.asInstanceOf[Request[TSPTaskDTO]])).mapError(_ => new Throwable("HTTP effect failed"))
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
