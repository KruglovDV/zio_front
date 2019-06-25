package clover.tsp.front.http

import clover.tsp.front.{DBInfo, HTTPSpec}
import clover.tsp.front.repository.Repository
import clover.tsp.front.repository.Repository.DBInfoRepository

import io.circe.literal._
import io.circe.generic.auto._

import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl.Http4sDsl

import zio.{DefaultRuntime, Ref, UIO, ZIO}
import zio.interop.catz._

class DBInfoSpec extends HTTPSpec{

  import DBInfoSpec._
  import DBInfoSpec.dbInfoService._

  val app = dbInfoService.service.orNotFound

  val dsl: Http4sDsl[DBInfoDTO] = Http4sDsl[DBInfoDTO]

  describe("DB Service"){

    it("should retrieve object") {

      val req = request[DBInfoDTO](Method.GET, "/")

      runWithEnv(
        check(
          app.run(req),
          Status.Ok,
          Some(DBInfo("db_source", "db_sink", "PostgreSQL", "SELECT * FROM test_db;"))
        )
      )

    }

  }

}

object DBInfoSpec extends DefaultRuntime{

  val dbInfoService: DBService[Repository] = DBService[Repository]("")

  val mkEnv: UIO[Repository] =
    for {
       store <- Ref.make(DBInfo("", "", "", ""))
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
