package clover.tsp.front.http

import clover.tsp.front.{DBInfoForm, HTTPSpec, DBInfoItem}
import clover.tsp.front.repository.Repository
import clover.tsp.front.repository.Repository.DBInfoRepository

import io.circe.literal._
import io.circe.generic.auto._

import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl.Http4sDsl

import zio.{DefaultRuntime, Ref, UIO, ZIO}
import zio.interop.catz._

class DBInfoFormSpec extends HTTPSpec{

  import DBInfoFormSpec._
  import DBInfoFormSpec.dbInfoService._

  val app = dbInfoService.service.orNotFound

  val dsl: Http4sDsl[DBInfoDTO] = Http4sDsl[DBInfoDTO]

  describe("DB Service"){

    it("should retrieve object") {

      val body = json"""{"source": "db_source", "sink": "db_sink", "dbType": "PostgreSQL", "query": "SELECT * FROM test_db;"}"""
      val req = request[DBInfoDTO](Method.GET, "/").withEntity(body)

      runWithEnv(
        check(
          app.run(req),
          Status.Ok,
          Some(DBInfoItem("some data"))
        )
      )

    }

  }

}

object DBInfoFormSpec extends DefaultRuntime{

  val dbInfoService: DBService[Repository] = DBService[Repository]("")

  val mkEnv: UIO[Repository] =
    for {
       store <- Ref.make(DBInfoItem("some data"))
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
