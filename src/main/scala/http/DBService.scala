package clover.tsp.front.http

import clover.tsp.front.{DBInfoForm, simpleRepository}
import clover.tsp.front.repository.Repository
import io.circe.generic.auto._
import io.circe.{Decoder, Encoder}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes}
import com.typesafe.scalalogging.Logger
import zio.TaskR
import zio.interop.catz._

final case class DBService[R <: Repository](rootUri: String) {

  type DBInfoDTO[A] = TaskR[R, A]

  implicit def circeJsonDecoder[A](implicit decoder: Decoder[A]): EntityDecoder[DBInfoDTO, A] = jsonOf[DBInfoDTO, A]

  implicit def circeJsonEncoder[A](implicit encoder: Encoder[A]): EntityEncoder[DBInfoDTO, A] =
    jsonEncoderOf[DBInfoDTO, A]

  val dsl: Http4sDsl[DBInfoDTO] = Http4sDsl[DBInfoDTO]
  import dsl._

  val log = Logger("DBService")

  def service: HttpRoutes[DBInfoDTO] =
    HttpRoutes.of[DBInfoDTO] {

      case req @ GET -> Root =>
        log.debug("Root method called")
        for {
          dbInfoFrom <- req.as[DBInfoForm]
          dbInfoItem <- simpleRepository.get(dbInfoFrom)
          res <- Ok(dbInfoItem)
        } yield res
    }

}
