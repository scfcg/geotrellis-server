package geotrellis.server.http4s.maml

import geotrellis.server.core.persistence.MamlStore
import MamlStore.ops._

import com.azavea.maml.ast.Expression
import com.azavea.maml.ast.codec.tree.ExpressionTreeCodec._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._
import io.circe._
import io.circe.syntax._
import cats._
import cats.implicits._
import cats.effect._
import com.typesafe.scalalogging.LazyLogging
import geotrellis.raster._
import geotrellis.raster.render._

import scala.math._
import java.net.URI
import java.util.UUID
import scala.util.Try
import scala.collection.mutable


class MamlPersistenceService[ExpressionStore: MamlStore](val store: ExpressionStore) extends Http4sDsl[IO] with LazyLogging {

  implicit val expressionDecoder = jsonOf[IO, Expression]

  def routes: HttpService[IO] = HttpService[IO] {
    case req @ POST -> Root / IdVar(key) =>
      (for {
        expr <- req.as[Expression]
        _    <- IO { logger.info(s"Attempting to store expression ($req.bodyAsText) at key ($key)") }
        res  <- store.putMaml(key, expr)
      } yield res).attempt flatMap {
        case Right(created) =>
          Created()
        case Left(err) =>
          logger.debug(err.toString, err)
          InternalServerError(err.toString)
      }

    case req @ GET -> Root / IdVar(key) =>
      logger.info(s"Attempting to retrieve expression at key ($key)")
      store.getMaml(key) flatMap {
        case Some(expr) => Ok(expr.asJson)
        case None => NotFound()
      }
  }
}

