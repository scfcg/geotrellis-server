package geotrellis.server.http4s.maml

import geotrellis.server.core.persistence.MamlStore
import MamlStore.ops._

import com.azavea.maml.ast.Expression
import com.azavea.maml.ast.codec.tree.ExpressionTreeCodec._
import com.azavea.maml.eval._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._
import io.circe._
import io.circe.syntax._
import cats.implicits._
import cats.effect._
import cats.data.Validated._
import cats._
import com.typesafe.scalalogging.LazyLogging
import geotrellis.raster._
import geotrellis.raster.render._

import scala.math._
import java.net.URI
import java.util.{UUID, NoSuchElementException}
import scala.util.Try
import scala.collection.mutable


class MamlTmsService[ExpressionStore: MamlStore](
  expressionStore: ExpressionStore,
  interpreter: BufferingInterpreter = BufferingInterpreter.DEFAULT
)(implicit timer: Timer[IO]) extends Http4sDsl[IO] with LazyLogging {

  implicit val expressionDecoder = jsonOf[IO, Expression]

  def routes: HttpService[IO] = HttpService[IO] {
    case req @ GET -> Root / IdVar(key) / IntVar(z) / IntVar(x) / IntVar(y) =>
      (for {
        maybeExpr <- expressionStore.getMaml(key)
        expr      <- IO { maybeExpr.get }.recoverWith({ case _: NoSuchElementException => throw MamlStore.ExpressionNotFound(key) })
        _         <- IO { logger.info(s"Attempting to interpret expression ($expr) at key ($key)") }
        resolved  <- BindSourcesWithContext.forTmsTile(expr)(timer)(z, x, y)
      } yield interpreter(resolved).andThen(_.as[Tile])).attempt flatMap {
        case Right(Valid(tile)) =>
          Ok(tile.renderPng(ColorRamps.Viridis).bytes)
        case Right(Invalid(errs)) =>
          BadRequest(errs.asJson)
        case Left(MamlStore.ExpressionNotFound(err)) =>
          logger.info(err.toString)
          NotFound()
        case Left(err) =>
          logger.debug(err.toString, err)
          InternalServerError(err.toString)
      }
  }
}

