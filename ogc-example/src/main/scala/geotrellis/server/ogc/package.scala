package geotrellis.server

import geotrellis.proj4.WebMercator
import geotrellis.raster._
import geotrellis.spark.tiling.ZoomedLayoutScheme
import com.azavea.maml.eval.directive._
import com.azavea.maml.eval.tile._
import com.azavea.maml.eval._
import com.azavea.maml.ast._
import com.azavea.maml._
import cats._
import cats.implicits._

package object ogc {
  val zls = ZoomedLayoutScheme(WebMercator, 256)

  // The actual implementation of an alternative evaluation strategy lives within this 'Directive'
  val customSlopeDirective = Directive { case (fm@FocalSlope(_), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList
      .sequence
      .map({ lr =>
        val image = lr.head
        val re = image.rasterExtent
        val zoom = zls.zoom(re.extent.xmin, re.extent.ymin, re.cellSize)
        val scale = 0.00001746438 * Math.pow(zoom, 4.826353)

        // A zfactor of 1.0 is used in the example exaggerated slope code (note the 1.0 below)
        //val zfactor = {
        //  val llExtent = re.extent.reproject(image.crs, LatLng)
        //  val middleY = llExtent.ymax - (llExtent.ymax - llExtent.ymin)
        //  val EQUATOR_METERS = 11320
        //  1 / (EQUATOR_METERS * math.cos(math.toRadians(middleY)))
        //}

        val scaledImg = image.dualMap({ i: Int => (i * scale).toInt }, { i: Double => i * scale })
        val slopeImg = scaledImg.slope(None, 1.0, re.cellSize)
        ImageResult(slopeImg)
      })
  }

  // We have to wrap up all of the behaviors we hope for the interpreter to implement
  val customSlopeInterpreter = NaiveInterpreter(
    List(
      SourceDirectives.rasterLiteral,
      SourceDirectives.intLiteral,
      SourceDirectives.dblLiteral,
      SourceDirectives.boolLiteral,
      SourceDirectives.geoJson,
      OpDirectives.additionTile orElse OpDirectives.additionInt orElse OpDirectives.additionDouble,
      OpDirectives.subtraction,
      OpDirectives.multiplicationTile orElse OpDirectives.multiplicationInt orElse OpDirectives.multiplicationDouble,
      OpDirectives.division,
      OpDirectives.pow,
      OpDirectives.maxTile orElse OpDirectives.maxInt orElse OpDirectives.maxDouble,
      OpDirectives.minTile orElse OpDirectives.minInt orElse OpDirectives.minDouble,
      OpDirectives.lessThan,
      OpDirectives.lessThanOrEqualTo,
      OpDirectives.equalTo,
      OpDirectives.notEqualTo,
      OpDirectives.greaterThan,
      OpDirectives.greaterThanOrEqualTo,
      OpDirectives.and,
      OpDirectives.or,
      OpDirectives.xor,
      OpDirectives.masking,
      OpDirectives.atan2,
      UnaryDirectives.sin,
      UnaryDirectives.cos,
      UnaryDirectives.tan,
      UnaryDirectives.sinh,
      UnaryDirectives.cosh,
      UnaryDirectives.tanh,
      UnaryDirectives.asin,
      UnaryDirectives.acos,
      UnaryDirectives.atan,
      UnaryDirectives.round,
      UnaryDirectives.floor,
      UnaryDirectives.ceil,
      UnaryDirectives.naturalLog,
      UnaryDirectives.log10,
      UnaryDirectives.sqrt,
      UnaryDirectives.abs,
      UnaryDirectives.isUndefined,
      UnaryDirectives.isDefined,
      UnaryDirectives.numericNegation,
      UnaryDirectives.logicalNegation,
      UnaryDirectives.classification,
      UnaryDirectives.imageSelection,
      FocalDirectives.max,
      FocalDirectives.min,
      FocalDirectives.mean,
      FocalDirectives.mode,
      FocalDirectives.median,
      FocalDirectives.sum,
      FocalDirectives.standardDeviation,
      customSlopeDirective,
      FocalDirectives.hillshade
    )
  )
}
