/*
 * Copyright 2020 Azavea
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package geotrellis.server.ogc

import java.time.ZonedDateTime

import cats.data.{NonEmptyList => NEL}
import cats.syntax.option._
import com.azavea.maml.ast.Expression
import geotrellis.proj4.CRS
import geotrellis.raster._
import geotrellis.server.extent.SampleUtils
import geotrellis.server.ogc.style._
import geotrellis.server.ogc.wms.CapabilitiesView
import geotrellis.store.{GeoTrellisPath, GeoTrellisRasterSource, LayerHeader}
import geotrellis.vector.{Extent, ProjectedExtent}
import jp.ne.opt.chronoscala.Imports._
import opengis.wms.BoundingBox

/**
 * This trait and its implementing types should be jointly sufficient, along with a WMS 'GetMap'
 *  (or a WMTS 'GetTile' or a WCS 'GetCoverage' etc etc) request to produce a visual layer
 *  (represented more fully by [[OgcLayer]].
 *  This type represents *merely* that there is some backing by which valid OGC layers
 *  can be realized. Its purpose is to provide the appropriate level of abstraction for OGC
 *  services to conveniently reuse the same data about underlying imagery
 */
trait OgcSource {
  def name: String
  def title: String
  def defaultStyle: Option[String]
  def styles: List[OgcStyle]
  def nativeExtent: Extent
  def nativeRE: GridExtent[Long]
  def extentIn(crs: CRS): Extent
  def bboxIn(crs: CRS): BoundingBox
  def nativeCrs: Set[CRS]
  def metadata: RasterMetadata
  def attributes: Map[String, String]
  def resampleMethod: ResampleMethod
  def timeInterval: Option[OgcTimeInterval]

  def nativeProjectedExtent: ProjectedExtent = ProjectedExtent(nativeExtent, nativeCrs.head)
}

trait RasterOgcSource extends OgcSource {
  def source: RasterSource

  def extentIn(crs: CRS): Extent = {
    val reprojected = source.reproject(crs)
    reprojected.extent
  }

  def bboxIn(crs: CRS): BoundingBox = {
    val reprojected = source.reproject(crs)
    CapabilitiesView.boundingBox(crs, reprojected.extent, reprojected.cellSize)
  }

  lazy val nativeRE: GridExtent[Long]      = source.gridExtent
  lazy val nativeCrs: Set[CRS]             = Set(source.crs)
  lazy val nativeExtent: Extent            = source.extent
  lazy val metadata: RasterMetadata        = source.metadata
  lazy val attributes: Map[String, String] = metadata.attributes
}

/**
 * An imagery source with a [[RasterSource]] that defines its capacities
 */
case class SimpleSource(
  name: String,
  title: String,
  source: RasterSource,
  defaultStyle: Option[String],
  styles: List[OgcStyle],
  resampleMethod: ResampleMethod
) extends RasterOgcSource {
  val timeInterval: Option[OgcTimeInterval] = None
}

case class GeoTrellisOgcSource(
  name: String,
  title: String,
  sourceUri: String,
  defaultStyle: Option[String],
  styles: List[OgcStyle],
  resampleMethod: ResampleMethod,
  timeMetadataKey: String = "times"
) extends RasterOgcSource {

  private val dataPath = GeoTrellisPath.parse(sourceUri)

  lazy val source = GeoTrellisRasterSource(dataPath)

  lazy val timeInterval: Option[OgcTimeInterval] =
    if (!source.isTemporal) None
    else if (source.times.size == 1) OgcTimeInterval(source.times.head).some
    else OgcTimeInterval(source.times.min, source.times.max.some, None).some

  /**
   * If temporal, try to match in the following order:
   *  1. To the closest time in known valid source times
   *  2. To timeInterval.start
   *  3. To the passed interval.start
   *
   *  @note If case 3 is matched, read queries to the returned
   *        RasterSource may return zero results.
   *
   * @param interval
   * @return
   */
  def sourceForTime(interval: OgcTimeInterval): GeoTrellisRasterSource =
    if (source.isTemporal) {
      val defaultTime = timeInterval.getOrElse(interval).start
      source.times.find { t =>
        interval match {
          case OgcTimeInterval(start, None, _)      => start == t
          case OgcTimeInterval(start, Some(end), _) => start <= t && t < end
          case _                                    => false
        }
      }.fold(sourceForTime(defaultTime))(sourceForTime)
    } else {
      source
    }

  def sourceForTime(time: ZonedDateTime): GeoTrellisRasterSource =
    if (source.isTemporal) GeoTrellisRasterSource(dataPath, Some(time))
    else source
}

case class MapAlgebraSourceMetadata(
  name: SourceName,
  crs: CRS,
  bandCount: Int,
  cellType: CellType,
  gridExtent: GridExtent[Long],
  resolutions: List[CellSize],
  sources: Map[String, RasterMetadata]
) extends RasterMetadata {
  /** MapAlgebra metadata usually doesn't contain a metadata that is common for all RasterSources */
  def attributes: Map[String, String] = Map.empty
  def attributesForBand(band: Int): Map[String, String] = Map.empty
}

/**
 * A complex layer, constructed from an [[Expression]] and one or more [[RasterSource]]
 *  mappings which allow evaluation of said [[Expression]]
 */
case class MapAlgebraSource(
  name: String,
  title: String,
  sources: Map[String, RasterSource],
  algebra: Expression,
  defaultStyle: Option[String],
  styles: List[OgcStyle],
  resampleMethod: ResampleMethod
) extends OgcSource {
  def extentIn(crs: CRS): Extent = {
    val reprojectedSources: NEL[RasterSource] =
      NEL.fromListUnsafe(sources.values.map(_.reproject(crs)).toList)
    val extents =
      reprojectedSources.map(_.extent)

    SampleUtils.intersectExtents(extents).getOrElse {
      throw new Exception("no intersection found among map map algebra sources")
    }
  }

  def bboxIn(crs: CRS): BoundingBox = {
    val reprojectedSources: NEL[RasterSource] =
      NEL.fromListUnsafe(sources.values.map(_.reproject(crs)).toList)
    val extents =
      reprojectedSources.map(_.extent)
    val extentIntersection =
      SampleUtils.intersectExtents(extents)
    val cellSize =
      SampleUtils.chooseLargestCellSize(reprojectedSources.map(_.cellSize))

    extentIntersection match {
      case Some(extent) =>
        CapabilitiesView.boundingBox(crs, extent, cellSize)
      case None =>
        throw new Exception("no intersection found among map map algebra sources")
    }
  }

  lazy val metadata: MapAlgebraSourceMetadata =
    MapAlgebraSourceMetadata(
      StringName(name),
      nativeCrs.head,
      minBandCount,
      cellTypes.head,
      nativeRE,
      resolutions,
      sources.mapValues(_.metadata)
    )

  lazy val nativeExtent: Extent = {
    val reprojectedSources: NEL[RasterSource] =
      NEL.fromListUnsafe(sources.values.map(_.reproject(nativeCrs.head)).toList)
    val extents =
      reprojectedSources.map(_.extent)
    val extentIntersection =
      SampleUtils.intersectExtents(extents)

    extentIntersection match {
      case Some(extent) =>
        extent
      case None =>
        throw new Exception("no intersection found among map algebra sources")
    }
  }

  lazy val nativeRE: GridExtent[Long] = {
    val reprojectedSources: NEL[RasterSource] =
      NEL.fromListUnsafe(sources.values.map(_.reproject(nativeCrs.head)).toList)
    val cellSize =
      SampleUtils.chooseSmallestCellSize(reprojectedSources.map(_.cellSize))

    new GridExtent[Long](nativeExtent, cellSize)
  }

  val timeInterval: Option[OgcTimeInterval] = None
  val attributes: Map[String, String]       = Map.empty
  lazy val nativeCrs: Set[CRS]              = sources.values.map(_.crs).toSet
  lazy val minBandCount: Int                = sources.values.map(_.bandCount).min
  lazy val cellTypes: Set[CellType]         = sources.values.map(_.cellType).toSet
  lazy val resolutions: List[CellSize]      = sources.values.flatMap(_.resolutions).toList.distinct
}
