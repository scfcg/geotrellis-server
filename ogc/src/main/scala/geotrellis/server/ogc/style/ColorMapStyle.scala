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

package geotrellis.server.ogc.style

import geotrellis.server.ogc._

import geotrellis.raster._
import geotrellis.raster.histogram.Histogram
import geotrellis.raster.render.{ColorMap, ColorRamp}
import geotrellis.raster.render.jpg.JpgEncoder
import geotrellis.util.np.linspace


case class ColorMapStyle(
  name: String,
  title: String,
  colorMap: ColorMap,
  legends: List[LegendModel] = Nil
) extends OgcStyle {
  def renderImage(
    mbtile: MultibandTile,
    format: OutputFormat,
    hists: List[Histogram[Double]]
  ): Array[Byte] = {
    format match {
      case format: OutputFormat.Png =>
        format.render(mbtile.band(bandIndex = 0), colorMap)

      case OutputFormat.Jpg =>
        mbtile.band(bandIndex = 0).renderJpg(colorMap).bytes

      case OutputFormat.GeoTiff => ??? // Implementation necessary
    }
  }
}