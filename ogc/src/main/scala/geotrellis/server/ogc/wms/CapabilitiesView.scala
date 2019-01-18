package geotrellis.server.ogc.wms

import geotrellis.proj4.{CRS, LatLng}

import java.net.URI
import scala.xml.Elem

class CapabilitiesView(model: RasterSourcesModel, crs: CRS = LatLng) {
  def toXML: Elem = {
    import opengis.wms._

    val service = Service(
      Name = Name.fromString("WMS", opengis.wms.defaultScope),
      Title = "GeoTrellis WMS",
      OnlineResource = OnlineResource(),
      KeywordList = Some(KeywordList(Keyword("WMS") :: Keyword("GeoTrellis") :: Nil))
    )

    val capability = {
      val getCapabilities = OperationType(
        Format = List("text/xml"),
        DCPType = List(DCPType(
          HTTP(Get = Get(OnlineResource(Map(
            "@{http://www.w3.org/1999/xlink}href" -> scalaxb.DataRecord(new URI("http://localhost/wms")),
            "@{http://www.w3.org/1999/xlink}type" -> scalaxb.DataRecord(xlink.Simple: xlink.TypeType)))))
        )))

      val getMap = OperationType(
        Format = List("text/xml"),
        DCPType = List(DCPType(
          HTTP(Get = Get(OnlineResource(Map(
            "@{http://www.w3.org/1999/xlink}href" -> scalaxb.DataRecord(new URI("http://localhost/wms")),
            "@{http://www.w3.org/1999/xlink}type" -> scalaxb.DataRecord(xlink.Simple: xlink.TypeType)))))
        )))

      Capability(
        Request = Request(GetCapabilities = getCapabilities, GetMap = getMap, GetFeatureInfo = None),
        Exception = Exception(List("XML", "INIMAGE", "BLANK")),
        Layer = Some(model.toLayer(crs))
      )
    }

    val ret = scalaxb.toXML[opengis.wms.WMS_Capabilities](
      obj = WMS_Capabilities(service, capability, Map("@version" -> scalaxb.DataRecord("1.3.0"))),
      namespace = None,
      elementLabel = Some("WMS_Capabilities"),
      scope = opengis.wms.defaultScope,
      typeAttribute = false
    )

    ret.asInstanceOf[scala.xml.Elem]
  }
}