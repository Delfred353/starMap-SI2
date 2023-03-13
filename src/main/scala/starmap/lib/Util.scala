package starmap.lib

import java.awt.Color
import starmap.model.*
import starmap.model.overlay.{FromPin, ToPin}

// import org.mapsforge.core.graphics.{Color => mapsColor}
import org.mapsforge.core.graphics.{Paint, Style}
import org.mapsforge.core.model.LatLong
import org.mapsforge.core.util.LatLongUtils
import org.mapsforge.map.layer.overlay.{Polyline, Marker}
import org.mapsforge.map.layer.overlay.{FixedPixelCircle => MapsForgeCircle}
import org.mapsforge.map.layer.Layer
import org.mapsforge.map.awt.graphics.{AwtGraphicFactory, AwtBitmap}
import javax.imageio._

import java.io.File

import scala.collection.JavaConverters._

import starmap.model.Geo

import starmap.model.overlay.{
  Circle,
  Path,
  Connect,
  DottedPath,
  StartPin,
  Shape,
  EndPin
}
import starmap.lib.Settings.CIRCLE_SHAPE_RADIUS

object Util {

  /**
   * @param p1
   *   un point de géolocalisation
   * @param p2
   *   un point de géolocalisation
   * @return
   *   la distance de Vincenty entre les deux points p1 et p2.
   */
  def vincenty(p1: Geo, p2: Geo): Double =
    LatLongUtils.vincentyDistance(point2latlong(p1), point2latlong(p2))

  private[lib] def point2latlong(p: Geo): LatLong = new LatLong(p.lat, p.long)

  private[lib] def toPolyline(
      edges: List[Geo],
      c: Color,
      dashed: Boolean
  ): Polyline = {
    // paint object
    val paint: Paint = AwtGraphicFactory.INSTANCE.createPaint();
    paint.setColor(c.getRGB());
    paint.setStrokeWidth(3);
    paint.setStyle(Style.STROKE);
    if (dashed) {
      paint.setDashPathEffect((Array[Float](2f, 8f)))
      paint.setStrokeWidth(5)
    }

    // polyline object
    val polyline: Polyline = new Polyline(paint, AwtGraphicFactory.INSTANCE);
    val coordinateList = polyline.getLatLongs();
    edges.foreach { e => coordinateList.add(point2latlong(e)) }

    polyline
  }

  private[lib] def toCircle(c: Circle): MapsForgeCircle = {

    val paintFill: Paint = AwtGraphicFactory.INSTANCE.createPaint();
    paintFill.setColor(org.mapsforge.core.graphics.Color.TRANSPARENT)
    paintFill.setStyle(Style.FILL)
    paintFill.setStrokeWidth(3)

    val paintStroke: Paint = AwtGraphicFactory.INSTANCE.createPaint();
    paintStroke.setColor(c.color.getRGB())
    paintStroke.setStyle(Style.STROKE)
    paintStroke.setStrokeWidth(2)

    new MapsForgeCircle(
      point2latlong(c.point),
      CIRCLE_SHAPE_RADIUS,
      paintFill,
      paintStroke
    )
  }

  private[lib] def scaleWithHeight(h: Int, i: AwtBitmap): Unit = {
    val height = i.getHeight
    val width = i.getWidth
    i.scaleTo(width * h / height, h)
  }

  private[lib] def toMarker(p: Geo, filename: String): Marker = {
    val img: AwtBitmap = new AwtBitmap(ImageIO.read(new File(filename)))
    scaleWithHeight(30, img)
    new Marker(Util.point2latlong(p), img, 0, -20)
  }

  private[lib] def shapeToLayer(s: Shape): Layer = {
    s match {
      case DottedPath(points, c)   => toPolyline(points, c, true)
      case Path(points, c)       => toPolyline(points, c, false)
      case circle @ Circle(p, c) => toCircle(circle)
      case StartPin(p)           => toMarker(p, "img/start.png")
      case EndPin(p)             => toMarker(p, "img/end.png")
      case FromPin(p)            => toMarker(p, "img/pin_red.png")
      case ToPin(p)              => toMarker(p, "img/pin_green.png")
      case Connect(p)            => toMarker(p, "img/connect.png")
    }
  }
  // scalastyle:on
}