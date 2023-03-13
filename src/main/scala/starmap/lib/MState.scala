package starmap.lib
import java.awt.*
import java.awt.event.*
import java.io.File
import java.net.{MalformedURLException, URL}
import javax.imageio.*
import javax.swing.*
import org.mapsforge.core.graphics.{GraphicFactory, Paint, Style}
import org.mapsforge.core.model.{BoundingBox, LatLong, Tile, Point as MapsForgePoint}
import org.mapsforge.map.awt.graphics.{AwtBitmap, AwtGraphicFactory}
import org.mapsforge.map.awt.util.AwtUtil
import org.mapsforge.map.awt.view.MapView
import org.mapsforge.map.controller.FrameBufferController
import org.mapsforge.map.datastore.MultiMapDataStore
import org.mapsforge.map.layer.cache.TileCache
import org.mapsforge.map.layer.download.TileDownloadLayer
import org.mapsforge.map.layer.download.tilesource.AbstractTileSource
import org.mapsforge.map.layer.overlay.Marker
import org.mapsforge.map.layer.renderer.{MapWorkerPool, TileRendererLayer}
import org.mapsforge.map.layer.{Layer, Layers}
import org.mapsforge.map.model.Model
import org.mapsforge.map.reader.{MapFile, ReadBuffer}
import org.mapsforge.map.rendertheme.ExternalRenderTheme
import starmap.app.*
import starmap.lib.GUIComponents.*
import starmap.lib.OpenStreetMapMapnik
import starmap.lib.Settings.*
import starmap.model.{Geo, *}
import starmap.model.overlay.{Shape, *}
import starmap.model.ui.*

import scala.collection.immutable.List

def fromToShapes(d: Option[Geo], a: Option[Geo]):List[Shape] =
  d.map( FromPin(_)).toList ++ a.map( ToPin(_)).toList

private[lib] object MState {
  // scalastyle:off
  private[lib] var o: Overlay = Nothing
  private[lib] var DepArr: (Option[Geo], Option[Geo]) = (None, None)

  private[lib] def set(f: Overlay => Overlay): Unit = o = f(o)
  private[lib] def guiState(): UIState =
    UIState(o, DepArr, Carte.itinLabel.getText)


  private[lib] def setTo(s: UIState): Unit = {
    val UIState(o, (d, a), label) = s
    MState.set(_ => o)
    MState.setTo(d, a)
    Carte.showOverlay(ShapeGroupLayer(false,fromToShapes(d,a),o))
    Carte.itinLabel.setText(label)
  }
  private[lib] def setTo(d: Option[Geo], a: Option[Geo]): Unit = DepArr = (d, a)
  // scalastyle:on
}