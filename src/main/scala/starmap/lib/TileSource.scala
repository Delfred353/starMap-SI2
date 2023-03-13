package starmap.lib

import java.net.{MalformedURLException, URL}
import org.mapsforge.core.model.Tile
import org.mapsforge.map.layer.download.tilesource.AbstractTileSource
import starmap.lib.Settings.*

private[lib] object TileSource {
  /** @note https://wiki.openstreetmap.org/wiki/Raster_tile_providers */
  private def defaultTileRequest(tsl: TileServerRequest, tile: Tile): String =
    val p: String = if tsl.prefix.isEmpty then "" else ("/" ++ tsl.prefix)
    s"""${p}/${tile.zoomLevel}/${tile.tileX}/${tile.tileY}.png"""

  case class TileServerRequest(
                                url: Array[String],
                                prefix: String = "",
                                req: (TileServerRequest, Tile) => String = defaultTileRequest,
                                maxZoomLevel: Int = 18
                              )
  private val tile_sources =
    Map(
      "osm" -> TileServerRequest(Array("tile.openstreetmap.org")),
      "osmabc" -> TileServerRequest(
        Array("a", "b", "c").map(_ ++ ".tile.openstreetmap.org")
      ),
      "osmfr" -> TileServerRequest(
        Array("a", "b", "c")
          .map(_ ++ ".tile.openstreetmap.fr"),
        "osmfr"
      ),
      "osmde" -> TileServerRequest(Array("tile.openstreetmap.de")),
      "stamen" -> TileServerRequest(
        Array("stamen-tiles.a.ssl.fastly.net"),
        "watercolor"
      ),
      "memo" -> TileServerRequest(Array("tile.memomaps.de"), "tilegen"),
      "stamenbw" -> TileServerRequest(
        Array("stamen-tiles.a.ssl.fastly.net"),
        "toner"
      ),
      "topo" -> TileServerRequest(
        Array("a", "b", "c").map(_ ++ ".tile.opentopomap.org")
      )
    )
  val tileSource: TileServerRequest = tile_sources(TILE_SOURCE_SHORT_NAME)


}