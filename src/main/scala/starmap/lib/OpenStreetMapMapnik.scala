package starmap.lib

import java.net.URL
import org.mapsforge.map.layer.download.tilesource.AbstractTileSource
import starmap.lib.OpenStreetMapMapnik.getHostName
import TileSource.*

/**
 * @note
 *   Adapapted from :
 *   https://github.com/mapsforge/mapsforge/blob/master/mapsforge-map/src/main/java/org/mapsforge/map/layer/download/tilesource/TileSource.java
 */
private[lib] object OpenStreetMapMapnik
    extends AbstractTileSource(
      tileSource.url,
      443
    ) {

  def getParallelRequestsLimit(): Int = 8
  def getTileUrl(tile: org.mapsforge.core.model.Tile): java.net.URL =
    URL(
      "https",
      getHostName(),
      this.port,
      tileSource.req(tileSource, tile)
    );
  def getZoomLevelMax(): Byte = 19
  def getZoomLevelMin(): Byte = 0
  def hasAlpha(): Boolean = false
}