package starmap.lib
import org.mapsforge.core.graphics.Filter
import org.mapsforge.core.util.Parameters

private[lib] object Settings {

  Parameters.NUMBER_OF_THREADS =
    3 // Multithreaded map rendering, default is 1. See doc. at above link.
  Parameters.MAXIMUM_BUFFER_SIZE = 6500000 // Default is 10000000.
  Parameters.SQUARE_FRAME_BUFFER =
    false // Square frame buffer, default is true.
  val DEFAULT_ZOOM_LEVEL: Int = 16
  val TILE_SOURCE_SHORT_NAME = "osmfr" // Map tiles server.  "osm", "osmabc", "osmfr", "osmde", "stamen", "memo", "stamenbw", "topo"
  val MAP_COLOR_FILTER =
    Filter.GRAYSCALE // GRAYSCALE, GRAYSCALE_INVERT, INVERT, NONE
  val CIRCLE_SHAPE_RADIUS: Float = 3f
}