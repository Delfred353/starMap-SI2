package starmap.lib

import java.awt.event.*
import java.awt.{Shape => AwtShape, *}
import java.io.File
import java.net.{MalformedURLException, URL}
import java.util.UUID
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
import scala.jdk.CollectionConverters.*
import starmap.app.*
import starmap.lib.GUIComponents.*
import starmap.lib.OpenStreetMapMapnik
import starmap.lib.Settings.*
import starmap.model.*
import starmap.model.overlay.*
import starmap.model.ui.*

object Carte {

  private val GRAPHIC_FACTORY: GraphicFactory = AwtGraphicFactory.INSTANCE

  private val clickTypes: Array[Object] =
    Array(ZCancelClick, BEndClick, AStartClick)

  private val mapView: MapView = new MapView()
  private var center: Geo = Geo(0,0)
  private val frame: JFrame = new JFrame()

  System.setProperty("http.agent", "Mozilla")

  /** Lancement de l'interface graphique Starmap
    * @param center
    *   le point sur lequel la carte sera centrée à l'affichage
    * @param frametitle
    *   le titre de la fenêtre graphique
    */
  def createMapView(center: Geo, frametitle: String): Unit = {
    val debug = false
    Carte.center = center
    frame.setTitle(frametitle)

    mapView.getMapScaleBar.setVisible(true)
    val screenSize: Dimension =
      java.awt.Toolkit.getDefaultToolkit.getScreenSize
    mapView.setPreferredSize(
      new Dimension(screenSize.width / 2, screenSize.height * 8 / 10)
    )

    // Tile cache
    val tileCache: TileCache = AwtUtil.createTileCache(
      mapView.getModel.displayModel.getTileSize,
      mapView.getModel.frameBufferModel.getOverdrawFactor,
      1024,
      new File(
        System.getProperty("java.io.tmpdir"),
        UUID.randomUUID().toString
      )
    )

    // Raster
    mapView.getModel.displayModel.setFixedTileSize(256)
    mapView.getModel.displayModel.setFilter(
      MAP_COLOR_FILTER
    )

    val tileDownloadLayer: TileDownloadLayer = new TileDownloadLayer(
      tileCache,
      mapView.getModel.mapViewPosition,
      OpenStreetMapMapnik,
      GRAPHIC_FACTORY
    ) {

      override def onTap(
          tapLatLong: LatLong,
          layerXY: MapsForgePoint,
          tapXY: MapsForgePoint
      ): Boolean = {
        if (debug) println("Point cliqué: " + tapLatLong)
        val clickType = JOptionPane.showOptionDialog(
          frame,
          "Point cliqué: \n" + tapLatLong + " \n Quelle action?",
          "Choix du type de marqueur",
          JOptionPane.DEFAULT_OPTION,
          JOptionPane.QUESTION_MESSAGE,
          new ImageIcon(),
          clickTypes,
          AStartClick
        )
        try {
          clickTypes(clickType) match {
            case ct: ClickType =>
              val p = Geo(tapLatLong.latitude, tapLatLong.longitude)
              val c = ct match {
                case AStartClick  => StartDef
                case BEndClick    => EndDef
                case ZCancelClick => Cancel
              }
              MState.setTo(
                ExosActionsBoutons.cliquerCarte(MState.guiState(), p, c))
            case _ => ()
          }
        } catch {
          case _: java.lang.ArrayIndexOutOfBoundsException => ()
        }
        true
      }
    }
    mapView.addLayer(tileDownloadLayer)
    val maplayer = mapView.getLayerManager.getLayers.get(0)
    tileDownloadLayer.start()

    val model: Model = mapView.getModel
    model.mapViewPosition.setCenter(Util.point2latlong(center))
    model.mapViewPosition.setZoomLevel(DEFAULT_ZOOM_LEVEL.toByte)
    placeComponents(mapView, maplayer, frame)

  }

  private def clearAllButMap(m: MapView): Unit = {
    for (l <- m.getLayerManager.getLayers.asScala.tail) {
      m.getLayerManager.getLayers.remove(l)
    }
  }

  // Mis le label ici, besoin de le changer globalement. Moche.
  private[lib] val itinLabel: JLabel = new JLabel(
    ExosActionsBoutons.message(MState.DepArr)
  )
  itinLabel.setVerticalAlignment(javax.swing.SwingConstants.TOP)
  itinLabel.setMaximumSize(new Dimension(400, 300))
  itinLabel.setPreferredSize(new Dimension(400, 300))

  /** Place les composants graphiques dans la fenêtre de l'application, et
    * définit leur réaction.
    * @param mapView
    *   la mapview
    * @param maplayer
    *   le layer du dessous
    * @param frame
    *   la fenêtre graphique
    */
  private[lib] def placeComponents(
      mapView: MapView,
      maplayer: Layer,
      frame: JFrame
  ): Unit = {

    val cboxTout: JRadioButton = new JRadioButton("Toutes les lignes")
    cboxTout.addItemListener(new ItemListener() {
      def itemStateChanged(ie: ItemEvent): Unit = {
        if (cboxTout.isSelected) {
          MState.setTo(
            ExosActionsBoutons.cliquerToutesLesLignes(MState.guiState()))
        }
      }
    })
    val lines: Array[LineItem] = ExosLignes
      .nomsDesLignes(allLines)
      .map(f => LineItem(f._1, f._2, f._3))
      .toArray
    val choixLigne: JComboBox[LineItem] = new JComboBox(lines)
    val cboxChoixLigne: JRadioButton = new JRadioButton("Ma ligne")

    val cboxRien: JButton = new JButton("Recommencer")
    cboxRien.addActionListener(new ActionListener() {
      def actionPerformed(ae: ActionEvent): Unit = {

        MState.setTo(
          ExosActionsBoutons.cliquerRecommencer(MState.guiState()))
        cboxTout.setSelected(false)
        cboxChoixLigne.setSelected(false)
      }
    })

    choixLigne.setPreferredSize(new Dimension(400, 30))
    choixLigne.setMaximumSize(new Dimension(400, 30))
    choixLigne.addActionListener(new ActionListener() {
      def actionPerformed(ae: ActionEvent): Unit = {
        if (cboxChoixLigne.isSelected) {
          val id = choixLigne.getSelectedItem.asInstanceOf[LineItem].id
          MState.setTo(
            ExosActionsBoutons.cliquerMaLigne(MState.guiState(), id))
        }
      }
    })
    cboxChoixLigne.addItemListener(new ItemListener() {
      def itemStateChanged(ie: ItemEvent): Unit = {
        if (cboxChoixLigne.isSelected  && !lines.isEmpty) {
          val id = choixLigne.getSelectedItem.asInstanceOf[LineItem].id
          MState.setTo(
            ExosActionsBoutons.cliquerMaLigne(MState.guiState(), id))
        }
      }
    })

    val butItin: JButton = new JButton("Rechercher")
    butItin.addActionListener(new ActionListener() {
      def actionPerformed(ae: ActionEvent): Unit = {
        MState.DepArr match {
          case (Some(_), Some(_)) =>
            cboxTout.setSelected(false)
            cboxRien.setSelected(false)
            cboxChoixLigne.setSelected(false)
          case _                  => ()
        }
        MState.setTo(
          ExosActionsBoutons.cliquerRechercher(MState.guiState()))
      }
    })

    val showBtnGroup: NoneSelectedButtonGroup = new NoneSelectedButtonGroup()
    for (b <- Seq(cboxTout, cboxRien, cboxChoixLigne)) showBtnGroup.add(b)

    val rienPanel: JPanel = new JPanel(new GridBagLayout())
    rienPanel.add(cboxRien)

    val toutPanel: JPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    toutPanel.add(cboxTout)

    val lignePanel: JPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    for (b <- Seq(cboxChoixLigne, choixLigne)) lignePanel.add(b)

    val controlLignePanel: JPanel = new JPanel(new BorderLayout())
    controlLignePanel.setBorder(BorderFactory.createTitledBorder("Lignes"))
    val controlLigneBox: Box = Box.createVerticalBox()
    for (b <- Seq(toutPanel, lignePanel)) {
      b.setAlignmentX(Component.LEFT_ALIGNMENT)
      controlLigneBox.add(b)
    }
    controlLignePanel.add(controlLigneBox, BorderLayout.NORTH)

    val itinPanel: JPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val itinPanelBox: Box = Box.createHorizontalBox()
    for (b <- Seq(butItin, itinLabel)) {
      b.setAlignmentY(Component.TOP_ALIGNMENT)
      itinPanelBox.add(b)
    }
    itinPanel.add(itinPanelBox)

    val itinControlPanel: JPanel = new JPanel(new BorderLayout())
    itinControlPanel.setBorder(BorderFactory.createTitledBorder("Itinéraire"))
    val itinControlBox: Box = Box.createHorizontalBox()
    for (b <- Seq(itinPanel)) {
      b.setAlignmentY(Component.TOP_ALIGNMENT)
      itinControlBox.add(b)
    }
    itinControlPanel.add(itinControlBox, BorderLayout.NORTH)

    val controlPanel: JPanel = new JPanel(new BorderLayout())
    val controlBox: Box = Box.createVerticalBox()
    for (b <- Seq(rienPanel, controlLignePanel, itinControlPanel)) {
      b.setAlignmentX(Component.LEFT_ALIGNMENT)
      b.setAlignmentY(Component.TOP_ALIGNMENT)
      controlBox.add(b)
    }
    controlPanel.add(controlBox, BorderLayout.NORTH)

    val zoomPanel: JPanel = new JPanel()
    val zoomIn: JButton = new JButton("Zoom +")
    zoomIn.addActionListener(new ActionListener() {
      def actionPerformed(ae: ActionEvent): Unit = {
        mapView.getModel.mapViewPosition.zoomIn()
      }
    })

    val zoomOut: JButton = new JButton("Zoom -")
    zoomOut.addActionListener(new ActionListener() {
      def actionPerformed(ae: ActionEvent): Unit = {
        mapView.getModel.mapViewPosition.zoomOut()
      }
    })
    val recentrer: JButton = new JButton("Centrer")
    recentrer.addActionListener(new ActionListener() {
      def actionPerformed(ae: ActionEvent): Unit = {
        mapView.getModel.mapViewPosition.setCenter(
          Util.point2latlong(Carte.center)
        )
        mapView.getModel.mapViewPosition.setZoomLevel(DEFAULT_ZOOM_LEVEL.toByte)
      }
    })
    for (b <- Seq(zoomOut, zoomIn, recentrer)) zoomPanel.add(b)

    val mapPanel: JPanel = new JPanel()
    mapPanel.setBorder(BorderFactory.createTitledBorder("Carte"))
    mapPanel.setLayout(new BoxLayout(mapPanel, BoxLayout.Y_AXIS))
    for (b <- Seq(mapView, zoomPanel)) mapPanel.add(b)

    val appPanel: JPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val appPanelBox: Box = Box.createHorizontalBox()
    for (b <- Seq(controlPanel, mapPanel)) {
      b.setAlignmentY(Component.TOP_ALIGNMENT)
      appPanelBox.add(b)
    }
    appPanel.add(appPanelBox)

    frame.add(appPanel)
    frame.pack()
    frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    frame.addWindowListener(new WindowAdapter() {
      override def windowClosing(e: WindowEvent): Unit = {
        mapView.getLayerManager
          .interrupt() // DD: mapView.destroyAll doesn't seem to handle this correctly
        mapView.destroyAll()
        AwtGraphicFactory.clearResourceMemoryCache()
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      }
    })
    frame.setVisible(true)
  }

  /** Affiche l'overlay sur la carte
    * @param overlay
    *   un overlay à afficher
    */
  def showOverlay(overlay: Overlay): Unit = {
    overlay match {
      case Nothing => clearAllButMap(mapView)
      case ShapeGroupLayer(_, sl, n) =>
        showOverlay(n)
        sl.foreach({ (s: starmap.model.overlay.Shape) => showShape(s) })
    }
  }

  /** Affiche sur la carte de l'application la forme s.
    * @param shape
    *   une forme à afficher
    */
  def showShape(shape: Shape): Unit = {
    mapView.addLayer(Util.shapeToLayer(shape))
  }

}