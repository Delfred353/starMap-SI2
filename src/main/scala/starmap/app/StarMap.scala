package starmap.app

import java.awt.Color
import starmap.lib.*
import starmap.lib.Carte.*
import starmap.model.*
import starmap.model.overlay.*
import ExosGeoFormes.*

/**
 * Point d'entrée : lancement de l'interface graphique affichant une carte de
 * Rennes et des boutons pour rechercher des itinéraires ou des lignes de bus.
 *
 * En plus des interactions avec les boutons, on peut :
 *
 *   - choisir le centre initial de la carte
 *   - afficher un point construit à la main, mais marqueur fixé.
 *   - afficher un chemin construit à la main.
 *   - en console, afficher tout ce qu'on veut.
 *
 * Les formes affichées par showShape sont uniquement là pour se familiariser
 * avec l'application, la modélisation des données et pour déboguer.
 *
 * Ces formes disparaissent à la première interaction avec les boutons de l'UI,
 * sauf les zooms.
 */

object StarMap extends App {

  /* Lancement de l'interface graphique */
  createMapView(ExosGeoFormes.mapCenter, "StarMap")

  /* Affichage de quelques formes construites « à la main ».
     Elles disparaissent à la première interaction avec l'UI (sauf les zooms).
   */
  showShape(Connect(ExosGeoFormes.mapCenter))
  showShape(StartPin(republique))
  showShape(EndPin(mairie))
  showShape(Circle(librairie, Color.BLUE))
  showShape(Circle(istic, Color.GREEN))
  showShape(ExosGeoFormes.chemin1)
  showShape(ExosGeoFormes.chemin2)
  showOverlay(ExosGeoFormes.parcoursLigne0)
}
