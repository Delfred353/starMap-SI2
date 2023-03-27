/* !!! ATTENTION. Remplissez correctement les champs ci-dessous.
 * !!!           Une pénalité de deux points par champ non reconnu automatiquement sera appliquée.
 * !!!           Si un candidat n'est pas déclaré, sa note sera 0.
 * !!!           Utilisez la liste ci-après pour remplir les champs CH13 et CH23.
 * ================================================================================================ *
 * CANDIDAT(E) 1
 *   CH11 Adresse de courriel universitaire de Rennes 1 : nicolas.merrer@etudiant.univ-rennes1.fr
 *   CH12 Numéro d'étudiant : 22102431
 *   CH13 Groupe et encadrant de TP habituel : IE3 Catherine Belleannée
 * ================================================================================================ *
 * CANDIDAT(E) 2
 *   CH21 Adresse de courriel universitaire de Rennes 1 : amelie.tournier@etudiant.univ-rennes1.fr
 *   CH22 Numéro d'étudiant : 22106854
 *   CH23 Groupe et encadrant de TP habituel : IE3 Catherine Belleannée
 * ================================================================================================ *
 * Merci de copier-coller la ligne adéquate pour remplir les champs CH13 et CH23 ci-dessus
    IE1 Thomas Anberrée
    IE1 Adrien Le Roch
    IE2 Thomas Anberrée
    IE2 Nicolas Bailluet
    IE3 Catherine Belleannée
    IE3 Gilles Lesventes
    IE4 Léo Cosseron
    IE4 Raoul Vorc’h
    IE5 Dimitri Lajou
    IE5 Théo Losekoot
    MA1 Mathieu Chambe
    MA1 Pierre Lermusiaux
    MA2 Adrien Le Roch
    MA2 Gilles Lesventes
 */
package starmap.app

import org.mapsforge.core.util.LatLongUtils

import java.awt.Color
import starmap.lib.Carte.showOverlay

import starmap.lib.*
import starmap.model.*
import starmap.model.overlay.*
import starmap.model.route.*
import starmap.model.ui.*
import starmap.app.ExosLignes.overlayLignes
import starmap.app.ExosOverlays.concatOverlay
import starmap.app.ExosArrets.overlayStops

/** Lignes et arrêts de bus, obtenus à partir des fichiers csv fournis. */
val (allLines, allStops): (List[Line], List[Stop]) = CSV.buildLinesAndStops()

private val MSG_SELECT_0: String =
  "Sélectionnez des points de départ et d'arrivée."
private val MSG_SELECT_1: String =
  "Sélectionnez un point d'arrivée."
private val MSG_SELECT_2: String =
  "Sélectionnez un point de départ"
private val MSG_SELECT_3: String =
  "Cliquez sur « rechercher » pour afficher l'itinéraire."

object ExosGeoFormes {

  /** Point marquant le centre de Rennes */
  val mapCenter: Geo = Geo(48.11078, -1.67525)

  val mairie: Geo = Geo(48.111334, -1.680100)
  val republique: Geo = Geo(48.110987, -1.679253)
  val istic: Geo = Geo(48.11541, -1.6387)
  val librairie: Geo = Geo(48.11039, -1.678)

  val chemin1: DottedPath = DottedPath(
    List(
      Geo(48.112424, -1.675196),
      Geo(48.112438, -1.673522),
      Geo(48.112180, -1.671183),
      Geo(48.112309, -1.669424),
      Geo(48.112438, -1.668179),
      Geo(48.113068, -1.668136)
    ),
    Color.ORANGE
  )

  val chemin2: Path = Path(
    List(
      Geo(48.112413, -1.675255),
      Geo(48.110235, -1.674311)
    ),
    Color(0xef, 0x85, 0x9d) // Le prefix 0x indique un nombre en base 16.
  )

  /**
   * Un overlay constitué d'une unique couche comportant une seule forme : le
   * parcours de la première ligne déclarée dans le fichier de donnée.
   *
   * @note
   *   votre définition doit être une expression unique, tenant sur une ligne,
   *   sans création de valeur intermédiaire.
   */

  val parcoursLigne0: Overlay =
    ShapeGroupLayer(true, List(allLines.head.parcours), Nothing)

}

object ExosLignes {

  /**
   * @param lines
   *   une liste de lignes de bus
   * @return
   *   la liste des informations essentielles pour chaque élément de lines, dans
   *   le même ordre :
   *   - son identifiant de ligne
   *   - son nom court
   *   - son libellé (nom long)
   *
   * @note
   *   - fonction utilisée dans l'interface graphique pour afficher le menu
   *     déroulant
   *   - utiliser pattern-matching et récursion
   *   - indication de longueur : environ 5 lignes
   */
  def nomsDesLignes(
      lines: List[Line]
  ): List[(String, String, String)] = {
    lines match {
      case Nil => Nil
      case head :: rest =>
        (head.id, head.name, head.libelle) :: nomsDesLignes(rest)

    }
  }

  /**
   * @param id
   *   identifiant de ligne
   * @param lines
   *   une liste de lignes de bus
   * @return
   *   l'élément de lines (s'il existe) qui a pour identifiant id
   * @note
   *   - utiliser pattern matching et récursion
   *   - indication de longueur : environ 5 lignes
   */

  def trouverLigneId(
      id: String,
      lines: List[Line]
  ): Option[Line] = {
    lines match {
      case Nil => None
      case head :: next =>
        if (head.id.equals(id)) {
          Option(head)
        } else {
          trouverLigneId(id, next)
        }
    }
  }

  /**
   * @param line
   *   une ligne de bus
   * @return
   *   un overlay pour visualiser line sur la carte
   *
   * @note
   *   indication de longueur : 1 ligne
   */
  def overlayLigne(line: Line): Overlay = {
    ShapeGroupLayer(true, List(line.parcours), Nothing)
  }

  /**
   * @param lines
   *   une liste de lignes de bus
   * @return
   *   un overlay pour visualiser toutes les lignes de lines sur la carte,
   *   constitué d'une couche par ligne à visualiser
   * @note
   *   - utiliser pattern matching et récursion
   *   - indication de longueur : environ 5 lignes
   */
  def overlayLignes(lines: List[Line]): Overlay = {
    lines match {
      case Nil => Nothing
      case head :: next =>
        ShapeGroupLayer(true, List(head.parcours), overlayLignes(next))
    }
  }

}

/**
 * ActionsBoutons regroupe les fonctionnalités associées aux composants de
 * l'interface graphique. Ces fonctionnalités font évoluer l'état de l'interface
 * graphique.
 */
object ExosActionsBoutons {

  /**
   * Un doublet correspondant à des points de départ et d'arrivée non définis
   */
  private val undefinedFromTo: (Option[Geo], Option[Geo]) = (None, None)

  /**
   * État de l'interface graphique tel que :
   *   - rien n'est affiché sur la carte
   *   - la recherche d'itinéraire est réinitialisée: les points de départ et
   *     d'arrivée de l'itinéraire à rechercher ne sont pas définis.
   */
  private val initialState: UIState =
    UIState(Nothing, undefinedFromTo, message(undefinedFromTo))

  /**
   * @param state
   *   l'état courant de l'interface graphique
   * @return
   *   l'état de l'interface graphique après avoir cliqué sur le bouton «
   *   Recommencer », c'est-à-dire l'état `initialState`.
   *
   * @note
   *   - ici, l'état renvoyé ne dépend pas de l'état `state` reçu en paramètre.
   *     La fonction `cliquerRecommencer` est donc une fonction constante (elle
   *     renvoie toujours la même valeur).
   */
  def cliquerRecommencer(state: UIState): UIState = initialState

  /**
   * @param state
   *   l'état courant de l'interface graphique
   * @param id
   *   un identifiant de ligne de bus
   * @return
   *   l'état de l'interface graphique après avoir cliqué sur le bouton « Ma
   *   ligne », avec `id` comme ligne de bus sélectionnée dans le menu
   *   déroulant.
   *   - Le nouvel overlay contient deux couches :
   *     - une couche pour le parcours de la ligne
   *     - une couche pour tous les arrêts le long du parcours
   *   - L'itinéraire à rechercher est réinitialisé.
   *
   * @note
   *   - indication de longueur : environ 10 lignes
   *   - suggestion : utiliser les fonctions ExosOverlays.concatOverlay,
   *     ExosLignes.overlayLigne, ExosArrets.overlayStops
   */
  def cliquerMaLigne(state: UIState, id: String): UIState = {

    val ligne: Option[Line] = ExosLignes.trouverLigneId(id, allLines)
    ligne match {
      case None => state
      case Some(l): Option[Line] =>
        val newOverlay = ExosOverlays.concatOverlay(
          ExosLignes.overlayLigne(l),
          ExosArrets.overlayStops(l.stops)
        )
        UIState(
          newOverlay,
          (Some(l.stops.head.coord), Some(l.stops.last.coord)),
          """<html><p style="color:blue;"> Ligne selectionnée affichée </p><html>"""
        )
    }
  }

  /**
   * @param state
   *   l'état courant de l'interface graphique (quelconque)
   * @return
   *   l'état de l'interface graphique après qu'on ait cliqué sur le bouton «
   *   Toutes les lignes » dans l'état `state`. La recherche d'itinéraire est
   *   réinitialisée. L'overlay est remplacé par un overlay qui affiche toutes
   *   les lignes et tous leurs arrêts.
   * @note
   *   - indication de longueur : environ 10 lignes
   *   - utiliser ExosOverlays.concatOverlay, ExosArrets.overlayStops,
   *     ExosLignes.overlayLignes
   */
  def cliquerToutesLesLignes(state: UIState): UIState = {
    val overlayTousLignesStops: Overlay =
      concatOverlay(overlayLignes(allLines), overlayStops(allStops))
    UIState(
      overlayTousLignesStops,
      (None, None),
      """<html><p style="color:blue;"> Toutes les lignes affichées </p><html>"""
    )
  }

  /**
   * @param fromTo
   *   les points (optionnels) de départ et d'arrivée d'un itinéraire cherché
   *
   * @return
   *   selon fromTo, un message indiquant les actions restant à effectuer afin
   *   de lancer la rechercher d'un itinéraire, parmi les messages MSG_SELECT_X.
   *
   * @note
   *   - indication de longueur : moins de 10 lignes
   *   - les chaines MSG_SELECT_0 à MSG_SELECT_3 sont définies au début de ce
   *     fichier.
   */
  def message(fromTo: (Option[Geo], Option[Geo])): String = {
    fromTo match {
      case (None, None) => MSG_SELECT_0
      case (_, None)    => MSG_SELECT_1
      case (None, _)    => MSG_SELECT_2
      case (_, _)       => MSG_SELECT_3
    }
  }

  /**
   * @param state
   *   l'état courant de l'interface graphique
   * @param p
   *   un point de géo-localisation cliqué sur la carte
   * @param c
   *   le bouton cliqué (Départ, Arrivée, Annuler) dans la popup indiquant le
   *   type de marqueur que l'on souhaite placer au point `p`
   * @return
   *   l'état de l'interface graphique après avoir cliqué sur `p` dans la carte:
   *   - l'overlay n'est pas modifié
   *   - les marqueurs fromTo sont ceux de `state` modifiés selon `c`
   *   - le message est modifié en conséquence
   *
   * @note
   *   - indication de longueur : moins de 10 lignes
   */
  def cliquerCarte(state: UIState, p: Geo, c: Click): UIState = {
    c match {
      case Cancel => state
      case StartDef =>
        val (a, b): (Option[Geo], Option[Geo]) = state.fromTo
        UIState(state.overlay, (Some(p), b), message(Some(p), b))
      case EndDef =>
        val (a, b): (Option[Geo], Option[Geo]) = state.fromTo
        UIState(state.overlay, (a, Some(p)), message(a, Some(p)))
    }
  }

  /**
   * @param state
   *   l'état courant de l'interface graphique (quelconque)
   * @return
   *   l'état de l'interface graphique après qu'on ait cliqué sur le bouton «
   *   Rechercher ». Quand la recherche d'itinéraire peut être effectuée, et
   *   qu'un itinéraire est alors trouvé :
   *   - l'itinéraire est dessiné sur la carte. Les autres couches représentant
   *     des itinéraires sont supprimées.
   *   - la description textuelle de l'itinéraire est aussi indiquée à
   *     l'utilisateur
   *   - les points de départ et d'arrivée ne sont PAS réinitialisés
   *
   * @note
   *   - indication de longueur : environ 15 lignes
   *   - utiliser ExosRoutes.searchItin, ExosRoutes.overlayRoute,
   *     ExosRoutes.optionRouteInstructions, ExosOverlays.clearAllItineraries
   */
  def cliquerRechercher(state: UIState): UIState = initialState // TODO

}

object ExosOverlays {

  /**
   * @param above
   *   un overlay quelconque
   * @param under
   *   un overlay quelconque
   * @return
   *   l'overlay correspondant à la concaténation de over et under. L'overlay
   *   under doit être placé « sous » over.
   *
   * @note
   *   - indication de longueur : environ 5 lignes
   *   - utiliser le pattern matching et la récursion
   */
  def concatOverlay(
      above: Overlay,
      under: Overlay
  ): Overlay = {
    above match {
      case Nothing => under
      case ShapeGroupLayer(bool: Boolean, chemin: List[Shape], Nothing) =>
        ShapeGroupLayer(bool, chemin, under)
      case ShapeGroupLayer(bool: Boolean, chemin: List[Shape], o: Overlay) =>
        ShapeGroupLayer(bool, chemin, concatOverlay(o, under))
    }
  }

  /**
   * @param overlay
   *   un overlay quelconque
   * @return
   *   l'overlay identique à overlay, mais sans aucun itinéraire visualisé
   * @note
   *   - indication de longueur : environ 5 lignes
   *   - utiliser le pattern matching et la récursion
   */

  def clearAllItineraries(overlay: Overlay): Overlay = {
    overlay match {
      case Nothing => Nothing
      case ShapeGroupLayer(_, shape, o) =>
        ShapeGroupLayer(false, shape, clearAllItineraries(o))
    }
  }
}

object ExosArrets {

  /**
   * @param stops
   *   une liste d'arrêts de bus
   * @return
   *   un overlay pour visualiser les arrêts de bus figurant dans la liste stops
   * @note
   *   - indication de longueur : environ 10 lignes
   *   - utiliser peut-être une fonction auxiliaire, le pattern matching et la
   *     récursion
   */
  def overlayStops(stops: List[Stop]): Overlay = {
    ShapeGroupLayer(
      true,
      stopsToShapes(stops),
      Nothing
    )

  }

  def stopsToShapes(stops: List[Stop]): List[Shape] = {
    stops match
      case Nil => Nil
      case head :: next =>
        Circle(head.coord, Color.BLUE) :: stopsToShapes(next)

  }
}

object ExosRoutes {

  /**
   * @param route
   *   un itinéraire quelconque
   * @return
   *   un overlay pour visualiser l'itinéraire `route` sur la carte.
   * @note
   *   C'est une fonction difficile à implémenter. Procédez par étapes en
   *   commençant par des itinéraires simples.
   */
  def overlayRoute(route: Route): Overlay = {
    route match {
      case End(end) => ShapeGroupLayer(true, List(EndPin(end.coord)), Nothing)
      case Walk(start, End(end)) =>
        ShapeGroupLayer(
          true,
          List(DottedPath(List(start.coord, end.coord), Color.RED)),
          Nothing
        )
      case Bus(start, lineId, End(end)) =>
        ShapeGroupLayer(
          true,
          List(Path(List(start.coord, end.coord), Color.RED)),
          Nothing
        )
      case Bus(start, lineId, Walk(startW,suite)) => ShapeGroupLayer(
          true,
          List(Path(List(start.coord, startW.coord), Color.RED)),
          overlayRoute(suite)
        )
      case Walk(start,Bus(startB,id,suite)) => ShapeGroupLayer(
          true,
          List(DottedPath(List(start.coord, startB.coord), Color.RED)),
          overlayRoute(suite)
        )
        case Bus(start, lineId, Bus(startB,suite)) => ShapeGroupLayer(
          true,
          List(Path(List(start.coord, startB.coord), Color.RED)),
          overlayRoute(suite)
        )
      case Walk(start, Walk(startW,id,suite)) => ShapeGroupLayer(
          true,
          List(DottedPath(List(start.coord, startW.coord), Color.RED)),
          overlayRoute(suite) 
    }
  } // TODO //encore à voir si ça marche

  /**
   * @param optRoute
   *   un itinéraire optionnel
   * @return
   *   une liste d'indications textuelles décrivant l'itinéraire optRoute dans
   *   l'interface graphique
   */
  def optionRouteInstructions(optRoute: Option[Route]): List[String] = {
    optRoute match {
      case None => Nil
      case Some(End(end)) =>  """<html><p style="color:blue;"> Fin de l'itinéraire à  </p><html>""" + end :: Nil
      case Some(Bus(start, id, suite)) => """<html><p style="color:blue;"> Monter dans le bus à  </p><html>""" + start :: optionRouteInstructions(Some(suite))
      case Some(Walk(start, suite)) => """<html><p style="color:blue;"> Marcher à partir de  </p><html>""" + start :: optionRouteInstructions(Some(suite))
    }
    } // TODO + a voir si ça marche

  /**
   * @param start
   *   une position quelconque
   * @param end
   *   une position quelconque
   * @return
   *   un trajet permettant de se rendre de `start` à `end` en utilisant les bus
   *   du réseau STAR.
   *
   * @note
   *   Bien lire les conseils donnés dans l'énoncé accompagnant ce projet.
   */
  def searchItin(start: Geo, end: Geo): Option[Route] = ??? // TODO
}
