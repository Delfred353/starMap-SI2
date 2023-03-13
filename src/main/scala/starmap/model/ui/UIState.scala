package starmap.model.ui

import starmap.lib.*
import starmap.model.*
import starmap.model.overlay.*

/** Type décrivant un état de l'interface graphique.
  *
  * @param overlay
  *   un overlay (ce qui est affiché sur la carte)
  * @param fromTo
  *   les points de départ et d'arrivée d'un itinéraire recherché. Il sont
  *   définis quand ils sont différents de None. Sinon, ils restent à définir
  *   par l'utilisateur.
  * @param message
  *   message affiché en regard du bouton « rechercher ». Ce message informe par
  *   exemple l'utilisateur des actions à effectuer pour définir et rechercher
  *   un itinéraire, ou bien décrit un itinéraire déjà trouvé.
  *
  * @note
  *   Le message peut contenir des balises html. Par exemple:
  *
  * """<html><p style="color:blue;">blah blah blah</p><html>"""
  *
  * @note
  *   Remarquez les triples guillemets permettant de définir une chaîne de
  *   caractères contenant elle-même des guillemets. Vous trouverez peut-être
  *   utile de lire :
  *   https://docs.scala-lang.org/overviews/core/string-interpolation.html
  */
case class UIState(
                    overlay: Overlay,
                    fromTo: (Option[Geo], Option[Geo]),
                    message: String
)