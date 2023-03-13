package starmap.model.ui

/**
 * Type décrivant les actions possibles associées à un point de la carte sur
 * lequel l'utilisateur vient de cliquer.
 *   - Cancel : annuler le clic, ne rien faire
 *   - StartDef : définir le point cliqué comme le départ de l'itinéraire
 *     recherché
 *   - EndDef : définir le point cliqué comme le point d'arrivée de l'itinéraire recherché
 */
sealed trait Click
case object Cancel extends Click
case object StartDef extends Click
case object EndDef extends Click