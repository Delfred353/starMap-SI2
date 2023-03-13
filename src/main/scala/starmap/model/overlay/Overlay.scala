package starmap.model.overlay

/**
 * Pile de couches superposables (Overlay).
 *
 * Chaque couche peut être pensée comme une feuille de plastique transparent
 * placée par-dessus un fond de carte, sur lequel on peut dessiner un
 * itinéraire, des repères, etc.
 *
 * Plusieurs couches peuvent être empilées, auquel cas les parties opaques des
 * couches supérieures obstruent les couches inférieures.
 *
 * Si aucune couche n'est présente, l'overlay est vide.
 *
 * Le fond de carte, complètement opaque, ne fait jamais partie de la pile.
 */
sealed trait Overlay

/** Pile vide */
case object Nothing extends Overlay

/**
 * Pile dont la couche supérieure contient un groupement de formes (ShapeGroup)
 *
 * @param isItinerary
 *   indique si `shapes` représente un itinéraire (true), ou autre chose (false)
 * @param shapes
 *   liste des formes formant le groupe
 * @param next
 *   reste de la pile (couches inférieures)
 */
case class ShapeGroupLayer(isItinerary: Boolean, shapes: List[Shape], next: Overlay)
    extends Overlay