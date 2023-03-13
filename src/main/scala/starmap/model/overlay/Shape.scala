package starmap.model.overlay

import starmap.model.Geo
import java.awt.Color

/**
 * Différentes formes à tracer sur une carte pour dessiner des itinéraires.
 */
sealed trait Shape

/**
 * Chemin tracé en pointillés, de couleur donnée
 *
 * @param points
 *   liste des points à tracer le long du chemin
 * @param color
 *   couleur des points
 */
case class DottedPath(points: List[Geo], color: Color) extends Shape

/**
 * Chemin au tracé continu, de couleur donnée
 *
 * @param points
 *   liste des points par lesquels passe le chemin, dans l'ordre
 * @param color
 *   couleur du chemin
 */

case class Path(points: List[Geo], color: Color) extends Shape

/**
 * Cercle de rayon fixé, marquant un arrêt de bus.
 *
 * @param point
 *   le centre du cercle
 * @param color
 *   la couleur du cercle
 *
 * @note
 *   - Le rayon du cercle est défini dans Settings, mais vous n'avez pas à le
 *     modifier.
 *   - `Color` est une classe de la bibliothèque java.awt (cf. import en début
 *     de ce fichier).
 *   - Une couleur peut être définie par ses composantes RGB : Color(255, 200,
 *     0)
 *   - Certains noms sont aussi disponibles : Color.ORANGE
 *   - Cf. https://docs.oracle.com/javase/8/docs/api/java/awt/Color.html
 */
case class Circle(point: Geo, color: Color) extends Shape

/**
 * Marqueur du début du premier trajet en bus sur un itinéraire
 *
 * @param point
 *   position de l'arrêt de bus
 */
case class StartPin(point: Geo) extends Shape

/**
 * Marqueur de fin du dernier trajet en bus sur un itinéraire
 *
 * @param point
 *   position de l'arrêt de bus
 */
case class EndPin(point: Geo) extends Shape

/**
 * Marqueur d'un arrêt intermédiaire sur un itinéraire (arrêt de descente où de
 * montée lors d'une correspondance).
 *
 * @param point
 *   position de l'arrêt de bus
 */
case class Connect(point: Geo) extends Shape

/**
 * Marqueur du début de l'itinéraire souhaité par l'utilisateur (position de
 * départ définie en cliquant sur la carte).
 *
 * @param point
 *   position
 *
 * @note
 *   se marqueur est utilisé par la package starmap.lib. Vous n'avez pas à
 *   l'utiliser directement dans vos propres définitions.
 */
case class FromPin(point: Geo) extends Shape

/**
 * Marqueur du la fin de l'itinéraire souhaité par l'utilisateur (position
 * d'arrivée définie en cliquant sur la carte).
 *
 * @param point
 *   position
 *
 * @note
 *   se marqueur est utilisé par la package starmap.lib. Vous n'avez pas à
 *   l'utiliser directement dans vos propres définitions.
 */
case class ToPin(point: Geo) extends Shape