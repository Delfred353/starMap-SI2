package starmap.model.route

import starmap.model.route.Stop

/**
 * Itinéraire (suite d'arrêts de bus)
 *
 * Un itinéraire est défini récursivement : un itinéraire est un arrêt,
 * éventuellement suivi d'un itinéraire.
 *
 * Dans les cas récursifs, on indique comment ce rendre de l'arrêt indiqué à
 * l'arrêt suivant (en bus ou à pied) :
 *
 *   - par l'identifiant d'une ligne de bus pour un trajet en bus, ou
 *   - sans rien pour un trajet à pied
 *
 */
sealed trait Route

/**
 * Itinéraire constitué d'un seul arrêt, utilisé pour définir la fin d'un
 * itinéraire.
 *
 * @param end
 *   l'arrêt auquel se termine l'itinéraire
 */

case class End(end: Stop) extends Route

/**
 * Itinéraire se terminant par un trajet sur une ligne de bus donnée.
 *
 * @param start
 *   arrêt de bus du début de l'itinéraire
 * @param lineId
 *   ligne de bus à emprunter à l'arrêt `start`
 * @param route
 *   suite de l'itinéraire
 */
case class Bus(start: Stop, lineId: String, route: Route) extends Route

/**
 * Itinéraire commençant par un trajet à pied
 *
 * @param start
 *   arrêt de bus depuis lequel il faut marcher
 * @param route
 *   suite de l'itinéraire
 */
case class Walk(start: Stop, route: Route) extends Route