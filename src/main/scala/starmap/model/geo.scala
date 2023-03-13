package starmap.model

/**
 * Points de géo-localisation
 *
 * @param lat:
 *   latitude du point en degrés décimaux
 *
 * @param long:
 *   longitude: du point en degrés décimaux
 *
 * @note
 *   6 chiffres après la virgule donnent une position au mètre près (à la
 *   latitude de Rennes)
 */
case class Geo(lat: Double, long: Double)