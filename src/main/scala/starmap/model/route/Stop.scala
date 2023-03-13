package starmap.model.route

import starmap.model.Geo

/**
 * Arrêts de bus (« stop » en anglais)
 * @param id
 *   identifiant unique de l'arrêt, par exemple 2453
 * @param name
 *   nom de l'arrêt, par exemple "Chaperonnais"
 * @param coord
 *   coordonnées géographiques de l'arrêt, par exemple Geo(48.175463,-1.662268)
 * @param linesId
 *   lignes desservant cet arrêt, par exemple List("0078-B-1494-2461",
 *   "0178-B-1494-2461")
 *
 * @note
 *   Des arrêts physiques différents, mêmes proches et de même nom, ont des identifiants différents.
 *   En général, un arrêt sur une ligne « aller » n'est pas le même que l'arrêt du même nom sur la ligne « retour ».
 */
case class Stop(id: Int, name: String, coord: Geo, linesId: List[String])