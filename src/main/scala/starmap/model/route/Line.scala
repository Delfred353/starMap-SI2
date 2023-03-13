package starmap.model.route

import starmap.model.overlay.Path

/**
 * Ligne de bus
 *
 * @param id
 *   identifiant unique de la ligne, tel que "0010-A-1175-1390"
 * @param name
 *   par exemple "10"
 * @param libelle
 *   par exemple "Rennes (Beaulieu Chimie) -> Rennes (Porte de Cleunay)"
 * @param startStopId
 *   code identifiant l'arrêt de départ de la ligne, par exemple 1175
 * @param endStopId
 *   code identifiant l'arrêt d'arrivée de la ligne, par exemple 1390
 * @param parcours
 *   liste des arrêts le long de cette ligne, par exemple
 *   Path(List(Geo(48.11706,-1.634049), Geo(48.11779,-1.634489), ...,
 *   Geo(48.10409,-1.712976), Geo(48.10409,-1.713395)),
 *   java.awt.Color(0xef859d)))
 * @param stops
 *   liste des arrêts desservis, dans l'ordre du parcours
 *
 * @note
 *    - `id` est un identifiant unique : deux lignes distinctes n'ont pas le même `id`.
 *    - En général pour un nom de ligne donné, tel que "10", il existe plusieurs lignes portant ce nom :
 *      - une ligne dans une sens
 *      - une ligne dans l'autre sens
 *      - parfois des lignes ayant des parcours légèrement différents
 *    - les parcours de certaines lignes contiennent des boucles
 *
 */
case class Line(
    id: String,
    name: String,
    libelle: String,
    startStopId: Int,
    endStopId: Int,
    parcours: Path,
    stops: List[Stop]
)