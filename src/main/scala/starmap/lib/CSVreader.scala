package starmap.lib

import starmap.model.route.{Line, Stop}

private[lib] trait CSVreader {

  /**
   * Lit les fichiers CSV suivants
   *   - csv/topologie-des-points-darret-de-bus-du-reseau-star.csv
   *   - csv/tco-bus-topologie-dessertes-td.csv
   *   - csv/parcours-des-lignes-de-bus-du-reseau-star.csv
   * @return
   *   les lignes et arrêts de bus décrites dans les fichiers lus
   */
  def buildLinesAndStops(): (List[Line], List[Stop])
}