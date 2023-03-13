package starmap.lib

import net.minidev.json.JSONArray
import net.minidev.json.JSONObject
import net.minidev.json.JSONValue
import starmap.model.Geo
import starmap.model.overlay.Path
import starmap.model.route.Line
import starmap.model.route.Stop
import java.awt.Color
import scala.io.Source


object CSV extends CSVreader {

  private[lib] case class IllFormedCSV(m: String) extends Exception

  private val dessertes_file = "csv/tco-bus-topologie-dessertes-td.csv"
  private lazy val (stopsLines, linesStops) = parseDessertes(dessertes_file)

  private def toGeo(s: String): Geo = {
    s.split(",").toList match {
      case lat :: long :: Nil => Geo(lat.toDouble, long.toDouble)
      case _                  => throw IllFormedCSV("Ill-formed CSV File")
    }
  }

  private def toGeos(s: String): List[Geo] = {
    var json = s.replaceAll("\"\"", "\"")
    json = json.substring(1, json.length() - 1)
    val map = JSONValue.parse(json).asInstanceOf[JSONObject]
    val edges = map.get("coordinates").asInstanceOf[JSONArray].toArray().toList
    val edgesArray = edges.map { a => a.asInstanceOf[JSONArray] }
    edgesArray.map { a =>
      Geo(a.get(1).asInstanceOf[Double], a.get(0).asInstanceOf[Double])
    }
  }


  private val SEP = ";"
  private def buildLignes(
      filename: String,
      stops: Map[Int, Stop]
  ): List[Line] = {
    val source =  Source.fromFile(filename)
    val csvRows = source.getLines().drop(1).toVector
    val busLines = for {
      line <- csvRows
      values = line.split(SEP)
      l_id = values(0)
      if values(6) == "Principal"
      if linesStops.contains(
        l_id
      ) // e.g. there is no line 0064-B-1556-4019, but stops are declared for this line
    } yield Line(
      l_id,
      values(3),
      values(7),
      //toSensCommercial(values(5)),
      values(8).toInt,
      values(10).toInt,
      Path(
        toGeos(values(13)),
        Color(Integer.parseInt(values(15).substring(1), 16))
      ),
      linesStops(l_id).toList.sortBy(_._1).map((_, v) =>  stops(v))
    )
    source.close()
    busLines.toList
  }

  private def parseDessertes(
      desserte: String
  ): (Map[Int, List[String]], Map[String, Map[Int, Int]]) = {
    val source = Source.fromFile(desserte)
    val lines = source.getLines().drop(1).toVector
    var map0 = Map[Int, List[String]]() // Stop_ID => List[Line_ID]
    var map1 = Map[String, Map[Int, Int]]() // Line_ID => Order => Stop_ID
    for (line <- lines; values = line.split(SEP)) {
      val l_id: String = values(0)
      val a_id: Int = values(4).toInt
      val ordre: Int = values(6).toInt
      val l_previous: List[String] = map0.getOrElse(a_id, List[String]())
      map0 = map0 + (a_id -> (l_id :: l_previous))
      val m_previous = map1.getOrElse(l_id, Map[Int, Int]())
      if m_previous.contains(ordre) then
        println(
          s"ERROR : duplicated order $ordre for $l_id, $a_id : $m_previous"
        )
      map1 = map1 + (l_id -> m_previous.updated(ordre, a_id))
    }
    source.close()
    (
      map0,
      map1.filter((_, m) => {
        val os = m.keys.toList.sorted; os == (1 to os.length).toList
      })
    ) // a few lines have missing stop 1
  }

  private def buildStops(): Map[Int, Stop] = {
    val busStops = "csv/topologie-des-points-darret-de-bus-du-reseau-star.csv"
    val source = Source.fromFile(busStops)
    val lines = source.getLines().drop(1)

    val stops = for {
      line <- lines
      values = line.split(SEP)
      stop_id = values(0).toInt
      stop = Stop(
        stop_id,
        values(2),
        toGeo(values(5)),
        stopsLines.getOrElse(values(0).toInt, List[String]())
      )
    } yield stop_id -> stop
    val res = stops.toMap
    source.close()
    res
  }

  override def buildLinesAndStops(): (List[Line], List[Stop]) = {
    val stops = buildStops()
    val ls =
      buildLignes("csv/parcours-des-lignes-de-bus-du-reseau-star.csv", stops)
    (ls, sanitize(stops.values.toList, ls))
  }

  // Vérifier que tous les arrêts appartiennent à une ligne donnée
  // ce n'est pas le cas avec 0803-B-1548-1615 qui est cherchée mais non trouvée.
  // 0056-B-4201-1242, 0054-B-4652-1242, 0150-A-3301-1167
  /**
   * @param stops
   *   les arrêts considérés
   * @param lines
   *   les lignes de bus considérées
   * @return
   *   les arrêts de bus dans stops qui appartiennent à au moins une ligne de
   *   lines
   */
  private def sanitize(stops: List[Stop], lines: List[Line]): List[Stop] = {
    stops.map({ case Stop(id, name, coord, stoplines) =>
      Stop(
        id,
        name,
        coord,
        stoplines.filter(line => lines.exists(l => l.id == line))
      )
    })
  }.filter(s => s.linesId.nonEmpty)

}