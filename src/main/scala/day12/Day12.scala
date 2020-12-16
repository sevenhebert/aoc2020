package day12

import scala.util.{Failure, Success, Try}

object Day12 {

  val filename = "day12/input.txt"

  val input = Try(
    io.Source.fromResource(filename).getLines.toArray) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  sealed trait Move
  case class Lat(distance: Long) extends Move
  case class Lon(distance: Long) extends Move
  case class Turn(degrees: Int) extends Move
  case class Forward(n: Long) extends Move

  private val northPattern = "([N].*)".r
  private val southPattern = "([S].*)".r
  private val eastPattern = "([E].*)".r
  private val westPattern = "([W].*)".r
  private val leftPattern = "([L].*)".r
  private val rightPattern = "([R].*)".r
  private val forwardPattern = "([F].*)".r

  val moves = input.map({
    case northPattern(n) => Lat(n.drop(1).toLong)
    case southPattern(s) => Lat(-1L * s.drop(1).toLong)
    case eastPattern(e) => Lon(e.drop(1).toLong)
    case westPattern(w) => Lon(-1L * w.drop(1).toLong)
    case rightPattern(r) => Turn(r.drop(1).toInt)
    case leftPattern(l) => Turn(-1 * l.drop(1).toInt)
    case forwardPattern(f) => Forward(f.drop(1).toLong)
  })

  def updateDegrees(total: Int): Int =
    if (total >= 360) total - 360
    else if (total < 0) total + 360
    else total

  def solve1(i: Int)(deg: Int)(lat: Long, lon: Long): Long =
    if (i == moves.length) lat.abs + lon.abs
    else moves(i) match {
      case Lat(distance) => solve1(i + 1)(deg)(lat + distance, lon)
      case Lon(distance) => solve1(i + 1)(deg)(lat, lon + distance)
      case Turn(add) => solve1(i + 1)(updateDegrees(deg + add))(lat, lon)
      case Forward(add) => (solve1(i + 1)(deg)_).tupled(deg match {
        case 0 => (lat, lon + add)
        case 90 => (lat - add, lon)
        case 180 => (lat, lon - add)
        case 270 => (lat + add, lon)
      })
    }

  val res1 = solve1(0)(0)(0, 0) // 1457

  def updateWaypoint(deg: Int, lat: Long, lon: Long): (Long, Long) =
    if (deg == 90 || deg == -270) (-1 * lon, lat)
    else if (deg == 180 || deg == -180) (-1 * lat, -1 * lon)
    else if (deg == 270 || deg == -90) (lon, -1 * lat)
    else (lat, lon)

  def solve2(i: Int)(shipLat: Long, shipLon: Long)(wayLat: Long, wayLon: Long): Long =
    if (i == moves.length) shipLat.abs + shipLon.abs
    else moves(i) match {
      case Lat(distance) => solve2(i + 1)(shipLat, shipLon)(wayLat + distance, wayLon)
      case Lon(distance) => solve2(i + 1)(shipLat, shipLon)(wayLat, wayLon + distance)
      case Turn(degrees) => (solve2(i + 1)(shipLat, shipLon)_).tupled(updateWaypoint(degrees, wayLat, wayLon))
      case Forward(factor) => solve2(i + 1)(shipLat + (factor * wayLat), shipLon + (factor * wayLon))(wayLat, wayLon)
    }

  val res2 = solve2(0)(0, 0)(1, 10) // 106860

}

