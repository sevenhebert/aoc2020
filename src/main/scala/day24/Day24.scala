package day24

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Day24 {

  val filename = "day24/input.txt"

  @tailrec
  def parseLine(line: String, res: Seq[String]): Seq[String] =
    line.headOption match {
      case Some('n') => parseLine(line.tail.tail, res :+ line.take(2))
      case Some('e') => parseLine(line.tail, res :+ line.head.toString)
      case Some('s') => parseLine(line.tail.tail, res :+ line.take(2))
      case Some('w') => parseLine(line.tail, res :+ line.head.toString)
      case None      => res
    }

  val input: Seq[Seq[String]] = Try(
    io.Source
      .fromResource(filename)
      .getLines
      .map(parseLine(_, Nil))
      .toSeq) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  def getCoordinates(dirs: Seq[String]): (Int, Int) = {
    @tailrec
    def recurse(cur: (Int, Int), ls: Seq[String]): (Int, Int) =
      if (ls.isEmpty) cur
      else {
        val (lat, lon) = cur
        ls.head match {
          case "nw" => recurse((lat, lon - 1), ls.tail)
          case "ne" => recurse((lat + 1, lon - 1), ls.tail)
          case "sw" => recurse((lat - 1, lon + 1), ls.tail)
          case "se" => recurse((lat, lon + 1), ls.tail)
          case "w"  => recurse((lat - 1, lon), ls.tail)
          case "e"  => recurse((lat + 1, lon), ls.tail)
        }
      }

    recurse((0, 0), dirs)
  }

  def getTileMap(instructions: Seq[Seq[String]]): Map[(Int, Int), Boolean] = {
    @tailrec
    def recurse(instructions: Seq[Seq[String]], tileState: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] =
      if (instructions.isEmpty) tileState
      else {
        val coordinateKey = getCoordinates(instructions.head)
        tileState.get(coordinateKey) match {
          case Some(value) => recurse(instructions.tail, tileState.updated(coordinateKey, !value))
          case None => recurse(instructions.tail, tileState.updated(coordinateKey, true))
        }
      }
    recurse(instructions, Map.empty)
  }

  val tiles: Map[(Int, Int), Boolean] = getTileMap(input)

  val res1: Int = tiles.count(_._2 == true)

  def expandTiles(tileState: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] = tileState.map {
    case ((lat, lon), _) => Map(
      (lat, lon - 1) -> false,
      (lat + 1, lon - 1) -> false,
      (lat - 1, lon + 1) -> false,
      (lat, lon + 1) -> false,
      (lat - 1, lon) -> false,
      (lat + 1, lon) -> false
    )
  }.flatten.toMap ++ tileState

  def nextTileState(prevState: Map[(Int, Int), Boolean], cur: ((Int, Int), Boolean)): Boolean = {
    val ((lat, lon), isBlack) = cur
    val nw = prevState.get(lat, lon - 1)
    val ne = prevState.get(lat + 1, lon - 1)
    val sw = prevState.get(lat - 1, lon + 1)
    val se = prevState.get(lat, lon + 1)
    val w = prevState.get(lat - 1, lon)
    val e = prevState.get(lat + 1, lon)
    val total = List(nw, ne, sw, se, w, e).flatten
    val numBlack = total.count(_ == true)

    if (isBlack && numBlack == 0) false
    else if (isBlack && numBlack > 2) false
    else if (!isBlack && numBlack == 2) true
    else isBlack
  }

  def nextState(prevState: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] =
    expandTiles(prevState).foldRight(prevState) {
      case (((lat, lon), state), acc) =>
        acc.updated((lat, lon), nextTileState(prevState, ((lat, lon), state)))
    }

  lazy val res2: Int = List.from(1 to 100).foldLeft[Map[(Int, Int), Boolean]](tiles)((acc, _) =>
    nextState(acc)).count(_._2 == true)

}
