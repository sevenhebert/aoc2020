package day17

import scala.util.{Failure, Success, Try}

object Day17v2 {

  private val filename = "day17/input.txt"

  private val input = Try(
    io.Source
      .fromResource(filename)
      .getLines
      .zipWithIndex
      .flatMap { case (row, rowIdx) => row.toCharArray.zipWithIndex.map {
        case (char, colIdx) => s"x:$colIdx,y:$rowIdx,z:0,w:0" -> char
      } }.toMap
  ) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  private val active = '#'
  private val inactive = '.'

  private def cubeCoordinates(cubeKey: String): Map[Char, Int] =
    cubeKey.split(',').map(str => (str.head, str.drop(2).toInt)).toMap

  private def getNeighbors(cubeKey: String, state: Map[String, Char]): Map[String, Char] = {
    val coordinates = cubeCoordinates(cubeKey)
//    println("coordinates " + coordinates)
    val xIdx = coordinates('x')
    val yIdx = coordinates('y')
    val zIdx = coordinates('z')
    val wIdx = coordinates('w')

    val initNeighbors = for {
      x <- List(xIdx - 1, xIdx, xIdx + 1)
      y <- List(yIdx - 1, yIdx, yIdx + 1)
      z <- List(zIdx - 1, zIdx, zIdx + 1)
      w <- List(wIdx - 1, wIdx, wIdx + 1)
    } yield s"x:$x,y:$y,z:$z,w:$w"

    val x = initNeighbors.map(neighborKey => state.get(neighborKey) match {
      case Some(neighborValue) => neighborKey -> neighborValue
      case None => neighborKey -> inactive
    }).toMap.removed(cubeKey)
    println("active " + x.count(_._2 == active))
    x
  }

  private def nextCubeState(cur: Char)(count: Int) = {
//    println("cur " + cur)
    if (cur == active && (count == 2 || count == 3)) active
    else if (cur == inactive && count == 3) active
    else inactive
  }

  def nextState(state: Map[String, Char]): Map[String, Char] = {
    state.foldRight(state) { case ((cubeKey, cubeState), acc) =>
      val neighbors = getNeighbors(cubeKey, state)
//      println("neighbors " + neighbors.size)
      val nextSelfState = nextCubeState(cubeState)(neighbors.count(_._2 == active))
      val update = neighbors.updated(cubeKey, nextSelfState)
//      println("update: " + update.size)
//      println("cubeKey: " + cubeKey)
      acc ++ update
    }
  }

//  input.toList.foreach(println)

  lazy val res2 = List.from(1 to 10).foldRight[Map[String, Char]](input)((_, acc) => {
    println(acc.count(_._2 == active))
    println("size: " + acc.size)
//    val coords = acc.keys.flatMap(_.split(',').map(str => (str.head.toString, str.drop(2).toInt)))
//    val xs = coords.filter(_._1 == "x")
//    println("xs: " + xs)
//    println("xMin: " + xs.minBy(_._2))
//    println("xMax: " + xs.maxBy(_._2))
//    val ys = coords.filter(_._1 == "y")
//    println("ys: " + ys)
//    println("yMin: " + ys.minBy(_._2))
//    println("yMax: " + ys.maxBy(_._2))
//    val zs = coords.filter(_._1 == "z")
//    println("zs: " + zs)
//    println("zMin: " + zs.minBy(_._2))
//    println("zMax: " + zs.maxBy(_._2))
//    val ws = coords.filter(_._1 == "w")
//    println("ws: " + ws)
//    println("wMin: " + ws.minBy(_._2))
//    println("wMax: " + ws.maxBy(_._2))
    println("-------------------------------------")
    nextState(acc)
  }).count(_._2 == active)

}
