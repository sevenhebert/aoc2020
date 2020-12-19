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

  private def getNeighbors(cubeKey: String): Map[String, Char] = {
    val Array(xIdx, yIdx, zIdx, wIdx) = cubeKey.split(',').map(_.drop(2).toInt)

    val initNeighbors = for {
      x <- List(xIdx - 1, xIdx, xIdx + 1)
      y <- List(yIdx - 1, yIdx, yIdx + 1)
      z <- List(zIdx - 1, zIdx, zIdx + 1)
      w <- List(wIdx - 1, wIdx, wIdx + 1)
    } yield s"x:$x,y:$y,z:$z,w:$w" -> inactive

    initNeighbors.toMap.removed(cubeKey)
  }

  private def nextCubeState(cur: Char)(count: Int) =
    if (cur == active && (count == 2 || count == 3)) active
    else if (cur == inactive && count == 3) active
    else inactive

  private def nextState(prevState: Map[String, Char]): Map[String, Char] = {
    val state = prevState.keysIterator.flatMap(getNeighbors).toMap ++ prevState
    state.map { case (cube, status) =>
      val count = getNeighbors(cube).keysIterator.flatMap(state.get).count(_ == active)
      cube -> nextCubeState(status)(count)
    }
  }

  lazy val res2 = List.from(1 to 6).foldLeft[Map[String, Char]](input)((acc, _) => {
    val stateNext = nextState(acc)
    println("number active: " + stateNext.count(_._2 == active))
    stateNext
  }).count(_._2 == active)

}
