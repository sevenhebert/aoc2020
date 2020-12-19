package day17

import scala.util.{Failure, Success, Try}

object Day17 {

  val filename = "day17/input.txt"

  val input = Try(
    io.Source
      .fromResource(filename)
      .getLines
      .map(_.toCharArray)
      .toArray) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  private val active = '#'
  private val inactive = '.'

  private val activePattern = "([#])".r
  private val inactivePattern = "([.])".r

  private def nextCubeState(cur: Char)(count: Int) =
    if (cur == active && (count == 2 || count == 3)) active
    else if (cur == inactive && count == 3) active
    else inactive

  private def countActiveNeighbors(layer: Int, row: Int, col: Int, state: Array[Array[Array[Char]]]) = {
    val depth = state.length - 1
    val length = state.head.length - 1
    val width = state.head.head.length - 1

    def count(lyrIdx: Int, rowIdx: Int, colIdx: Int): Int =
      if (lyrIdx < 0 || lyrIdx > depth || rowIdx < 0 || rowIdx > length || colIdx < 0 || colIdx > width) 0
      else if (lyrIdx == layer && rowIdx == row && colIdx == col) 0
      else state(lyrIdx)(rowIdx)(colIdx) match {
        case activePattern(_) => 1
        case inactivePattern(_) => 0
      }

    List(layer - 1, layer, layer + 1).map(lyr =>
      /* north + northeast + northwest + south + southeast + southwest + center + east + west */
      count(lyr, row - 1, col) +
        count(lyr, row - 1, col + 1) +
        count(lyr, row - 1, col - 1) +
        count(lyr, row + 1, col) +
        count(lyr, row + 1, col + 1) +
        count(lyr, row + 1, col - 1) +
        count(lyr, row, col) +
        count(lyr, row, col + 1) +
        count(lyr, row, col - 1)
    ).sum
  }

  def increaseArea(state: Array[Array[Array[Char]]]): Array[Array[Array[Char]]] =
    state.map(layer => layer.map(_.prepended(inactive).appended(inactive))
      .prepended(Array.fill(state.head.length + 2)(inactive))
      .appended(Array.fill(state.head.length + 2)(inactive)))
      .prepended(Array.fill(state.head.length + 2)(Array.fill(state.head.length + 2)(inactive)))
      .appended(Array.fill(state.head.length + 2)(Array.fill(state.head.length + 2)(inactive)))

  def nextState(state: Array[Array[Array[Char]]]): Array[Array[Array[Char]]] =
    state.zipWithIndex.map {
      case (layer, layerIdx) =>
        layer.zipWithIndex.map {
          case (row, rowIdx) =>
            row.zipWithIndex.map {
              case (cube, colIdx) =>
                nextCubeState(cube)(countActiveNeighbors(layerIdx, rowIdx, colIdx, state))
            }
        }
    }

  List.from(1 to 6).foldLeft[Array[Array[Array[Char]]]](Array(input))((acc, _) => {
    acc.foreach(layer => {
      layer.foreach(row => println(row.mkString))
      println("acc depth: " + acc.length)
      println("length: " + layer.length)
      println("width: " + layer.head.length)
      println
    })

    println(acc.flatten.flatten.count(_ == active))
    println("-------------------------------------")
    nextState(increaseArea(acc))
  }).flatten.flatten.count(_ == active)

}
