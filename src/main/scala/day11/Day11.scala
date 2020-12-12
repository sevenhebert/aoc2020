package day11

import scala.util.{Failure, Success, Try}

object Day11 {

  val filename = "day11/input.txt"

  private val input = Try(
    io.Source
      .fromResource(filename)
      .getLines
      .map(_.toCharArray)
      .toArray) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  private val width = input(0).length - 1
  private val height = input.length - 1

  private val empty = 'L'
  private val occupied = '#'

  private val emptyPattern = "([L])".r
  private val occupiedPattern = "([#])".r
  private val floorPattern = "([.])".r

  def nextSeatState(cur: Char, tolerance: Int)(count: Int) =
    if (cur == empty && count == 0) occupied
    else if (cur == occupied && count > tolerance) empty
    else cur

  def countSeats(row: Int, col: Int, state: Array[Array[Char]], onlyAdjacent: Boolean) = {

    def rec(rowIdx: Int, colIdx: Int, rise: Int, run: Int): Int =
      if (rowIdx < 0 || rowIdx > height || colIdx < 0 || colIdx > width) 0
      else state(rowIdx)(colIdx) match {
        case emptyPattern(_) => 0
        case occupiedPattern(_) => 1
        case floorPattern(_) => if (onlyAdjacent) 0 else rec(rowIdx + rise, colIdx + run, rise, run)
      }

    /* north + northeast + northwest + south + southeast + southwest + east + west */
    rec(row - 1, col, -1, 0) +
      rec(row - 1, col + 1, -1, 1) +
      rec(row - 1, col - 1, -1, -1) +
      rec(row + 1, col, 1, 0) +
      rec(row + 1, col + 1, 1, 1) +
      rec(row + 1, col - 1, 1, -1) +
      rec(row, col + 1, 0, 1) +
      rec(row, col - 1, 0, -1)
  }

  def nextState(tolerance: Int, onlyAdjacent: Boolean, state: Array[Array[Char]]): Array[Array[Char]] =
    state.zipWithIndex.map({
      case (row, rowIdx) =>
        row.zipWithIndex.map({
          case (seat, seatIdx) =>
            nextSeatState(seat, tolerance)(countSeats(rowIdx, seatIdx, state, onlyAdjacent))
        })
    })

  lazy val res1 = List
    .from(1 to 100)
    .foldLeft[Array[Array[Char]]](input)((acc, _) => nextState(3, onlyAdjacent = true, acc))
    .flatten
    .count(_ == occupied) // 2438

  lazy val res2 = List
    .from(1 to 100)
    .foldLeft[Array[Array[Char]]](input)((acc, _) => nextState(4, onlyAdjacent = false, acc))
    .flatten
    .count(_ == occupied) // 2174

  List.from(1 to 100).foldLeft[Array[Array[Char]]](input)((acc, _) => {
    println(acc.flatten.count(_ == occupied))
    nextState(???, ???, acc)
  }).flatten.count(_ == occupied)
}
