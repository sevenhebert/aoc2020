package day11

import scala.annotation.tailrec
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
  private val floor = '.'

  private val emptyPattern = "([L])".r
  private val occupiedPattern = "([#])".r
  private val floorPattern = "([.])".r


  def nextSeatState1(cur: Char)(count: Int) =
    if (cur == empty && count == 0) occupied
    else if (cur == occupied && count > 3) empty
    else cur

  def nextSeatState2(cur: Char)(count: Int) =
    if (cur == empty && count == 0) occupied
    else if (cur == occupied && count > 4) empty
    else cur

  def countAdjacents(row: Int, col: Int, state: Array[Array[Char]]) = {
    def n(rowIdx: Int, colIdx: Int): Int =
      if (rowIdx < 0) 0
      else state(rowIdx)(colIdx) match {
        case emptyPattern(_)    => 0
//        case floorPattern(_)    => 0 // part1
        case floorPattern(_)    => n(rowIdx - 1, colIdx)
        case occupiedPattern(_) => 1
      }

    def ne(rowIdx: Int, colIdx: Int): Int =
      if (rowIdx < 0 || colIdx > width) 0
      else state(rowIdx)(colIdx) match {
        case emptyPattern(_)    => 0
//        case floorPattern(_)    => 0
        case floorPattern(_)    => ne(rowIdx - 1, colIdx + 1)
        case occupiedPattern(_) => 1
      }

    def nw(rowIdx: Int, colIdx: Int): Int =
      if (rowIdx < 0 || colIdx < 0) 0
      else state(rowIdx)(colIdx) match {
        case emptyPattern(_)    => 0
//        case floorPattern(_)    => 0
        case floorPattern(_)    => nw(rowIdx - 1, colIdx - 1)
        case occupiedPattern(_) => 1
      }

    def s(rowIdx: Int, colIdx: Int): Int =
      if (rowIdx > height) 0
      else state(rowIdx)(colIdx) match {
        case emptyPattern(_)    => 0
//        case floorPattern(_)    => 0
        case floorPattern(_)    => s(rowIdx + 1, colIdx)
        case occupiedPattern(_) => 1
      }

    def se(rowIdx: Int, colIdx: Int): Int =
      if (rowIdx > height || colIdx > width) 0
      else state(rowIdx)(colIdx) match {
        case emptyPattern(_)    => 0
//        case floorPattern(_)    => 0
        case floorPattern(_)    => se(rowIdx + 1, colIdx + 1)
        case occupiedPattern(_) => 1
      }

    def sw(rowIdx: Int, colIdx: Int): Int =
      if (rowIdx > height || colIdx < 0) 0
      else state(rowIdx)(colIdx) match {
        case emptyPattern(_)    => 0
//        case floorPattern(_)    => 0
        case floorPattern(_)    => sw(rowIdx + 1, colIdx - 1)
        case occupiedPattern(_) => 1
      }

    def e(rowIdx: Int, colIdx: Int): Int =
      if (colIdx > width) 0
      else state(rowIdx)(colIdx) match {
        case emptyPattern(_)    => 0
//        case floorPattern(_)    => 0
        case floorPattern(_)    => e(rowIdx, colIdx + 1)
        case occupiedPattern(_) => 1
      }

    def w(rowIdx: Int, colIdx: Int): Int =
      if (colIdx < 0) 0
      else state(rowIdx)(colIdx) match {
        case emptyPattern(_)    => 0
//        case floorPattern(_)    => 0
        case floorPattern(_)    => w(rowIdx, colIdx - 1)
        case occupiedPattern(_) => 1
      }

      n(row - 1, col) +
      ne(row - 1, col + 1) +
      nw(row - 1, col - 1) +
      s(row + 1, col) +
      se(row + 1, col + 1) +
      sw(row + 1, col - 1) +
      e(row, col + 1) +
      w(row, col - 1)
  }

  def nextState(state: Array[Array[Char]]): Array[Array[Char]] =
    state.zipWithIndex.map({
      case (row, rowIdx) =>
        row.zipWithIndex.map({
          case (seat, seatIdx) =>
//            nextSeatState1(seat)(countAdjacents(rowIdx, seatIdx, state)) // part1
            nextSeatState2(seat)(countAdjacents(rowIdx, seatIdx, state))
        })
    })

//  lazy val res1 = List
//    .from(1 to 100)
//    .foldLeft[Array[Array[Char]]](input)((acc, _) => nextState(acc))
//    .flatten
//    .count(_ == occupied)
//
//  lazy val res2 = List
//    .from(1 to 100)
//    .foldLeft[Array[Array[Char]]](input)((acc, _) => nextState(acc))
//    .flatten
//    .count(_ == occupied)

    List.from(1 to 50).foldLeft[Array[Array[Char]]](input)((acc, _) => {
        println(acc.flatten.count(_ == occupied))
        nextState(acc)
      }).flatten.count(_ == occupied)
}
