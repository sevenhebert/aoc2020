package day3

import scala.util.{Failure, Success, Try}

object Day3 {

  val filename = "day3/input.txt"

  val input = Try(
    io.Source
      .fromResource(filename)
      .getLines
      .map(_.toCharArray)
      .toArray) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  val width = input(0).length - 1
  val height = input.length - 1

  def solve(colIdx: Int, rowIdx: Int, count: Int)(right: Int, down: Int): Int = {
    if (rowIdx > height) count
    else if (colIdx > width) solve(colIdx - width - 1, rowIdx, count)(right, down)
    else {
      val isTree = input(rowIdx)(colIdx) == '#'
      val countUpdate = if (isTree) count + 1 else count
      solve(colIdx + right, rowIdx + down, countUpdate)(right, down)
    }
  }


  val res1 = solve(0, 0, 0)(3, 1)

  val res2 = Array((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    .foldRight(1.toLong)((steps, acc) => acc * solve(0, 0, 0)(steps._1, steps._2).toLong)
}
