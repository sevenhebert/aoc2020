import scala.util.{Failure, Success, Try}

object Day9 {

  val filename = "day9/input.txt"

  val input = Try(
    io.Source.fromResource(filename).getLines.map(_.toLong).toArray) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  def isValid(sliceIdx: Int, slice: Array[Long], target: Long): Boolean = {
    if (sliceIdx == 25) false
    else slice.find(_ + slice(sliceIdx) == target) match {
      case None => isValid(sliceIdx + 1, slice, target)
      case Some(value) =>
        if (value != slice(sliceIdx)) true
        else isValid(sliceIdx + 1, slice, target)
    }
  }

  def solve(idx: Int): Long = {
    val pointer = idx + 25
    val slice = input.slice(idx, idx + 25)
    val targetSum = input(pointer)
    if (isValid(0, slice, targetSum)) solve(idx + 1)
    else input(pointer)
  }

  val res1 = solve(0)

  val target = 248131121

  def solve2(i: Int, j: Int, cache: Set[(Int, Int)]): Long = {
    if (cache.contains(i, j)) solve2(i, j + 1, cache)
    else {
      val slice = input.slice(i, j + 1)
      val sliceSum = slice.sum
      val update = cache + ((i, j))
      if (sliceSum == target) slice.min + slice.max
      else if (sliceSum > target) solve2(i + 1, i + 2, update)
      else solve2(i, j + 1, update)
    }
  }

  val res2 = solve2(0, 1, Set.empty)

}
