package day1

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.util.{Failure, Success, Try}

object Day1 {

  val filename = "day1/input.txt"

  val input = Try(
    io.Source.fromResource(filename).getLines.map(_.toInt).toArray) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  @tailrec
  def twoSum(ls: Array[Int], seen: HashSet[Int]): Int = {
    val target = 2020 - ls.head
    if (seen.contains(target)) target * ls.head
    else twoSum(ls.tail, seen + ls.head)
  }

  val twoSumRes = twoSum(input.tail, HashSet(input.head))

  @tailrec
  def threeSum(i: Int, j: Int, seen: HashSet[Int]): Int = {
    if (j == input.length) threeSum(i + 1, i + 2, seen)
    else {
      val target = 2020 - input(i) - input(j)
      if (seen.contains(target)) target * input(i) * input(j)
      else threeSum(i, j + 1, seen + input(j))
    }
  }

  val threeSumRes = threeSum(0, 1, HashSet(input(0)))

}
