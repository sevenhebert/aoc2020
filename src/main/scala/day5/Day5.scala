package day5

import scala.util.{Failure, Success, Try}

object Day5 {

  val filename = "day5/input.txt"

  val input = Try(io.Source.fromResource(filename).getLines.toArray) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  val lowerHalf = "([FL])".r
  val upperHalf = "([BR])".r

  val boardingPasses = input.map(i =>
    Integer.parseInt(i.map({
      case lowerHalf(_) => '0'
      case upperHalf(_) => '1'
    }), 2))

  val highestPass = boardingPasses.max

  val sortedPasses = boardingPasses.sorted

  def findMissingPass(i: Int, j: Int): Double =
    if (((sortedPasses(j) - sortedPasses(i)) / 2) > 0.5) sortedPasses(i) + 1
    else findMissingPass(i + 1, j + 1)

  val missingPass = findMissingPass(0, 1)
}
