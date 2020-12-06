package day5

import scala.io.Source

object Day5 {

  val input = Source.fromResource("day5/input.txt").getLines.toArray

  val lowerHalf = "([FL])".r
  val upperHalf = "([BR])".r

  val boardingPasses = input.map(i => Integer.parseInt(i.map({
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

