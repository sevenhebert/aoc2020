package day22

import scala.util.{Failure, Success, Try}

object Day22 {

  val filename = "day22/input.txt"

  val (playerOne, playerTwo) = Try {
    val Array(p1, p2) = io.Source
      .fromResource(filename)
      .mkString
      .split("\n\n")
      .map(_.split("\n"))
    (p1.tail.map(_.toInt).toList, p2.tail.map(_.toInt).toList)
  } match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  def getScore(winningDeck: List[Int]) = winningDeck.reverse.zipWithIndex.foldLeft(0) {
    case (acc, (cur, idx)) => acc + (cur * (idx + 1))
  }

  def solve1(d1: List[Int], d2: List[Int]): (Int, List[Int]) =
    if (d1.isEmpty) (2, d2)
    else if (d2.isEmpty) (1, d1)
    else if (d1.head > d2.head) solve1(d1.tail :+ d1.head :+ d2.head, d2.tail)
    else solve1(d1.tail, d2.tail :+ d2.head :+ d1.head)

  val (winner1, winner1Deck) = solve1(playerOne, playerTwo)

  val score1 = getScore(winner1Deck) // 32495

  def solve2(d1: List[Int], d2: List[Int], seen: Set[(List[Int], List[Int])]): (Int, List[Int]) = {
    val nextSn = seen + ((d1, d2))

    if (d1.isEmpty) (2, d2)
    else if (d2.isEmpty) (1, d1)
    else if (seen == nextSn) (1, d1)
    else {
      val (p1Hand :: p1Rest, p2Hand :: p2Rest) = (d1, d2)

      if (p1Rest.length < p1Hand || p2Rest.length < p2Hand) {
        if (p1Hand > p2Hand) solve2(p1Rest :+ p1Hand :+ p2Hand, p2Rest, nextSn)
        else solve2(p1Rest, p2Rest :+ p2Hand :+ p1Hand, nextSn)
      }
      else {
        val (subGameWinner, _) = solve2(p1Rest.take(p1Hand), p2Rest.take(p2Hand), Set())
        if (subGameWinner == 1) solve2(p1Rest :+ p1Hand :+ p2Hand, p2Rest, nextSn)
        else solve2(p1Rest, p2Rest :+ p2Hand :+ p1Hand, nextSn)
      }
    }
  }

  val (winner2, winner2Deck) = solve2(playerOne, playerTwo, Set())

  val score2 = getScore(winner2Deck) // 32665

}
