package day13

import scala.util.{Failure, Success, Try}

object Day13 {

  val filename = "day13/input.txt"

  val input = Try(
    io.Source.fromResource(filename).getLines.toArray) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  val depart = input.head.toLong

  val unrestrictedBusLines = input(1).split(',')
  val restrictedBusLines = unrestrictedBusLines.filterNot(_ == "x").map(_.toLong)

  val res1 = restrictedBusLines.map(bus => (bus, ((bus * (depart / bus)) + bus) - depart)).minBy(_._2)

  /* (busline: Long, offset: Int) */
  val busLinesOffsets = unrestrictedBusLines.zipWithIndex.filterNot(_._1 == "x").map(bus => (bus._1.toLong, bus._2.toLong))

  val hint = 100000000000000L
  val r = busLinesOffsets.map(bo => {

    val (bus, offset) = bo
    val numDepartsBeforeHint = bus * (hint / bus)
    val numDepartsAfterHint = numDepartsBeforeHint + bus + offset
    val minsUntilNextDepart = numDepartsAfterHint - hint

    (bus, minsUntilNextDepart)
  }) // Array((17,9), (41,11), (37,22), (367,128), (19,39), (23,51), (29,47), (613,51), (13,65))

  val aOffset = 0
  val bOffset = 1
  val cOffset = 4
  val dOffset = 6
  val eOffset = 7

def solve2(t: Long, a: Int, b: Int, c: Int, d: Int, e: Int): Long = {
    if (t !=0 && a == 7 && b == bOffset && c == cOffset && d == dOffset && e == eOffset) t
    else if (t == 166500) t
    else {
      val nextA = if (a == 1) 17 else a - 1
      val nextB = if (b == 1) 41 else b - 1
      val nextC = if (c == 1) 37 else c - 1
      val nextD = if (d == 1) 367 else d - 1
      val nextE = if (e == 1) 19 else e - 1
      solve2(t + 1, nextA, nextB, nextC, nextD, nextE)
    }
  }

  lazy val res2 = solve2(0, 9, 13, 59, 31, 19)

}
