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

  /* (bus line: Long, offset: Int) */
  val busLinesOffsets = unrestrictedBusLines.zipWithIndex.filterNot(_._1 == "x").map(bus => (bus._1.toLong, bus._2.toLong))

  val hint = 100000000000000L
  val r = busLinesOffsets.map(bo => {

    val (bus, offset) = bo
    val numDepartsBeforeHint = bus * (hint / bus)
    val numDepartsAfterHint = numDepartsBeforeHint + bus + offset
    val minsUntilNextDepart = numDepartsAfterHint - hint

    (bus, minsUntilNextDepart)
  }) // Array((17,9), (41,11), (37,22), (367,128), (19,39), (23,51), (29,47), (613,51), (13,65))

  def solve2(t: Long, a: Long, b: Long, c: Long, d: Long, e: Long, f: Long, g: Long, h: Long, i: Long): Long = {
    println("timestamp: " + t)
    println("lines: " + a, b, c, d, e, f, g, h, i)
    if (a == 0 && b == 0 && c == 0 && d == 0 && e == 0 && f == 0 && g == 0 && h == 0 && i == 0) t
    else {
      val nextA = if (a == 0) 17 else a - 1
      val nextB = if (b == 0) 41 else b - 1
      val nextC = if (c == 0) 37 else c - 1
      val nextD = if (d == 0) 367 else d - 1
      val nextE = if (e == 0) 19 else e - 1
      val nextF = if (f == 0) 23 else f - 1
      val nextG = if (g == 0) 29 else g - 1
      val nextH = if (h == 0) 613 else h - 1
      val nextI = if (i == 0) 13 else i - 1

      solve2(t + 1, nextA, nextB, nextC, nextD, nextE, nextF, nextG, nextH, nextI)
    }
  }

  lazy val res2 = solve2(0L, 9L, 11L, 22L, 128L, 39L, 51L, 47L, 51L, 65L)
}