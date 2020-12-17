package day13

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Day13 {

  private val filename = "day13/input.txt"

  private val input = Try(
    io.Source.fromResource(filename).getLines.toArray) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  private val depart = input.head.toLong

  private val unrestrictedBusLines = input(1).split(',')
  private val restrictedBusLines = unrestrictedBusLines.filterNot(_ == "x").map(_.toLong)

  val res1: (Long, Long) =
    restrictedBusLines.map(bus => (bus, ((bus * (depart / bus)) + bus) - depart)).minBy(_._2)

  val busLinesOffsets: Array[(Long, Long)] =
    unrestrictedBusLines.zipWithIndex.filterNot(_._1 == "x").map(bus => (bus._1.toLong, bus._2.toLong))

  @tailrec
  def solve2(t: Long, stepLength: Long, idx: Int): Long =
    if (idx == busLinesOffsets.length) t - stepLength
    else {
      val (busLine, offset) = busLinesOffsets(idx)

      if ((t + offset) % busLine != 0L) solve2(t + stepLength, stepLength, idx)
      else solve2(t + (busLine * stepLength), busLine * stepLength, idx + 1)
    }

  private val (busLine, _) = busLinesOffsets.head
  val res2 = solve2(0, busLine, 1)

}
