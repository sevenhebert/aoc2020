package day8

import scala.util.{Failure, Success, Try}

object Day8 {

  val filename = "day8/input.txt"

  val input = Try(
    io.Source
      .fromResource(filename)
      .getLines
      .toArray
      .map(x => (x.take(3), x.drop(4)))) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  def solve(idx: Int, seen: Set[Int], lastAcc: Int): Int = {
    if (seen.contains(idx)) lastAcc
    else {
      val (op, move) = input(idx)
      val update = seen + idx
      op match {
        case "acc" => solve(idx + 1, update, lastAcc + move.toInt)
        case "jmp" => solve(idx + move.toInt, update, lastAcc)
        case "nop" => solve(idx + 1, update, lastAcc)
      }
    }
  }

  val res1 = solve(0, Set.empty, 0)

  def nextChgIdx(idx: Int): Int = if (input(idx)._1 == "acc") nextChgIdx(idx + 1) else idx

  val switch = Map("jmp" -> "nop", "nop" -> "jmp")

  def solve2(idx: Int, seen: Set[Int], acc: Int, chgIdx: Int): Int =
    if (idx > input.length - 1) acc
    else if (seen.contains(idx)) solve2(0, Set.empty, 0, nextChgIdx(chgIdx + 1))
    else {
      val (op, move) =
        if (idx == chgIdx) (switch(input(idx)._1), input(idx)._2)
        else input(idx)
      val update = seen + idx
      op match {
        case "acc" => solve2(idx + 1, seen, acc + move.toInt, chgIdx)
        case "jmp" => solve2(idx + move.toInt, update, acc, chgIdx)
        case "nop" => solve2(idx + 1, update, acc, chgIdx)
      }
    }

  val res2 = solve2(0, Set.empty, 0, 0)

}
