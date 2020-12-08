package day8

import scala.util.{Failure, Success, Try}

object Day8 {

  val filename = "input.txt"

  val input = Try(io.Source.fromResource(filename).getLines.toArray.map(x => (x.take(3), x.drop(4)))) match {
    case Failure(error) => throw new Exception(error.getMessage)
    case Success(res)   => res
  }

  def getSign(c: Char): Int = if (c == '-') -1 else 1

  def solve(idx: Int, seen: Set[Int], lastAcc: Int): Int = {
    if (seen.contains(idx)) lastAcc
    else {
      val (op, move) = input(idx)
      val update = seen + idx
      val delta = getSign(move(0)) * move.drop(1).toInt
      op match {
        case "acc" => solve(idx + 1, update, lastAcc + delta)
        case "jmp" => solve(idx + delta, update, lastAcc)
        case "nop" => solve(idx + 1, update, lastAcc)
      }
    }
  }

  val res1 = solve(0, Set.empty, 0)

  val switch = Map("acc" -> "acc", "jmp" -> "nop", "nop" -> "jmp")

  def nextChange(idx: Int): Int ={
    if (input(idx)._1 == "acc") nextChange(idx + 1)
    else idx
  }

  def solve2(idx: Int, seen: Set[Int], acc: Int, chgIdx: Int): Int =
    if (idx > input.length - 1) acc
    else if (seen.contains(idx)) solve2(0, Set.empty, 0, nextChange(chgIdx + 1))
    else {
      val (op, move) = if (idx == chgIdx) (switch(input(idx)._1), input(idx)._2) else input(idx)
      val update = seen + idx
      val delta = getSign(move(0)) * move.drop(1).toInt
      op match {
        case "acc" => solve2(idx + 1, seen, acc + delta, chgIdx)
        case "jmp" => solve2(idx + delta, update, acc, chgIdx)
        case "nop" => solve2(idx + 1, update, acc, chgIdx)
      }
    }

  val res2 = solve2(0, Set.empty, 0, 0)

}
