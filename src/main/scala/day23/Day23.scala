package day23

import scala.util.{Failure, Success, Try}

object Day23 {

  val filename = "day23/input.txt"

  val input = Try(io.Source.fromResource(filename).toVector.map(_.asDigit)) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  def getDestinationIdx(initialTarget: Int, remainingCups: Vector[Int], min: Int): Int = {
    val rcm = remainingCups.zipWithIndex.toMap

    def recurse(target: Int): Int =
      if (target < min) rcm.maxBy(_._1)._2 + 1
      else rcm.get(target) match {
        case Some(targetIdx) => targetIdx + 1
        case None => recurse(target - 1)
      }

    recurse(initialTarget)
  }

  def playNext(round: Int)(curIdx: Int, cups: Vector[Int]): String = {
    println("round " + round)
    if (round == 100) {
      val (left, right) = cups.splitAt(cups.indexOf(1))
      (right.drop(1) ++ left).mkString
    }
    else {
      val (picked, remaining) =
        if (curIdx + 4 < cups.length) (cups.slice(curIdx + 1, curIdx + 4), cups.patch(curIdx + 1, Nil, 3))
        else {
          val backSlice = cups.slice(curIdx + 1, curIdx + 4)
          val frontSlice = cups.slice(0, 3 - backSlice.length)
          (backSlice ++ frontSlice, cups.dropRight(backSlice.length).drop(frontSlice.length))
        }
      val destinationIdx = getDestinationIdx(cups(curIdx) - 1, remaining, cups.min)
      val update = {
        val temp = remaining.patch(destinationIdx, picked, 0)
        val curPos = temp.indexOf(cups(curIdx))
        if (curPos > curIdx) temp.drop(curPos - curIdx) :++ temp.take(curPos - curIdx)
        else temp
      }
      val nextCupIdx = if (curIdx + 1 == cups.length) 0 else curIdx + 1
      playNext(round + 1)(nextCupIdx, update)
    }
  }

  val res1 = playNext(0)(0, input) // 76952348

  val input2 = input :++ List.from(input.max + 1 to 1000000)

//  val res2 = playNext(0)(0, input2)

}
