package day10

import java.time.Clock

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, HashSet}
import scala.util.{Failure, Success, Try}

object Day10 {

  val filename = "day10/input.txt"

  val input = Try(
    io.Source.fromResource(filename).getLines.map(_.toInt).toArray) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  val sorted = input.sorted
  val outlet = 0
  val device = sorted.max + 3
  val nput = (outlet +: sorted) :+ device

  val (ones, threes) = nput.sliding(2).partition(n => n(1) - n(0) > 1)
  val res1 = ones.size * threes.size


  @tailrec
  def getCombos(i: Int, cur: Array[Int], combos: HashMap[String, Array[Int]]): HashMap[String, Array[Int]] = {
    if (i == cur.length - 1) combos
    else if (cur(i + 1) - cur(i - 1) > 3) getCombos(i + 1, cur, combos)
    else {
      val subArr = cur.take(i) ++ cur.drop(i + 1)
      val subKey = subArr.mkString(":")
      val update = combos ++ HashMap(subKey -> subArr)
      getCombos(i + 1, cur, update)
    }
  }

  val initCombos = getCombos(1, nput, HashMap.empty)
  val initRes = HashSet(nput.mkString(":"))

  @tailrec
  def getInnerCombos(i: Int, cur: Array[Array[Int]], temp: HashMap[String, Array[Int]], res: HashSet[String]): HashSet[String] =
    if (cur.isEmpty) res
    else if (i == cur.length) getInnerCombos(0, temp.values.toArray, HashMap.empty, res ++ temp.keySet)
    else getInnerCombos(i + 1, cur, temp ++ getCombos(1, cur(i), HashMap.empty), res)

  lazy val res2 = getInnerCombos(0, initCombos.values.toArray, initCombos, initRes).size

  val clock: Clock = Clock.systemDefaultZone
  val startTime = clock.millis
  println(s"Part 2: $res2")
  println(s"Solved in ${clock.millis - startTime}")

}
