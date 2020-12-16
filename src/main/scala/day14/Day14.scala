package day14

import scala.util.{Failure, Success, Try}

object Day14 {

  private val filename = "day14/input.txt"

  def maskParser1(s: String): List[(Char, Int)] =
    s.drop(7).reverseIterator.zipWithIndex.filter(_._1.isDigit).toList

  def maskParser2(s: String): List[(Char, Int)] =
    s.drop(7).replaceAll("0", "Z").reverseIterator.zipWithIndex.filter(_._1 != '0').toList

  def memParser(s: String): (Long, Long) =
    (s.drop(4).takeWhile(_.isDigit).toLong, s.drop(s.indexOf('=') + 2).toLong)

  val getInput = (maskParser: String => List[(Char, Int)]) => Try(io.Source
    .fromResource(filename)
    .getLines
    .toArray
    .foldLeft[List[(List[(Char, Int)], List[(Long, Long)])]](Nil)((a, c) =>
      if (c.startsWith("mask")) a :+ (maskParser(c), Nil)
      else a.dropRight(1) :+ (a.last._1, a.last._2 :+ memParser(c)))
  ) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  val input1 = getInput(maskParser1)

  def applyMask(mask: List[(Char, Int)], value: Long): Long = mask match {
    case ::((mask, maskIdx), tail) =>
      val update = if (mask == '0') value & ~(1L << maskIdx) else value | (1L << maskIdx)
      applyMask(tail, update)
    case Nil => value
  }

  def solve1(idx: Int, res: Map[Long, Long]): Long =
    if (idx == input1.length) res.values.sum
    else {
      val (mask, mem) = input1(idx)
      val update = mem.map{ case(memAddr, memVal) => memAddr -> applyMask(mask, memVal) }.toMap
      solve1(idx + 1, res ++ update)
    }

  val input2 = getInput(maskParser2)

  def maskCombos(idx: Int, cmbs: Set[List[(Char, Int)]]): Set[List[(Char, Int)]] = {
    if (idx == cmbs.head.length) cmbs.map(_.filter(_._1 != 'Z'))
    else if (cmbs.head(idx)._1 != 'X') maskCombos(idx + 1, cmbs)
    else maskCombos(idx + 1, cmbs.flatMap(combo => List(combo.updated(idx, ('1', idx)), combo.updated(idx, ('0', idx)))))
  }

  def solve2(idx: Int, res: Map[Long, Long]): Long =
    if (idx == input2.length) res.values.sum
    else {
      val (mask, mem) = input2(idx)
      val update = mem.flatMap{ case(memAddr, memVal) => maskCombos(0, Set(mask)).map(applyMask(_, memAddr) -> memVal) }.toMap
      solve2(idx + 1, res ++ update)
    }

  lazy val res1 = solve1(0, Map.empty) // 17934269678453

  lazy val res2 = solve2(0, Map.empty) // 3440662844064

}
