package day7

import scala.util.{Failure, Success, Try}

object Day7 {

  val filename = "day7/input.txt"

  val input = Try(io.Source.fromResource(filename).getLines.toList) match {
    case Failure(error) => throw new Exception(error.getMessage)
    case Success(value) => value
  }
  def parse(input: List[String]) = input.map(line => {
    val pattern = "(\\sbag+?)(\\b|s\\b)(\\.|)"
    val raw = line.replaceAll(pattern, "")
    val (key, values) = raw.splitAt(raw.indexOf("contain"))
    (key.trim, values.replaceAll("contain ", ""))
  })

  val n = parse(input)

  val lookupContainingBags = n.foldRight[Map[String, Set[String]]](Map.empty)((cur, acc) => {
    val (value, rawKeys) = cur
    val update = rawKeys.split(", ").map(k => (k.filterNot(_.isDigit).trim, value))
    update.foldRight[Map[String, Set[String]]](acc)((c, updatedMap) => {
      val (updateKey, containedByBag) = c
      updatedMap.get(updateKey) match {
        case Some(containedByBags) => updatedMap + (updateKey -> (containedByBags + containedByBag))
        case None => acc + (updateKey -> Set(containedByBag))
      }
    })
  })

  def countOuterBags(seen: Set[String], keys: Set[String]): Int =
    if (keys.isEmpty) seen.size
    else {
      val cur = keys.flatMap(k => lookupContainingBags.get(k)).flatten
      val update = seen ++ cur
      countOuterBags(update, update.diff(seen))
    }

  val res1 = countOuterBags(Set.empty, Set("shiny gold"))

  val inputMap: Map[String, Array[String]] = n.map(el => (el._1, el._2.split(", "))).toMap
  def countInnerBags(key: String): Long = inputMap.get(key) match {
    case Some(bagsContained) => 1 + bagsContained.map(bag =>
      if (bag == "no other") 0
      else {
        val (multiplier, containedBag) = bag.splitAt(2)
        multiplier.trim.toLong * countInnerBags(containedBag)
      }).sum
    case None => 0
  }

  val res2 = countInnerBags("shiny gold") - 1

}
