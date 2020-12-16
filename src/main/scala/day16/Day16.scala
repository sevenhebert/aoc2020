package day16

import scala.util.{Failure, Success, Try}

object Day16 {

  val filename = "day16/input.txt"

  def parseRules(raw: String) = raw.split('\n').map(r => {
    val strs = r.split(':')
    val range = strs(1).replace("or", "-").split('-').map(_.trim.toInt)
    (strs(0), (range(0), range(1), range(2), range(3)))
  }).toMap

  def parseTickets(raw: String) = raw.split('\n').drop(1).map(_.mkString.split(',').map(_.toInt))

  val input = Try {
    val rules :: ticket :: tickets = io.Source.fromResource(filename).mkString.split("\n\n").toList

    (parseRules(rules), parseTickets(ticket).head, parseTickets(tickets.last))
  } match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  val (rulesMap, myTicket, nearbyTickets) = input

  /* aggregate min max of rules */
  val (lowMin, lowMax, highMin, highMax) = rulesMap.values
    .reduce((agg, rng) => (rng._1.min(agg._1), rng._2.max(agg._2), rng._3.min(agg._3), rng._4.max(agg._4)))

  def isValid(value: Int)(lowMin: Int, lowMax: Int, highMin: Int, highMax: Int) =
    lowMin <= value && value <= lowMax || highMin <= value && value <= highMax

  val errorRate = nearbyTickets.foldRight[Int](0)((ticket, aggTotal) =>
    aggTotal + ticket.foldRight[Int](0)((value, ticTotal) =>
      ticTotal + (if (isValid(value)(lowMin, lowMax, highMin, highMax)) 0 else value))) // 29019

  val validTickets = nearbyTickets.foldRight[Array[Array[Int]]](Array.empty)((ticket, validTix) =>
    if (ticket.forall(isValid(_)(lowMin, lowMax, highMin, highMax))) validTix :+ ticket else validTix)

  val validTixByFields = validTickets.transpose

  val invalidFieldByRule = rulesMap.keys.foldRight[Map[String, Array[Int]]](Map.empty)((ruleKey, acc) => {
    val (ruleLowMin, ruleLowMax, ruleHighMin, ruleHighMax) = rulesMap(ruleKey)
    val invalidFieldsForRule = validTixByFields.zipWithIndex.map { case (fieldValues, fieldIdx) =>
      if (fieldValues.forall(isValid(_)(ruleLowMin, ruleLowMax, ruleHighMin, ruleHighMax))) -1 else fieldIdx
    }.filterNot(_ == -1)
    acc.updated(ruleKey, invalidFieldsForRule)
  })

  val allFieldsByRule = rulesMap.keys.map(_ -> Array.from(0 until rulesMap.size)).toMap

  val validFieldByRule = invalidFieldByRule.foldRight(allFieldsByRule) {
    case ((ruleKey, invalidFields), acc) => acc.updated(ruleKey, acc(ruleKey).diff(invalidFields))
  }

  def processOfElimination(choices: Map[String, Array[Int]], chosen: Map[String, Int], prevSize: Int): Map[String, Int] = {
    if (choices.isEmpty || chosen.size == prevSize) chosen
    else {
      val (chose, available) = choices.partition(_._2.length == 1)
      val nextChoices = chose.foldRight(available) {
        case ((choseKey, choseField), result) =>
          val (containsChose, rest) = result.removed(choseKey).partition(_._2.contains(choseField.last))
          val avaliableUpdate = containsChose.map { case (key, values) => key -> values.filterNot(_ == choseField.last) }
          avaliableUpdate ++ rest
      }
      val nextChosen = chosen ++ chose.map { case (key, value) => key -> value.last }
      processOfElimination(nextChoices, nextChosen, chosen.size)
    }
  }

  val fieldIndices = processOfElimination(validFieldByRule, Map.empty, -1)

  def mapTicketToFields(ticket: Array[Int], fieldsMap: Map[String, Int]) = fieldsMap.foldRight(fieldsMap) {
    case ((key, fieldIdx), result) => result.updated(key, ticket(fieldIdx))
  }

  val res2 = mapTicketToFields(myTicket, fieldIndices).filter(_._1.startsWith("departure")).foldRight(1L) {
    case ((_, value), total) => value.toLong * total
  } // 517827547723

}
