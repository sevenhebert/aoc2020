package day19

import day19.Day19._

import scala.annotation.tailrec

object Day19v2 {

  def checkMessage(message: String, ruleMap: Map[Int, Rule]): Boolean = {

    def loopUntilComplete(startIdx: Int): (Boolean, Int) = {
      @tailrec
      def fortyTwoUntilFalse(curIdx: Int, count: Int): (Int, Int) = {
        val (isValid, nextIdx) = recurse(curIdx, ruleMap(42))
        if (isValid && nextIdx < message.length) fortyTwoUntilFalse(nextIdx, count + 1)
        else (curIdx, count)
      }

      @tailrec
      def thirtyOneUntilComplete(curIdx: Int, remaining: Int): (Boolean, Int) = {
        val (isValid, nextIdx) = recurse(curIdx, ruleMap(31))
        if (isValid && remaining > 0 && nextIdx == message.length) (true, nextIdx)
        else if (isValid && nextIdx < message.length) thirtyOneUntilComplete(nextIdx, remaining - 1)
        else (false, curIdx)
      }

      (thirtyOneUntilComplete _).tupled(fortyTwoUntilFalse(startIdx, 0))
    }

    def recurse(curIdx: Int, rule: Rule): (Boolean, Int) =
      if (curIdx == message.length) (true, curIdx)
      else rule match {
        case Terminal(char) =>
          if (char == message.charAt(curIdx)) (true, curIdx + 1)
          else (false, curIdx)
        case Combine2(ls, rs) =>
          (ls, rs) match {
            case (Substitute(8), Substitute(11)) =>
              val (firstEightIsValid, nextIdx) = recurse(curIdx, ruleMap(42))
              if (firstEightIsValid) loopUntilComplete(nextIdx)
              else (false, curIdx)
            case _ =>
              val (leftIsValid, nextIdx) = recurse(curIdx, ls)
              if (leftIsValid) recurse(nextIdx, rs)
              else (false, curIdx)
          }
        case Branch(ls, rs) =>
          val (leftIsValid, nextIdx) = recurse(curIdx, ls)
          if (leftIsValid) (true, nextIdx)
          else recurse(curIdx, rs)
        case Substitute(sub) => recurse(curIdx, ruleMap(sub))
      }

    val (isValid, endIdx) = recurse(0, ruleMap(0))
    isValid && endIdx == message.length
  }

  lazy val res1: Int = messages1.map(checkMessage(_, ruleMap1)).count(_ == true) // 187

  val Array(rules2, messages2) = getInput(filename2)

  val ruleMap2: Map[Int, Rule] = getRuleMap(rules2)

  lazy val res2: Int = messages2.map(checkMessage(_, ruleMap2)).count(_ == true) // 392

}
