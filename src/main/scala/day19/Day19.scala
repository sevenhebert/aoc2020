package day19

import scala.util.{Failure, Success, Try}

object Day19 {

  val filename = "day19/input.txt"

  val Array(rules, messages) = Try(
    io.Source
      .fromResource(filename)
      .mkString
      .split("\n\n")
      .map(_.split("\n"))
  ) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  val terminalPattern = "(^[ab]$)".r.unanchored
  val combinePattern = "^([0-9]{1,3})\\s([0-9]{1,3})$".r.unanchored
  val branchPattern = "([0-9]{1,3}\\s?[0-9]{0,3})\\s\\|\\s([0-9]{1,3}\\s?[0-9]{0,3})".r.unanchored
  val substitutePattern = "(^[0-9]{1,3}$)".r.unanchored

  sealed trait Rule
  case class Terminal(char: Char) extends Rule
  case class Combine(left: Rule, right: Rule) extends Rule
  case class Branch(ls: Rule, rs: Rule) extends Rule
  case class Substitute(sub: Int) extends Rule

  def extract(ruleStr: String): Rule = ruleStr.trim match {
    case terminalPattern(letter) => Terminal(letter.last)
    case combinePattern(a, b) => Combine(extract(a), extract(b))
    case branchPattern(ls, rs) => Branch(extract(ls), extract(rs))
    case substitutePattern(sub) => Substitute(sub.toInt)
  }

  val ruleMap = messages.map { str =>
    val Array(ruleId, valueStr) = str.replaceAll("\"", "").split(':')
    ruleId.toInt -> extract(valueStr)
  }.toMap

  def process(rule: Rule): Rule = rule match {
    case Terminal(_) => rule
    case Combine(left, right) => Combine(process(left), process(right))
    case Branch(ls, rs) => Branch(process(ls), process(rs))
    case Substitute(sub) => process(ruleMap(sub))
  }

  def getCombinations(rule: Rule): Array[String] = {
    def combine(ls: Array[String], rs: Array[String]): Array[String] = ls.flatMap(l => rs.map(r => l + r))
    def traverse(r: Rule, temp: String, results: Array[String]): Array[String] = r match {
      case Terminal(char) => results :+ (temp :+ char)
      case Combine(ls, rs) => combine(traverse(ls, temp, results), traverse(rs, temp, results))
      case Branch(ls, rs) => traverse(ls, temp, results) ++ traverse(rs, temp, results)
    }
    traverse(rule, "", Array.empty)
  }

  val ruleZero = process(ruleMap(0))
  val ruleZeroValidMessages = getCombinations(ruleZero)
  val validMessages = messages.map(ruleZeroValidMessages.contains).count(_ == true)

}
