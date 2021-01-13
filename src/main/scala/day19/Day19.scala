package day19

import scala.util.{Failure, Success, Try}

object Day19 {

  val filename1 = "day19/input.txt"
  val filename2 = "day19/input2.txt"

  def getInput(filename: String): Array[Array[String]] = Try(
    io.Source
      .fromResource(filename)
      .mkString
      .split("\n\n")
      .map(_.split("\n"))
  ) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  val Array(rules1, messages1) = getInput(filename1)

  val terminalPattern = "(^[ab]$)".r.unanchored
  val combine2Pattern = "^([0-9]{1,3})\\s([0-9]{1,3})$".r.unanchored
  val combine3Pattern = "^([0-9]{1,3})\\s([0-9]{1,3})\\s([0-9]{1,3})$".r.unanchored
  val branchPattern = "([0-9]{1,3}\\s?[0-9]{0,3})\\s\\|\\s([0-9]{1,3}\\s?[0-9]{0,3}\\s?[0-9]{0,3})".r.unanchored
  val substitutePattern = "(^[0-9]{1,3}$)".r.unanchored

  sealed trait Rule
  case class Terminal(char: Char) extends Rule
  case class Combine2(left: Rule, right: Rule) extends Rule
  case class Combine3(left: Rule, middle: Rule, right: Rule) extends Rule
  case class Branch(ls: Rule, rs: Rule) extends Rule
  case class Substitute(sub: Int) extends Rule

  def extract(ruleStr: String): Rule = ruleStr.trim match {
    case terminalPattern(letter) => Terminal(letter.head)
    case combine3Pattern(a, b, c) => Combine3(extract(a), extract(b), extract(c))
    case combine2Pattern(a, b) => Combine2(extract(a), extract(b))
    case branchPattern(ls, rs) => Branch(extract(ls), extract(rs))
    case substitutePattern(sub) => Substitute(sub.toInt)
  }

  def getRuleMap(rules: Array[String]): Map[Int, Rule] = rules.map { str =>
    val Array(ruleId, valueStr) = str.replaceAll("\"", "").split(':')
    ruleId.toInt -> extract(valueStr)
  }.toMap

  val ruleMap1: Map[Int, Rule] = getRuleMap(rules1)

//  def process1(rule: Rule): Rule = rule match {
//    case Terminal(_) => rule
//    case Combine2(left, right) => Combine2(process1(left), process1(right))
//    case Combine3(left, middle, right) => Combine3(process1(left), process1(middle), process1(right))
//    case Branch(ls, rs) => Branch(process1(ls), process1(rs))
//    case Substitute(sub) => process1(ruleMap1(sub))
//  }

//  def getCombinations(rule: Rule): Array[String] = {
//    def combine2(ls: Array[String], rs: Array[String]): Array[String] = ls.flatMap(l => rs.map(r => l + r))
//    def combine3(ls: Array[String], ms: Array[String], rs: Array[String]): Array[String] =
//      ls.flatMap(l => ms.flatMap(m => rs.map(r => l + m + r)))
//    def traverse(r: Rule, temp: String, results: Array[String]): Array[String] = r match {
//      case Terminal(char) => results :+ (temp :+ char)
//      case Combine2(ls, rs) => combine2(traverse(ls, temp, results), traverse(rs, temp, results))
//      case Combine3(ls, ms, rs) =>
//        combine3(traverse(ls, temp, results), traverse(ms, temp, results), traverse(rs, temp, results))
//      case Branch(ls, rs) => traverse(ls, temp, results) ++ traverse(rs, temp, results)
//    }
//    traverse(rule, "", Array.empty)
//  }

//  lazy val ruleZero1: Rule = process1(ruleMap1(0))
//  lazy val ruleZeroValidMessages: Array[String] = getCombinations(ruleZero1)
//  lazy val validMessages: Int = messages1.map(ruleZeroValidMessages.contains).count(_ == true) // 187

}
