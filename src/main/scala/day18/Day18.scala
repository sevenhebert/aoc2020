package day18

import scala.util.parsing.combinator.RegexParsers
import scala.util.Try

class SimpleCalculator extends RegexParsers {

  def number: Parser[BigInt] = """\d+(\.\d*)?""".r ^^ { _.toInt }

  def expr1: Parser[BigInt] = factor1 ~ rep("*" ~ factor1 | "+" ~ factor1) ^^ { case n ~ ls => ls.foldLeft(n) {
    case (x, "+" ~ y) => x + y
    case (x, "*" ~ y) => x * y
  }}

  def factor1: Parser[BigInt] = number | "(" ~> expr1 <~ ")"

  //  assert(evaluate(expr1, "2 * 3 + (4 * 5)") == 26)
  //  assert(evaluate(expr1, "5 + (8 * 3 + 9 + 3 * 4 * 3)") == 437)
  //  assert(evaluate(expr1, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") == 12240)
  //  assert(evaluate(expr1, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 13632)

  def multiplication: Parser[BigInt => BigInt] = "*" ~ term ^^ { case "*" ~ n => _ * n }

  def addition: Parser[BigInt => BigInt] = "+" ~ factor2 ^^ { case "+" ~ n => _ + n }

  def expr2: Parser[BigInt] = term ~ rep(multiplication) ^^ { case n ~ ls => ls.foldLeft(n)((acc, f) => f(acc)) }

  def factor2: Parser[BigInt] = number | "(" ~> expr2 <~ ")"

  def term: Parser[BigInt] = factor2 ~ rep(addition) ^^ { case n ~ ls => ls.foldLeft(n)((acc, f) => f(acc)) }

  //  assert(evaluate(expr2, "2 * 3 + (4 * 5)") == 46)
  //  assert(evaluate(expr2, "5 + (8 * 3 + 9 + 3 * 4 * 3)") == 1445)
  //  assert(evaluate(expr2, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") == 669060)
  //  assert(evaluate(expr2, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 23340)

  def evaluate(parser: Parser[BigInt], input: String): BigInt = parseAll(parser, input) match {
    case Success(result, _) => result
    case NoSuccess(msg, _) => scala.sys.error(msg)
  }}

object Day18 extends SimpleCalculator {

  val filename = "day18/input.txt"

  val input = Try(io.Source.fromResource(filename).getLines.toArray) match {
    case util.Failure(err) => throw new Exception(err.getMessage)
    case util.Success(res) => res
  }

  val res1: BigInt = input.map(evaluate(expr1, _)).sum // 1890866893020

  val res2: BigInt = input.map(evaluate(expr2, _)).sum // 34646237037193

}
