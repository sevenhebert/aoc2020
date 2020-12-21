//package day18
//
//import scala.util.parsing.combinator._
//
//class ExpressionParser extends RegexParsers {
//
//  def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
//
//  def expression: Parser[Int] = (number ^^ { _.toInt }) ~ opt(operator ~ expression) ^^ {
//    case singleInt ~ None =&gt; singleInt
//    case
//  }
//
//  def operator: Parser[(Int, Int) => Int] = ("*" | "+") ^^ {
//    case "*" => (x, y) => x * y
//    case "+" => (x, y) => x + y
//  }
//
//}
//
//object Day18 extends ExpressionParser {
//
//  val result = parseAll(expression, "9 * 2 + 3 + 9 * ((3 * 3 + 7 * 6 + 5 + 5) * 8 * (6 * 8 + 7 * 2) + 4)")
//
//}
//
///*
//  operand stack as a stack of numeric values, initially empty.
//  operator stack as a stack of operators, initially empty.
//  priorities as a map (lookup table) of operators to priority levels (all priorities positive, with higher value = higher priority).
//  position as current parsing position in expression, initially 0.
// */
