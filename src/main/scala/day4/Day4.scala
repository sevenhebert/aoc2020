package day4

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object Day4 {

  val filename = "day4/input.txt"

  val input = Try(
    io.Source
      .fromResource(filename)
      .mkString
      .split("\n\n")
      .map(_.replaceAll("\n", " "))
  ) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  case class Passport(
   byr: Int,
   iyr: Int,
   eyr: Int,
   hgt: String,
   hcl: String,
   ecl: String,
   pid: String,
   cid: Option[String]
  ) {
    require(1920 to 2002 contains byr)
    require(2010 to 2020 contains iyr)
    require(2020 to 2030 contains eyr)
    if (hgt.takeRight(2) == "cm") require(150 to 193 contains hgt.dropRight(2).toInt)
    else require(59 to 76 contains hgt.dropRight(2).toInt)
    require(hcl.charAt(0) == '#' && hcl.drop(1).forall(_.isLetterOrDigit))
    require(List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(ecl))
    require(pid.length == 9)
  }

  val res = input.flatMap(i => {
    val maybePassport = i.split(" ").map(el => (el.take(3), el.drop(4))).toMap
    try {
      Some(Passport(
        maybePassport("byr").toInt,
        maybePassport("iyr").toInt,
        maybePassport("eyr").toInt,
        maybePassport("hgt"),
        maybePassport("hcl"),
        maybePassport("ecl"),
        maybePassport("pid"),
        maybePassport.get("cid")
      ))
    } catch {
      case NonFatal(_) => None
    }
  }).length
}
