package day6

import scala.util.{Failure, Success, Try}

object Day6 {

  val filename = "day6/input.txt"

  val input = Try(
    io.Source
      .fromResource(filename)
      .mkString
      .split("\n\n")
      .map(_.split("\n"))) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  val res1 = input.map(_.reduce((acc, cur) => acc + cur).distinct.length).sum

  val res2 = input.map(_.reduce((acc, cur) => acc.intersect(cur)).length).sum
}
