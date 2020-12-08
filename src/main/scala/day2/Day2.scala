package day2

import scala.util.{Failure, Success, Try}

object Day2 {

  val filename = "day2/input.txt"

  val input = Try(
    io.Source.fromResource(filename).getLines.toList) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  val res1 = input.foldRight(0)((el, acc) => {
    val input = el.split(" ")
    val range = input(0).split("-")
    val min = range(0).toInt
    val max = range(1).toInt
    val letter = input(1).charAt(0)
    val count = input(2).count(_ == letter)
    val update = if (min to max contains count) 1 else 0
    acc + update
  })

  val res2 = input.foldRight(0)((el, acc) => {
    val input = el.split(" ")
    val positions = input(0).split("-")
    val aIdx = positions(0).toInt - 1
    val bIdx = positions(1).toInt - 1
    val letter = input(1).charAt(0)
    val aIsValid = input(2).charAt(aIdx).equals(letter)
    val bIsValid = input(2).charAt(bIdx).equals(letter)
    (aIsValid, bIsValid) match {
      case (true, true)   => 0 + acc
      case (false, false) => 0 + acc
      case (true, false)  => 1 + acc
      case (false, true)  => 1 + acc
    }
  })
}
