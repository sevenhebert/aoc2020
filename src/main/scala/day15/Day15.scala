package day15

import scala.util.{Failure, Success, Try}

object Day15 {

  private val filename = "day15/input.txt"

  private val input = Try(
    io.Source.fromResource(filename).mkString.split(',').map(_.toInt)) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  private def solve(idx: Int, last: Int)(seen: Map[Int, (Int, Int)]): Int =
    if (idx == 30000000) last
    else {
      val update = (key: Int) => seen.updatedWith(key)({
        case Some((_, number)) => Option(number, idx)
        case None => Option(-1, idx)
      })

      seen.get(last) match {
        case Some((prev, recent)) =>
          if (prev != -1) solve(idx + 1, recent - prev)(update(recent - prev))
          else solve(idx + 1, 0)(update(0))
        case None => solve(idx + 1, 0)(update(0))
      }
    }

  private val initMap = input.foldRight[Map[Int, (Int, Int)]](Map.empty)(
    (cur, acc) => acc ++ Map(cur -> (-1, input.indexOf(cur))))

//  lazy val res1 = solve(input.length, input.last)(initMap) // 376 (changed solves exit condition)

  lazy val res2 = solve(input.length, input.last)(initMap) // 323780

}
