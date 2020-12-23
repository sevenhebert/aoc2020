package day21

import scala.util.{Failure, Success, Try}

object Day21 {

  val filename = "day21/input.txt"

  val input = Try(
    io.Source
      .fromResource(filename)
      .getLines
      .map { str => str.splitAt(str.indexOf("(contains")) }
      .map { case (i, a) =>
        val allergen = a.dropRight(1).drop(10).replaceAll(",", "").split("\\s")
        val ingredient = i.split("\\s").toSet
        (allergen, ingredient)
      }
      .toArray
  ) match {
    case Failure(err) => throw new Exception(err.getMessage)
    case Success(res) => res
  }

  val allergenIngredientsMap = input.foldRight[Map[String, Set[String]]](Map.empty) {
    case ((allergens, ingredients), acc) => acc ++ allergens.map(allergen => acc.get(allergen) match {
      case Some(i) => allergen -> i.intersect(ingredients)
      case None => allergen -> ingredients
    })
  }

  val allIngredientsWithAllergens = allergenIngredientsMap.values.reduce((acc, cur) => acc ++ cur)

  val allIngredients = input.flatMap(_._2)

  val allIngredientsWithoutAllergens = allIngredients.toSet.diff(allIngredientsWithAllergens)

  val res1 = allIngredientsWithoutAllergens.foldRight(0)((i, acc) => acc + allIngredients.count(_ == i)) // 2493

  def solve2(cur: Array[(String, Set[String])], idx: Int): String =
    if (idx == cur.length && cur.forall(_._2.size == 1)) cur.sortBy(_._1).map(_._2.head).mkString(",")
    else if (idx == cur.length) solve2(cur, 0)
    else if (cur(idx)._2.size != 1) solve2(cur, idx + 1)
    else solve2(cur.map(el => if (el == cur(idx)) el else (el._1, el._2.diff(cur(idx)._2))), idx + 1)

  val res2 = solve2(allergenIngredientsMap.toArray, 0) // kqv,jxx,zzt,dklgl,pmvfzk,tsnkknk,qdlpbt,tlgrhdh

}
