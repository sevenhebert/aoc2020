package day20

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object Day20 {

  val filename = "day20/input.txt"

  val input: Array[(Long, Array[Array[Char]])] = Try(
    io.Source
      .fromResource(filename)
      .mkString
      .split("\n\n")
      .map(str => {
        val tile = str.split("\n")
        (tile.head.drop(5).dropRight(1).toLong, tile.drop(1).map(_.toCharArray))
      })
  ) match {
    case Success(res) => res
    case Failure(err) => throw new Exception(err.getMessage)
  }

  val inputMap: Map[Long, Array[Array[Char]]] = input.toMap

  def getTileBoarders(tile: Array[Array[Char]]): Set[String] = {
    val top = tile.head.mkString
    val bottom = tile.last.mkString
    val left = tile.map(_.head).mkString
    val right = tile.map(_.last).mkString

    val transposedReverse = tile.transpose.reverse
    val trLeft = transposedReverse.map(_.head).mkString
    val trRight = transposedReverse.map(_.last).mkString

    val reverse = tile.reverse
    val rLeft = reverse.map(_.head).mkString
    val rRight = reverse.map(_.last).mkString

    Set(top, bottom, left, right, trLeft, trRight, rLeft, rRight)
  }

  val tileBoarders: Map[Long, Set[String]] = input.map {
    case (tileId, tile) => (tileId, getTileBoarders(tile))
  }.toMap

  val tileNeighborsMap: Map[Long, Set[Long]] = input.foldRight[Map[Long, Set[Long]]](Map.empty) {
    case ((tileId, tile), acc) =>
      val allOtherBoarders = tileBoarders.removed(tileId)
      acc + (tileId -> allOtherBoarders.foldRight[Set[Long]](Set.empty) {
        case ((oId, oTile), neighborAcc) =>
          if (oTile.intersect(getTileBoarders(tile)).nonEmpty) neighborAcc + oId
          else neighborAcc
      })
  }

  val tilesByNeighborCount: Map[Int, Map[Long, Set[Long]]] = tileNeighborsMap.groupBy(_._2.size)

  val corners: Map[Long, Set[Long]] = tilesByNeighborCount(2)

  val edges: Map[Long, Set[Long]] = tilesByNeighborCount(3)

  val inner: Map[Long, Set[Long]] = tilesByNeighborCount(4)

  val res1: Long = corners.foldRight(1L) { case ((tileId, _), acc) => acc * tileId } // 14129524957217

  val arrangedTileIds: Array[Array[(Long, Set[Long])]] = {
    val corner = corners.head
    val otherCorners = corners.tail

    def findNext(current: (Long, Set[Long]), tileSet: Map[Long, Set[Long]]): Map[Long, Set[Long]] =
      current._2.foldRight[Map[Long, Set[Long]]](Map.empty)((nId, acc) =>
        tileSet.get(nId) match {
          case Some(values) =>
            if (values.contains(current._1)) acc.updated(nId, values) else acc
          case None => acc
        })

    @tailrec
    def getHead(
      res: Array[(Long, Set[Long])],
      edgeMap: Map[Long, Set[Long]]
    ): (Array[(Long, Set[Long])], Map[Long, Set[Long]]) = {
      val current = res.last
      val (currentId, _) = current
      val found = findNext(current, edgeMap)
      if (found.size == 1) {
        val (nextId, nextNbrs) = found.head
        val resUpdate = res :+ (nextId, nextNbrs.excl(currentId))
        val edgeMapUpdate = edgeMap.removed(nextId)
        getHead(resUpdate, edgeMapUpdate)
      }
      else {
        val foundCorner = findNext(current, otherCorners)
        val (cornerId, cornerNbrs) = foundCorner.head
        val cornerUpdate = cornerNbrs.excl(currentId)
        val resUpdate = res :+ (cornerId, cornerUpdate)
        (resUpdate, edgeMap)
      }
    }

    val (cId, cNbr) = corner
    val initMatches = findNext(corner, edges)
    val (imId, imNbr) = initMatches.last
    val (initRow, remainingEdges) = getHead(Array((cId, cNbr.excl(imId)), (imId, imNbr.excl(cId))), edges.removed(imId))
    val length = initRow.length

    @tailrec
    def getRest(
      rIdx: Int,
      cIdx: Int,
      res: Array[Array[(Long, Set[Long])]],
      innerMap: Map[Long, Set[Long]],
      edgeMap: Map[Long, Set[Long]]
    ): Array[Array[(Long, Set[Long])]] =
      if (rIdx == length) res
      else if (cIdx == 0) {
        val (prevRowNbrId, prevRowNbrNbrs) = res(rIdx - 1)(cIdx)
        val (prnNbrId, _) = res(rIdx - 1)(cIdx + 1)
        val tileId = prevRowNbrNbrs.excl(prnNbrId).head
        val tileNbrs = if (rIdx == length - 1) otherCorners(tileId) else edgeMap(tileId)
        val rowUpdate = Array((tileId, tileNbrs.removedAll(Set(prevRowNbrId))))
        val resUpdate = res :+ rowUpdate
        val edgeMapUpdate = edgeMap.removed(tileId)
        val innerMapUpdate = if (rIdx == length - 1) edgeMapUpdate else innerMap
        getRest(rIdx, cIdx + 1, resUpdate, innerMapUpdate, edgeMapUpdate)
      }
      else if (cIdx == length - 1) {
        val (prevRowNbrId, prevRowNbrNbrs) = res(rIdx - 1)(cIdx)
        val (curRowNbrId, curRowNbrNbrs) = res(rIdx)(cIdx - 1)
        val tileId = curRowNbrNbrs.intersect(prevRowNbrNbrs).head
        val tileNbrs = if (rIdx == length - 1) otherCorners(tileId) else edgeMap(tileId)
        val rowUpdate = res(rIdx) :+ (tileId, tileNbrs.removedAll(Set(prevRowNbrId, curRowNbrId)))
        val resUpdate = res.updated(rIdx, rowUpdate)
        getRest(rIdx + 1, 0, resUpdate, innerMap, edgeMap.removed(tileId))
      }
      else {
        val (prevRowNbrId, prevRowNbrNbrs) = res(rIdx - 1)(cIdx)
        val (curRowNbrId, curRowNbrNbrs) = res(rIdx)(cIdx - 1)
        val tileId = curRowNbrNbrs.intersect(prevRowNbrNbrs).head
        val tileNbrs = innerMap(tileId)
        val tileNbrsUpdate = tileNbrs.removedAll(Set(prevRowNbrId, curRowNbrId))
        val rowUpdate = res(rIdx) :+ (tileId, tileNbrsUpdate)
        val resUpdate = res.updated(rIdx, rowUpdate)
        getRest(rIdx, cIdx + 1, resUpdate, innerMap.removed(tileId), edgeMap)
      }

    getRest(1, 0, Array(initRow), inner, remainingEdges)
  }

  val arrangedTiles: Array[Array[Array[Array[Char]]]] = arrangedTileIds.map {
    _.map { case (id, _) => inputMap(id) }
  }

  val orientNwTile: Array[Array[Char]] = arrangedTiles.head.head.map(_.reverse)

  def rotate(tile: Array[Array[Char]]): Array[Array[Char]] = tile.transpose.reverse

  def flip(tile: Array[Array[Char]]): Array[Array[Char]] = tile.reverse

  def rotateByIdx(tile: Array[Array[Char]], idx: Int): Array[Array[Char]] = idx match {
    case 0 => tile
    case 1 => rotate(tile)
    case 2 => rotate(rotate(tile))
    case 3 => rotate(rotate(rotate(tile)))
    case 4 => flip(tile)
    case 5 => rotate(flip(tile))
    case 6 => rotate(rotate(flip(tile)))
    case 7 => rotate(rotate(rotate(flip(tile))))
  }

  val orientedTiles: Array[Array[Array[Array[Char]]]] = {
    val init = arrangedTiles.updated(0, arrangedTiles(0).updated(0, orientNwTile))
    val len = init.head.length

    def getWestBoarder(tile: Array[Array[Char]]) = tile.map(_.head)

    def getEastBoarder(tile: Array[Array[Char]]) = tile.map(_.last)

    def getNorthBoarder(tile: Array[Array[Char]]) = tile.head

    def getSouthBoarder(tile: Array[Array[Char]]) = tile.last

    @tailrec
    def rotateUntilMatch(tile: Array[Array[Char]], idx: Int, whichBoarder: String, matchPattern: Array[Char]): Array[Array[Char]] = {
      val current = rotateByIdx(tile, idx)
      val tileBoarder = whichBoarder match {
        case "north" => getNorthBoarder(current)
        case "east" => getEastBoarder(current)
        case "south" => getSouthBoarder(current)
        case "west" => getWestBoarder(current)
      }

      if (tileBoarder sameElements matchPattern) current
      else rotateUntilMatch(tile, idx + 1, whichBoarder, matchPattern)
    }

    @tailrec
    def recurse(rowIdx: Int, colIdx: Int, res: Array[Array[Array[Array[Char]]]]): Array[Array[Array[Array[Char]]]] =
      if (rowIdx == len && colIdx == 0) res
      else if (colIdx == len) recurse(rowIdx + 1, 0, res)
      else if (rowIdx == 0) {
        val matchTile = res(rowIdx)(colIdx - 1)
        val matchBoarder = getEastBoarder(matchTile)
        val curTile = res(rowIdx)(colIdx)
        val curTileBoarder = getWestBoarder(curTile)

        if (matchBoarder sameElements curTileBoarder) recurse(rowIdx, colIdx + 1, res)
        else {
          val tileUpdate = rotateUntilMatch(curTile, 0, "west", matchBoarder)
          val rowUpdate = res(rowIdx).updated(colIdx, tileUpdate)
          val resUpdate = res.updated(rowIdx, rowUpdate)
          recurse(rowIdx, colIdx + 1, resUpdate)
        }
      }
      else {
        val matchTile = res(rowIdx - 1)(colIdx)
        val matchBoarder = getSouthBoarder(matchTile)
        val curTile = res(rowIdx)(colIdx)
        val curTileBoarder = getNorthBoarder(curTile)

        if (matchBoarder sameElements curTileBoarder) recurse(rowIdx, colIdx + 1, res)
        else {
          val tileUpdate = rotateUntilMatch(curTile, 0, "north", matchBoarder)
          val rowUpdate = res(rowIdx).updated(colIdx, tileUpdate)
          val resUpdate = res.updated(rowIdx, rowUpdate)
          recurse(rowIdx, colIdx + 1, resUpdate)
        }
      }

    recurse(0, 1, init)
  }

  val withoutBoarders: Array[Array[Array[Array[Char]]]] =
    orientedTiles.map(_.map(t => t.drop(1).dropRight(1).map(_.drop(1).dropRight(1))))

  @tailrec
  def coalesceColumns(tiles: Array[Array[Array[Char]]], res: Array[Array[Char]]): Array[Array[Char]] =
    if (tiles.isEmpty) res
    else coalesceColumns(tiles.tail, res.zip(tiles.head).map { case(acc, chars) => acc ++ chars })

  val sea: Array[Array[Char]] = withoutBoarders.flatMap(tiles => coalesceColumns(tiles, Array.fill(12)(Array.empty)))

  val seaMonsterTop: Regex = "(..................#.)".r
  val seaMonsterMid: Regex = "(#....##....##....###)".r
  val seaMonsterBottom: Regex = "(.#..#..#..#..#..#...)".r

  def findSeaMonsters(sea: Array[Array[Char]]): Map[Int, Array[Int]] = {
    @tailrec
    def findSection(idx: Int, pattern: Regex, res: Array[(Int, Int)]): Array[(Int, Int)] =
      if (idx == sea.length) res
      else {
        val curRes = sea(idx).sliding(20).flatMap(subArr => subArr.mkString match {
          case pattern(_) => Option(idx, sea(idx).indexOfSlice(subArr))
          case _ => None
        })

        if (curRes.nonEmpty) findSection(idx + 1, pattern, res ++ curRes)
        else findSection(idx + 1, pattern, res)
      }

    def toMap(res: Array[(Int, Int)]) = res.foldRight[Map[Int, Array[Int]]](Map.empty) {
      case ((keyRowIdx, colIdx), acc) => acc.updatedWith(keyRowIdx) {
        case Some(values) =>  Option(values :+ colIdx)
        case None =>  Option(Array(colIdx))
      }
    }
    val topSections = toMap(findSection(0, seaMonsterTop, Array.empty))
    val midSections = toMap(findSection(0, seaMonsterMid, Array.empty))
    val bottomSections = toMap(findSection(0, seaMonsterBottom, Array.empty))

    val midBottomMatches = bottomSections.keySet.map(_ - 1).intersect(midSections.keySet)

    val midMatchMap = midBottomMatches.map(mid => mid -> midSections(mid)).toMap
    val bottomMatchMap = midBottomMatches.map(bottom => bottom -> bottomSections(bottom + 1)).toMap
    val matchColumnMap = midBottomMatches.toArray.map(row => {
      val midCols = midMatchMap(row)
      val bottomCols = bottomMatchMap(row)
      row -> midCols.intersect(bottomCols)
    }).toMap
    val intersectTop = matchColumnMap.foldRight[Map[Int, Array[Int]]](Map.empty) {
      case((rowIdx, midBottomCols), acc) => topSections.get(rowIdx - 1) match {
        case Some(topCols) => acc + (rowIdx -> midBottomCols.intersect(topCols))
        case None => acc
    }}

//    println("midSections " + midSections.keySet)
//    println("====================================")
//    println("bottomSections " + bottomSections.keySet)
//    println("====================================")
//    println("matchColumns " + matchColumnMap)
//    println("intersectTop " + intersectTop.map(a=>(a._1, a._2.toList)))

    intersectTop
  }

  //  lazy val hasSeaMonMidPos0 = findSeaMonster(0, sea.transpose)
  //  lazy val hasSeaMonMidPos1 = findSeaMonster(0, rotateByIdx(sea, 1))
  //  lazy val hasSeaMonMidPos2 = findSeaMonster(0, rotateByIdx(sea, 2))
  val hasSeaMonMidPos3 = findSeaMonsters(rotateByIdx(sea, 3))
  //  lazy val hasSeaMonMidPos4 = findSeaMonster(0, rotateByIdx(sea, 4))
  //  lazy val hasSeaMonMidPos5 = findSeaMonster(0, rotateByIdx(sea, 5))
  //  lazy val hasSeaMonMidPos6 = findSeaMonster(0, rotateByIdx(sea, 6))
  //  lazy val hasSeaMonMidPos7 = findSeaMonster(0, rotateByIdx(sea, 7))

  def updateTopSection(row: Array[Char], offset: Int): Array[Char] =
    row.updated(18 + offset, 'S')

  def updateMidSection(row: Array[Char], offset: Int): Array[Char] =
    row.updated(0 + offset, 'S')
      .updated(5 + offset, 'S')
      .updated(6 + offset, 'S')
      .updated(11 + offset, 'S')
      .updated(12 + offset, 'S')
      .updated(17 + offset, 'S')
      .updated(18 + offset, 'S')
      .updated(19 + offset, 'S')

  def updateBottomSection(row: Array[Char], offset: Int): Array[Char] =
    row.updated(1 + offset, 'S')
      .updated(4 + offset, 'S')
      .updated(7 + offset, 'S')
      .updated(10 + offset, 'S')
      .updated(13 + offset, 'S')
      .updated(16 + offset, 'S')

  def replaceSeaMonster(monsters: Map[Int, Array[Int]]): Array[Array[Char]] =
    monsters.foldRight(rotateByIdx(sea, 3)) { case ((rowIdx, cols), acc) =>
      cols.foldRight(acc) { case (col, rowAcc) =>
        val topUpdate = updateTopSection(rowAcc(rowIdx - 1), col)
        val midUpdate = updateMidSection(rowAcc(rowIdx), col)
        val bottomUpdate = updateBottomSection(rowAcc(rowIdx + 1), col)
        rowAcc.updated(rowIdx - 1, topUpdate).updated(rowIdx, midUpdate).updated(rowIdx + 1, bottomUpdate)
      }
    }

  val seaMonsters = findSeaMonsters(rotateByIdx(sea, 3))

  val res2 = replaceSeaMonster(seaMonsters).flatten.count(_ == '#')

}
