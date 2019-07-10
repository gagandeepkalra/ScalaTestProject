package codeforces

/*
https://codeforces.com/contest/1175/problem/E

Given n ranges and  m queries in form [𝑥;𝑦]. What is the minimal number of intervals you have to take so that every point
(not necessarily integer) from 𝑥 to 𝑦 is covered by at least one of them?

We do this `Path Compression` style, point every no. in range to an interval and keep updating that interval with the farthest range.
We answer queries offline sorted by R values.

Many points correspond to one single interval, it cannot go functional this way, introduce a middleware, the index
 */
object MinimalSegmentCover {

  type Index = Int
  type L = Int
  type R = Int
  type Result = Int

  val MAX = 500000

  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)

    val intervals: Map[L, R] = (0 until n)
      .map(_ => io.StdIn.readLine().split(" ").map(_.toInt))
      .collect { case Array(x, y) => (x, y) }
      .groupBy(_._1)
      .map { case (_, pairs) => pairs.maxBy(_._2) }

    val (intervalMap: Map[L, (R, Index)], indexToResultMap: Map[Index, (R, Result)]) = {
      val (intervalItr, indexToResultItr) = intervals.zipWithIndex.map { case ((l, r), index) => ((l, (r, index)), (index, (r, 1))) }.unzip
      (intervalItr.toMap, indexToResultItr.toMap)
    }

    val valueToRightmostIntervalsIndexMap: Map[Int, Index] =
      (0 to MAX).foldLeft(List[(Int, Index)]()) {
        case (ls, i) =>
          val current: Option[(R, Index)] = intervalMap.get(i)
          val prev: Option[(R, Index)] = ls.headOption.map(_._2).flatMap(index => indexToResultMap.get(index).map(p => (p._1, index)))

          current.flatMap { case c@(cr, _) => prev.map { case p@(pr, _) => if (cr >= pr) c else p } }
            .orElse(current)
            .orElse(prev)
            .collect { case (r, index) if r >= i => (i, index) :: ls }.getOrElse(ls)
      }.toMap


    val sortedQueriesByR: Seq[((L, R), Index)] = (0 until m)
      .map(_ => io.StdIn.readLine().split(" ").map(_.toInt))
      .collect { case Array(x, y) => (x, y) }
      .zipWithIndex
      .sortBy(_._1._2)

    def pathCompression(l: L, r: R, map: Map[Index, (R, Result)]): Map[Index, (R, Result)] = {
      valueToRightmostIntervalsIndexMap.get(l).flatMap(index => map.get(index).map(p => (index, p._1, p._2))) match {
        case Some((_, rightmost, _)) if rightmost >= r => map
        case Some((index, rightmost, steps)) if rightmost > l =>

          val newMap = pathCompression(rightmost, r, map)

          valueToRightmostIntervalsIndexMap.get(rightmost).flatMap(newMap.get)
            .collect { case (rightmostR, res) if r <= rightmostR => newMap + (index -> (rightmostR, steps + res)) }
            .getOrElse(newMap)

        case _ => map
      }
    }

    val resultMap = sortedQueriesByR.foldLeft(List[(Index, Int)](), indexToResultMap) {
      case ((resList, map), ((ql, qr), qi)) =>

        val newMap = pathCompression(ql, qr, map)
        val newResList = valueToRightmostIntervalsIndexMap.get(ql).flatMap(newMap.get)
          .collect { case (r, res) if r >= qr => (qi, res) :: resList }
          .getOrElse(resList)

        (newResList, newMap)

    }._1.toMap

    println((0 until m).map(index => resultMap.getOrElse(index, -1)).mkString("\n"))

  }

}
