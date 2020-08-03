package codeforces.contests._1374

/*
https://codeforces.com/contest/1374/problem/F

The problem is solvable only if there are even inversions, e.g. 312 -> 213 -> 123 as to 132 -> ???.
Now there's one additional possibility of duplicates, e.g. 2133 -> 3213 -> 1323 -> 1332 -> 1233.

We generate sequence just like bubble sort, moving the smallest value to the left in each iteration.
 */
object CyclicShiftsSorting {

  def countInversions(arr: Array[Int]): Int =
    arr.indices.foldLeft(0)((accI, i) =>
      accI + (i + 1 until arr.length).foldLeft(0)((accJ, j) =>
        accJ + (if (arr(i) > arr(j)) 1 else 0)))

  def main(args: Array[String]): Unit = {
    println({
      for (_ <- 1 to io.StdIn.readInt()) yield {
        val n = io.StdIn.readInt()
        val seq = io.StdIn.readLine().split(" ").map(_.toInt)

        val originalZipped = seq.zipWithIndex
        val sortedSeq = originalZipped.sortBy(_._1).toList

        def rotateRight(i: Int): Unit = {
          val a = originalZipped(i)
          val b = originalZipped(i + 1)
          val c = originalZipped(i + 2)

          originalZipped(i) = c
          originalZipped(i + 1) = a
          originalZipped(i + 2) = b
        }

        @scala.annotation.tailrec
        def loop(pos: Int, turn: List[(Int, Int)], stillOdd: Boolean, result: List[Int]): List[Int] = {
          turn match {
            case Nil if !stillOdd => result
            case Nil if stillOdd => Nil
            case f :: s :: tail if stillOdd && f._1 == s._1 =>
              val idS = originalZipped.indexOf(s)
              val tempR = (idS - 2 to pos by -2).foldLeft(result) { (acc, i) => rotateRight(i); (i + 1) :: acc }

              val newR = if (pos + 1 < n && originalZipped(pos + 1) == s) {
                rotateRight(pos)
                rotateRight(pos)
                pos + 1 :: pos + 1 :: tempR
              } else tempR
              loop(pos + 1, f :: tail, stillOdd = false, newR)

            case h :: tail =>
              val idV = originalZipped.indexOf(h)
              val tempR = (idV - 2 to pos by -2).foldLeft(result) { (acc, i) => rotateRight(i); (i + 1) :: acc }

              val newR = if (pos + 1 < n && originalZipped(pos + 1) == h) {
                rotateRight(pos)
                rotateRight(pos)
                pos + 1 :: pos + 1 :: tempR
              } else tempR
              loop(pos + 1, tail, stillOdd, newR)
          }
        }

        val areInversionsOdd = countInversions(seq) % 2 == 1
        val allDistinct = seq.distinct.length == n

        if (areInversionsOdd && allDistinct) "-1" else {
          val result = loop(0, sortedSeq, areInversionsOdd, Nil)
          s"${result.length}\n${result.reverse.mkString(" ")}"
        }
      }
    }.mkString("\n"))
  }
}
