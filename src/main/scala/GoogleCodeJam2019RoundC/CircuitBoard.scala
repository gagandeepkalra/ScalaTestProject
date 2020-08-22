package GoogleCodeJam2019RoundC

import algorithms.arrays.Stack.largestRectangleInAHistogram
import algorithms.range.SegmentTree

object CircuitBoard {

  def main(args: Array[String]): Unit = {
    val testCases = io.StdIn.readInt()

    for (t <- 1 to testCases) {

      val Array(rows, cols, k) = io.StdIn.readLine().split(" ").map(_.toInt)

      val grid = (0 until rows).map(_ => io.StdIn.readLine.split(" ").map(_.toInt))

      val minSegmentTrees = (0 until rows).map(r => SegmentTree[Int](grid(r), (_: Int) min (_: Int)))

      val maxSegmentTrees = (0 until rows).map(r => SegmentTree[Int](grid(r), (_: Int) max (_: Int)))

      def findMaxDifferenceForRow(row: Int)(l: Int, r: Int): Int = {
        maxSegmentTrees(row).query(l, r) - minSegmentTrees(row).query(l, r)
      }

      /**
       *
       * @param row row index
       * @param col column index
       * @param l   binarySearch left index
       * @param r   binarySearch right index
       * @return maximum right we can go from (row, col) inclusive
       */
      @scala.annotation.tailrec
      def binarySearch(row: Int, col: Int)(l: Int, r: Int): Int = {
        if (l > r)
          l - col
        else {
          val m = (l + r) / 2
          val midResult = findMaxDifferenceForRow(row)(col, m)

          if (midResult <= k)
            binarySearch(row, col)(m + 1, r)
          else
            binarySearch(row, col)(l, m - 1)
        }
      }

      val gridWithRightView = (0 until rows).map(r => (0 until cols).map(c => binarySearch(r, c)(c, cols - 1)))

      val result = (0 until cols).map(c => largestRectangleInAHistogram((0 until rows).map(r => gridWithRightView(r)(c)))).max

      println(s"Case #$t: $result")
    }
  }

}
