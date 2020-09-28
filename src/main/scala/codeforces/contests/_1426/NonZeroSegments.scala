package codeforces.contests._1426

/*
https://codeforces.com/contest/1426/problem/D

We find distinct segments with sum = 0, use Map to find SubArrays with zero sum, further optimise with binary search
 */
object NonZeroSegments {

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val arr = io.StdIn.readLine.split(" ").map(_.toLong)

    val prefix = arr.scanLeft(0L)(_ + _)
    val groups = collection.mutable.Map((0 to n).groupBy(prefix).toSeq: _*)
    val result = {
      (1 to n).foldLeft((0, 0)) { case ((acc, lastIndex), i) =>

        val sum = prefix(i)
        val indices = groups.getOrElse(sum, IndexedSeq.empty)

        val x = lastIndex - 1
        val y = i - 1

        @scala.annotation.tailrec
        def binarySearch(l: Int, r: Int): Boolean = {
          if (l > r) false else {
            val m = (l + r) / 2
            val key = indices(m)
            if (x <= key && key <= y) true
            else if (key < x) binarySearch(m + 1, r)
            else binarySearch(l, m - 1)
          }
        }

        if (binarySearch(0, indices.length - 1))
          (acc + 1, i)
        else
          (acc, lastIndex)
      }._1
    }
    println(result)
  }
}
