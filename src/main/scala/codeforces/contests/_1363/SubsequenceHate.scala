package codeforces.contests._1363

/*
https://codeforces.com/contest/1363/problem/B

Sweep a partition across.
 */
object SubsequenceHate {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val s = io.StdIn.readLine()

      val ones = {
        val arr = new Array[Int](s.length)
        arr(0) = if (s(0) == '1') 1 else 0
        (1 until s.length).foreach(i => arr(i) = (if (s(i) == '1') 1 else 0) + arr(i - 1))
        arr
      }

      def onesInRange(x: Int, y: Int): Int =
        if (x > y) 0 else if (x == 0) ones(y) else ones(y) - ones(x - 1)

      def zeroesInRange(x: Int, y: Int): Int =
        if (x > y) 0 else y - x + 1 - onesInRange(x, y)

      def makeZeroCost(x: Int, y: Int): Int = onesInRange(x, y)

      def makeOneCost(x: Int, y: Int): Int = zeroesInRange(x, y)

      val last = s.length - 1

      println {
        (0 to last).foldLeft(Int.MaxValue)((acc, i) =>
          acc min (makeOneCost(0, i) + makeZeroCost(i + 1, last)) min (makeZeroCost(0, i) + makeOneCost(i + 1, last)))
      }
    }
  }

}
