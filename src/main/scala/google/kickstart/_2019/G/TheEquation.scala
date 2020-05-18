package google.kickstart._2019.G

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050e02/000000000018fe36

[Array]

we reduce all numbers to there set bit counters. Now to find k we start at the most significant bit and do best effort to
include a set bit for k. For this we use an estimate for the rest of the bits to not go over m. Linear time.
 */
object TheEquation {

  def main(args: Array[String]): Unit = {

    for (t <- 1 to io.StdIn.readInt()) {
      val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toLong)

      val setBits = new Array[Long](51)
      io.StdIn.readLine.split(" ").foreach { s =>
        val l = s.toLong
        for (i <- 0 until 51) setBits(i) += (l >> i) & 1
      }

      val minCostArray: Array[Long] = setBits.zipWithIndex
        .map { case (v, i) => (v min n - v) * (1L << i) }
        .scanLeft(0L)(_ + _)
        .tail

      @scala.annotation.tailrec
      def calculateK(i: Int, k: Long, acc: Long): Long = {
        if (i < 0) k
        else {
          val ones = setBits(i)
          val zeroes = n - ones

          val minCost = if (i >= 1) minCostArray(i - 1) else 0L

          if (acc + zeroes * (1L << i) + minCost <= m)
            calculateK(i - 1, k + (1L << i), acc + zeroes * (1L << i))
          else
            calculateK(i - 1, k, acc + ones * (1L << i))
        }
      }

      println {
        val result = if (minCostArray.last > m) -1 else calculateK(50, 0L, 0L)
        s"Case #$t: $result"
      }

    }
  }
}
