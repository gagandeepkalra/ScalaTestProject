package google.kickstart._2020.C

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff43/00000000003381cb

store prefix count for each, then for each i add to answer count of sub-arrays of size j*j ending at that position

optimisations: we use array for frequency counting instead of Map
 */
object PerfectSubarray {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val a = io.StdIn.readLine.split(" ").map(_.toInt)

      val (maxN, maxP) = {
        var maxN, maxP = 0
        for (i <- 0 until n) if (a(i) < 0) maxN -= a(i) else maxP += a(i)
        (maxN, maxP)
      }

      val result = {
        val frequency = new Array[Long](maxN + maxP + 1) // maxN be the offset
        frequency(maxN) = 1 // empty prefix, maxN is the zero

        (0 until n).foldLeft((0, 0L)) { case ((prefix, ans), i) =>
          val uPrefix = prefix + a(i)

          @scala.annotation.tailrec
          def loop(j: Int, acc: Long = 0): Long =
            if (j * j <= maxP && maxN + uPrefix - j * j >= 0)
              loop(j + 1, acc + frequency(maxN + uPrefix - j * j))
            else
              acc

          val uAns = ans + loop(0)

          frequency(maxN + uPrefix) += 1 // we include afterwards as we want non empty sub arrays

          (uPrefix, uAns)
        }._2
      }

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }
}
