package google.kickstart._2020.A

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ffc7/00000000001d3f5b

[Binary search]

Find all differences, sort, then search through the range 1 to max diff, minimum diff value for which #breaks <= k
 */
object Workout {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(_, k) = io.StdIn.readLine.split(" ").map(_.toInt)
      val arr = io.StdIn.readLine.split(" ").sliding(2).map { case Array(a, b) => b.toInt - a.toInt }.toArray.sorted


      def countBreaks(diff: Int): Int =
        arr.indices.foldLeft(0) { (acc, i) => acc + (arr(i) - 1) / diff }

      @scala.annotation.tailrec
      def binarySearch(l: Int, r: Int): Int = {
        if (l == r) l
        else {
          val m = (l + r) / 2
          val b = countBreaks(m)

          if (b <= k)
            binarySearch(l, m)
          else // b > k
            binarySearch(m + 1, r)
        }
      }

      printFormattedOutput(t, binarySearch(1, arr.last))
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }

}
