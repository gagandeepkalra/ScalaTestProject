package google.kickstart._2020.C

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff43/00000000003380d2

[Array]
 */
object Countdown {
  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
      val arr = io.StdIn.readLine.split(" ").map(_.toInt)

      val kIndexes = arr.indices.filter(arr(_) == k)

      @scala.annotation.tailrec
      def doesMatch(i: Int, value: Int): Boolean =
        value < 1 || i < n && arr(i) == value && doesMatch(i + 1, value - 1)

      val result = kIndexes.count(i => doesMatch(i + 1, k - 1))

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }
}
