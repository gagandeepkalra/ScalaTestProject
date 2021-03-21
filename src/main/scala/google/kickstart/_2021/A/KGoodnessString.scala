package google.kickstart._2021.A

/**
 * https://codingcompetitions.withgoogle.com/kickstart/round/0000000000436140/000000000068cca3
 */
object KGoodnessString {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
      val string      = io.StdIn.readLine

      val goodnessScore = (0 until n / 2).count(i => string(i) != string(n - 1 - i))

      printFormattedOutput(t, (goodnessScore - k).abs)
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }
}
