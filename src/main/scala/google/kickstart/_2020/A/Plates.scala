package google.kickstart._2020.A

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ffc7/00000000001d40bb

[Dynamic Programming]

bottom up considering only one selection at each level, dp(n, p) = given n levels and p total choices
 */
object Plates {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, k, p) = io.StdIn.readLine.split(" ").map(_.toInt)
      val arr = Array.fill(n)(io.StdIn.readLine.split(" ").map(_.toInt).scanLeft(0)(_ + _))

      val dp = Array.ofDim[Int](n, p + 1)
      for {
        k <- 0 to (k min p)
      } dp(0)(k) = arr(0)(k)

      for {
        n <- 1 until n
        p <- 0 to p
        k <- 0 to (k min p)
      } {
        dp(n)(p) = dp(n)(p) max (dp(n - 1)(p - k) + arr(n)(k))
      }

      printFormattedOutput(t, dp(n - 1)(p))
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }
}
