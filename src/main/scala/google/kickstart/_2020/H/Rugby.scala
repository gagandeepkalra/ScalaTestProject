package google.kickstart._2020.H

object Rugby {
  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()

      val coords = (1 to n).map { _ =>
        io.StdIn.readLine.split(" ").map(_.toInt)
      }

      val ys = coords.map(_(1)).sorted

      val yResult = {
        val median = ys(n / 2)
        ys.foldLeft(0L)((acc, v) => acc + (v - median).abs)
      }

      val xs = coords.map(_(0)).sorted

      val xResult = {
        val xss = (0 until n).map(i => xs(i) - i).sorted // start positions
        val median = xss(n/2)
        xss.foldLeft(0L)((acc, v) => acc + (v - median).abs) // move everyone's start position to same
      }

      printFormattedOutput(t, xResult + yResult)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
