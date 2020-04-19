package google.kickstart._2020.B

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ffc8/00000000002d83bf

[Array]

fold right
 */
object BusRoutes {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, d) = io.StdIn.readLine.split(" ").map(_.toLong)
      val arr = io.StdIn.readLine.split(" ").map(_.toLong)

      val result = arr.foldRight(d) { (i, day) => day - day % i }

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }

}
