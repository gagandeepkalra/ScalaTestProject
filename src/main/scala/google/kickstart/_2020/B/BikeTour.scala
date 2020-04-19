package google.kickstart._2020.B

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ffc8/00000000002d82e6

Array

Count hills
 */
object BikeTour {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val arr = io.StdIn.readLine.split(" ").map(_.toInt)

      val result = (1 until n - 1).count(i => arr(i - 1) < arr(i) && arr(i) > arr(i + 1))

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Int): Unit = {
    println(s"Case #$i: $op")
  }
}
