package google.kickstart._2020.A

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ffc7/00000000001d3f56

[Array]

squeeze in as many within budget
 */
object Allocation {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, budget) = io.StdIn.readLine.split(" ").map(_.toInt)
      val arr = io.StdIn.readLine.split(" ").map(_.toInt).sorted

      val result = arr.foldLeft((budget, 0)) { case (acc@(b, r), i) => if (i <= b) (b - i, r + 1) else acc }._2

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }

}
