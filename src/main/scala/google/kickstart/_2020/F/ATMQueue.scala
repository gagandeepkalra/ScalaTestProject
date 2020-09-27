package google.kickstart._2020.F

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff48/00000000003f4ed8


Tokenize by batch sequence id, then sort to success.

Took a couple of attempts to get the tokenizing right
 */
object ATMQueue {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
      val arr = io.StdIn.readLine.split(" ").map(_.toInt)

      val result = arr.zip(1 to n).sortBy(p => if (p._1 % k == 0) (p._1 / k) - 1 else p._1 / k).map(_._2).mkString(" ")

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }

}
