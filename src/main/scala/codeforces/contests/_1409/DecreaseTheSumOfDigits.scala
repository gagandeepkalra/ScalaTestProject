package codeforces.contests._1409

/*
https://codeforces.com/contest/1409/problem/D

we start incrementing from right, making each digit equal to zero, send carry along
 */
object DecreaseTheSumOfDigits {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val Array(n, g) = io.StdIn.readLine.split(" ").map(_.toLong)

      val digits = n.toString.map(_ - '0')

      @scala.annotation.tailrec
      def loop(i: Int = digits.indices.last, carry: Int = 0, s: Int = digits.sum): Long = {
        if (i < 0) ("1" + "0" * digits.length).toLong - n
        else if (s <= g) ((digits.slice(0, i + 1).mkString("").toLong + carry).toString + "0" * (digits.length - 1 - i)).toLong - n
        else loop(i - 1, if (carry == 0) 1 else 1, s - digits(i) + (if (carry == 0) 1 else 0))
      }

      println(loop())
    }
  }
}
