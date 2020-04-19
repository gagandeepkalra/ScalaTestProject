package google.kickstart._2020.B

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ffc8/00000000002d83dc

[Permutations, Combinations and Probabilities]

Given a grid with a hole find probability for reaching the other end

Answer is finding the probability to reach the diagonal from the opposite corners of the hole

We use logarithms for large nCr.

 */
object WanderingRobot {
  import math.{exp, log}

  val maxN = 100000

  val logOfFatorial: Array[Double] = {
    val arr = new Array[Double](2 * maxN + 1)
    arr(0) = 0
    for (i <- 1 until arr.length) arr(i) = arr(i - 1) + log(i)
    arr
  }

  def logNcr(n: Int, r: Int): Double = logOfFatorial(n) - logOfFatorial(r) - logOfFatorial(n - r)

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(y, x, b, a, d, c) = io.StdIn.readLine.split(" ").map(_.toInt)

      def probability(r: Int, c: Int): Double =
        if (r == x)
          (1 to c).map(probability(r - 1, _)).sum * 0.5
        else if (c == y)
          (1 to r).map(probability(_, c - 1)).sum * 0.5
        else
          exp(logNcr(r + c - 2, r - 1) + (r + c - 2) * log(0.5))

      @scala.annotation.tailrec
      def takeWhile(r: Int, rF: Int => Int, c: Int, cF: Int => Int, acc: Double = 0.0): Double =
        if (1 <= r && r <= x && 1 <= c && c <= y) {
          takeWhile(rF(r), rF, cF(c), cF, acc + probability(r, c))
        } else acc

      printFormattedOutput(t, takeWhile(c + 1, _ + 1, b - 1, _ - 1) + takeWhile(a - 1, _ - 1, d + 1, _ + 1))
    }
  }

  def printFormattedOutput(i: Int, op: Double): Unit = {
    println(f"Case #$i: $op%.6f")
  }
}
