package codeforces.contest1197

/*
https://codeforces.com/contest/1197/problem/D

cost = Sum(l, r) − 𝑘 * ⌈(𝑟−𝑙+1)/𝑚⌉, maximise

For each segment of size m, calculate how much farther back we can go (best(i)) (positivity), and then
compare maximum result after including each of m single elements to the right

 */
object YetAnotherSubarrayProblem {

  type Index = Int

  def main(args: Array[String]): Unit = {
    val Array(n, m, k) = io.StdIn.readLine().split(" ").map(_.toInt)
    val elements = io.StdIn.readLine().split(" ").map(_.toInt)

    val prefixSum = elements.scanLeft(0l)(_ + _).tail

    def sum(l: Int, r: Int): Long = prefixSum(r) - (if (l > 0) prefixSum(l - 1) else 0)

    val best = new Array[Long](n)

    val result = (0 until n).foldLeft(0l) { case (res, i) =>
      best(i) = (sum(i - m + 1, i) - k + (if (i - m >= 0) best(i - m) else 0)) max 0
      val maxAhead = (1 to m).filter(_ + i < n).foldLeft(res)((newRes, j) => newRes max (best(i) + sum(i + 1, i + j) - k))
      res max best(i) max maxAhead
    }

    println(result)


  }

}
