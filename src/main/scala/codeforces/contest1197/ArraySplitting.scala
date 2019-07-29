package codeforces.contest1197

/*
https://codeforces.com/contest/1197/problem/C
 */
object ArraySplitting {

  def main(args: Array[String]): Unit = {
    val Array(n, k) = io.StdIn.readLine().split(" ").map(_.toInt)
    println(
      if (n == 1) {
        io.StdIn.readInt()
        0
      }
      else {
        io.StdIn
          .readLine()
          .split(" ")
          .map(_.toInt)
          .sliding(2)
          .map(arr => arr(1) - arr(0))
          .toArray
          .sorted
          .slice(0, n - k)
          .sum
      }
    )
  }
}
