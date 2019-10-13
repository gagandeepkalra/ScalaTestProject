package codeforces.contest1175

/*
https://codeforces.com/contest/1175/problem/D

Strategy-

s = suffix sum array
f = indexes array

** f(0) is always 0

this implies =>
  res = s(f(0)) - s(f(1)) + 2*(s(f(1)) - s(f(2))) + 3*(s(f(2)) - s(f(3))) + ...
  res = s(f(0)) + s(f(1)) + s(f(2)) + s(f(3)) + ...
  this means we select the top k max elements form the suffix sum array


 */
object ArraySplitting {

  def main(args: Array[String]): Unit = {
    val Array(n, k) = io.StdIn.readLine().split(" ").map(_.toInt)
    val seq = io.StdIn.readLine().split(" ").map(_.toLong)

    val suffixSum = seq.scanRight(0l)(_ + _)
    println(suffixSum(0) + suffixSum.drop(1).dropRight(1).sorted.takeRight(k - 1).sum)
  }

}