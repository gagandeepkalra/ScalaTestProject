package codeforces.contests._1462

/**
  * https://codeforces.com/contest/1462/problem/E1
  * You are given a sequence 𝑎 of length 𝑛 consisting of integers from 1 to 𝑛. The sequence may contain duplicates (i.e. some elements can be equal).
  *
  * Find the number of tuples of 𝑚 elements such that the maximum number in the tuple differs from the minimum by no more than 𝑘. Formally,
  * you need to find the number of tuples of 𝑚 indices 𝑖1<𝑖2<…<𝑖𝑚, such that
  *
  * max(𝑎𝑖1,𝑎𝑖2,…,𝑎𝑖𝑚)−min(𝑎𝑖1,𝑎𝑖2,…,𝑎𝑖𝑚)≤𝑘.
  *
  * [𝑥, 𝑥+1, 𝑥+2];
  * [𝑥, 𝑥+1, 𝑥+1];
  * [𝑥, 𝑥+2, 𝑥+2];
  * [𝑥, 𝑥, 𝑥+1];
  * [𝑥, 𝑥, 𝑥+2];
  * [𝑥, 𝑥, 𝑥].
  *
  * 𝑐𝑛𝑡[𝑥]⋅𝑐𝑛𝑡[𝑥+1]⋅𝑐𝑛𝑡[𝑥+2];
  * 𝑐𝑛𝑡[𝑥]⋅𝑐𝑛𝑡[𝑥+1]⋅(𝑐𝑛𝑡[𝑥+1]−1)2;
  * 𝑐𝑛𝑡[𝑥]⋅𝑐𝑛𝑡[𝑥+2]⋅(𝑐𝑛𝑡[𝑥+2]−1)2;
  * 𝑐𝑛𝑡[𝑥]⋅(𝑐𝑛𝑡[𝑥]−1)2⋅𝑐𝑛𝑡[𝑥+1];
  * 𝑐𝑛𝑡[𝑥]⋅(𝑐𝑛𝑡[𝑥]−1)2⋅𝑐𝑛𝑡[𝑥+2];
  * 𝑐𝑛𝑡[𝑥]⋅(𝑐𝑛𝑡[𝑥]−1)⋅(𝑐𝑛𝑡[𝑥]−2)6.
  *
  */
object CloseTuples1 {

  def function(frequency: Int => Long): Int => Long =
    Seq(
      (k: Int) => frequency(k) * frequency(k + 1) * frequency(k + 2),
      (k: Int) => frequency(k) * (frequency(k + 1) * (frequency(k + 1) - 1)) / 2,
      (k: Int) => frequency(k) * (frequency(k + 2) * (frequency(k + 2) - 1)) / 2,
      (k: Int) => ((frequency(k) * (frequency(k) - 1)) / 2) * frequency(k + 1),
      (k: Int) => ((frequency(k) * (frequency(k) - 1)) / 2) * frequency(k + 2),
      (k: Int) => frequency(k) * (frequency(k) - 1) * (frequency(k) - 2) / 6
    ).foldLeft((_: Int) => 0L)((accF, f) => (k: Int) => accF(k) + f(k))

  def main(args: Array[String]): Unit = {
    println({
      for (_ <- 0 until io.StdIn.readInt) yield {
        val n = io.StdIn.readInt()

        val a = io.StdIn.readLine.split(" ").map(_.toInt)

        val frequency = new Array[Long](n + 3)
        a.foreach(frequency(_) += 1)

        val f = function(frequency)

        (1 to n).foldLeft(0L)(_ + f(_))
      }
    }.mkString("\n"))
  }
}
