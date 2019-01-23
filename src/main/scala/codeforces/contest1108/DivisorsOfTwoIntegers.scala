package codeforces.contest1108

/*
http://codeforces.com/contest/1108/problem/B
 */
object DivisorsOfTwoIntegers {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val arr = io.StdIn.readLine.split(" ").map(_.toInt).sorted.reverse
    val first = arr.head

    println(first + " " + arr.tail.find(i => first % i != 0).orElse(arr.sliding(2).find(a => a(0) == a(1)).map(_.head)).get)
  }
}
