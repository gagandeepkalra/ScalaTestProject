package codeforces

/*

http://codeforces.com/contest/1043/problem/A

 */
object Elections {

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val arr = io.StdIn.readLine.split(" ").map(_.toInt)

    val sum = arr.sum.toDouble
    val max = arr.max

    var result = (2 * sum) / n

    if (result == result.toInt) result += 1

    println(result.ceil.toInt max max)
  }

}
