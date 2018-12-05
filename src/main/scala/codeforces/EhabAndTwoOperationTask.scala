package codeforces

import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.StringTokenizer


/*

https://codeforces.com/contest/1088/problem/C

 */
object EhabAndTwoOperationTask {

  val br = new BufferedReader(new InputStreamReader(System.in))

  def readIntArray(n: Int): Array[Int] = {
    val st = new StringTokenizer(br.readLine())

    val arr = new Array[Int](n)
    var i = 0
    while (st.hasMoreElements) {
      arr(i) = st.nextToken().toInt
      i += 1
    }

    arr
  }

  def readInt(): Int = {
    new StringTokenizer(br.readLine()).nextToken.toInt
  }

  def main(args: Array[String]): Unit = {
    val n = readInt()
    val input: Array[Int] = readIntArray(n)
    val initialIncrement = 3 * (input.length max input.max)

    println((1 to n).foldLeft(new StringBuilder().append(n+1).append('\n').append(s"1 $n $initialIncrement\n"))((acc, i) => acc.append(s"2 $i " + (initialIncrement + input(i-1) - i) + "\n")))
  }
}
