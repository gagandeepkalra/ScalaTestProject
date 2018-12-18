package codeforces

import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.StringTokenizer

/*

https://codeforces.com/contest/1043/problem/F
minimum set such that gcd is one

 */
object MakeItOne {

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
    val st = new StringTokenizer(br.readLine())
    st.nextToken.toInt
  }

  def power(x: Long, y: Long): Long = {
    if (x == 0) 0
    else {
      var ans: Long = 1
      var temp: Long = x

      var i = y
      while (i > 0) {
        if ((i & 1) == 1) {
          ans = (ans * temp) % MOD
        }
        temp = (temp * temp) % MOD
        i = i / 2
      }

      ans
    }
  }

  val maxCount = 300001
  val MOD = 1000000009

  val factorialOf: Array[Long] = new Array[Long](maxCount)
  factorialOf(0) = 1
  for (i <- 1 until maxCount) factorialOf(i) = (i * factorialOf(i - 1)) % MOD

  val inverseModOfFactorial: Array[Long] = new Array[Long](maxCount)
  inverseModOfFactorial(maxCount - 1) = power(factorialOf(maxCount - 1), MOD - 2)

  for (i <- maxCount - 1 to 1 by -1) inverseModOfFactorial(i - 1) = (i * inverseModOfFactorial(i)) % MOD

  def nCr(n: Int, r: Int): Long = {
    if (r < 0 || n < r) 0
    else ((factorialOf(n) * inverseModOfFactorial(r) % MOD) * inverseModOfFactorial(n - r)) % MOD
  }

  def sub(x: Long, y: Long): Long = {
    x - y + (if (x >= y) 0 else MOD)
  }

  def main(args: Array[String]): Unit = {
    val n = readInt()
    val arr = readIntArray(n)

    val isPresent = new Array[Boolean](maxCount)
    arr.foreach(i => isPresent(i) = true)

    val countOfElementsThatDivide = new Array[Int](maxCount)
    for {
      i <- 1 until maxCount
      j <- i until maxCount by i if isPresent(j)
    } countOfElementsThatDivide(i) += 1

    val dp = Array.ofDim[Long](8, maxCount)
    for {
      i <- 1 to 7
      j <- maxCount - 1 to 1 by -1
    } {
      dp(i)(j) = nCr(countOfElementsThatDivide(j), i)
      var k = 2
      while (k * j < maxCount) {
        dp(i)(j) = sub(dp(i)(j), dp(i)(k * j))
        k += 1
      }

      if (dp(i)(1) > 0) {
        println(i)
        System.exit(0)
      }
    }

    println(-1)
  }
}
