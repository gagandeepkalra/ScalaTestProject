package codeforces

import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.StringTokenizer

/*

https://codeforces.com/contest/1043/problem/E

 */
object TrainHardWinEasy {

  val br = new BufferedReader(new InputStreamReader(System.in))

  def readLongPair(): (Long, Long) = {
    val st = new StringTokenizer(br.readLine())
    (st.nextToken.toLong, st.nextToken.toLong)
  }

  def readIntPair(): (Int, Int) = {
    val st = new StringTokenizer(br.readLine())
    (st.nextToken.toInt, st.nextToken.toInt)
  }

  def main(args: Array[String]): Unit = {
    val (n, m) = readIntPair()
    val rangeN = 0 until n
    val rangeM = 0 until m

    val scores = new Array[(Long, Long, Long, Int)](n)

    rangeN.foreach(i => scores(i) = {
      val (x, y) = readLongPair()
      (x, y, y - x, i)
    })

    val indexWhenSorted = new Array[Int](n)
    val scoresSorted = scores.sortBy(_._3)

    rangeN.foreach(i => indexWhenSorted(scoresSorted(i)._4) = i)

    val prefixSum = new Array[(Long, Long)](n)
    prefixSum(0) = (scoresSorted(0)._1, scoresSorted(0)._2)
    (1 until n).foreach(i => prefixSum(i) = (scoresSorted(i)._1 + prefixSum(i - 1)._1, scoresSorted(i)._2 + prefixSum(i - 1)._2))

    val enmitites = Array.fill[List[Int]](n)(List())

    rangeM.foreach { _ =>
      val (x, y) = readIntPair()
      enmitites(x - 1) = (y - 1) :: enmitites(x - 1)
      enmitites(y - 1) = (x - 1) :: enmitites(y - 1)
    }

    println(rangeN.foldLeft(new StringBuilder())((sb, i) => sb.append(compute(i)).append(" ")).toString())


    def compute(i: Int): Long = {
      val sortedIdx = indexWhenSorted(i)

      var result = 0l

      if (sortedIdx > 0) {
        result += scores(i)._1 * sortedIdx.toLong + prefixSum(sortedIdx - 1)._2
      }

      result += (prefixSum(n - 1)._1 - prefixSum(sortedIdx)._1) + scores(i)._2 * (n - 1 - sortedIdx)

      enmitites(i).foreach(idx => result -= scores(i)._1 + scores(idx)._2 min scores(i)._2 + scores(idx)._1)

      result
    }
  }

}
