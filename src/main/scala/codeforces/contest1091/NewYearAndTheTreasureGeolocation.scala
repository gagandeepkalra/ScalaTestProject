package codeforces.contest1091

import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.StringTokenizer

object NewYearAndTheTreasureGeolocation {

  val br = new BufferedReader(new InputStreamReader(System.in))

  def readInt(): Int = {
    val st = new StringTokenizer(br.readLine())
    st.nextToken().toInt
  }

  def readLongPair(): (Long, Long) = {
    val st = new StringTokenizer(br.readLine())
    (st.nextToken.toLong, st.nextToken.toLong)
  }

  def addIntPairs(p1: (Long, Long), p2: (Long, Long)): (Long, Long) = (p1._1 + p2._1, p1._2 + p2._2)

  def main(args: Array[String]): Unit = {
    val n = readInt()
    val sum = (0 until 2 * n).foldLeft((0l, 0l))((acc, _) => addIntPairs(acc, readLongPair()))
    println(sum._1 / n + " " + sum._2 / n)
  }

}
