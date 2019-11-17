package codeforces.contest1244

import scala.annotation.tailrec

/*
https://codeforces.com/contest/1244/problem/E
 */
object MinimizingDifference {

  def findCost(seq: Seq[Long]): Seq[Long] =
    seq.zipWithIndex
      .scanLeft(0L) {
        case (acc, (e, i)) =>
          acc + math.abs(e - (if (i > 0) seq(i - 1) else e)) * i
      }
      .tail

  def main(args: Array[String]): Unit = {
    val Array(n, k), elements = io.StdIn.readLine.split(" ").map(_.toLong)

    val sorted: Array[Long] = elements.sorted

    val forward: Seq[Long] = findCost(sorted)
    val backward: Seq[Long] = findCost(sorted.reverse)

    val leftResult = (0 until n.toInt).map { i =>
      val now = forward(i)
      val nowPlusOne = if (i + 1 == n) None else Some(forward(i + 1))
      val remaining = k - now
      if (remaining >= 0) {
        val indexOfValueGreaterThanRemaining =
          findCeiling(backward, remaining, 0, n.toInt - 1 - i)

        val other = backward(indexOfValueGreaterThanRemaining - 1)

        val weightLeft = i + 1
        val weightRight = indexOfValueGreaterThanRemaining

        val lIndex = i
        val rIndex = n.toInt - indexOfValueGreaterThanRemaining

        val subtraction = if (lIndex < rIndex) {
          val consideration = k - now - other
          if (weightLeft < weightRight) {
            nowPlusOne
              .map(nextToNow => (nextToNow - now).min(consideration))
              .map(
                leftUsed =>
                  leftUsed / weightLeft + (consideration - leftUsed + leftUsed % weightLeft) / weightRight
              )
              .getOrElse(consideration / weightRight)
          } else
            consideration / weightRight

        } else 0

        val ans: Long = math.abs(sorted(lIndex) - sorted(rIndex)) - subtraction
        ans max 0
      } else Long.MaxValue
    }.min

    val rightResult = (0 until n.toInt).map { i =>
      val now = backward(i)
      val nowPlusOne = if (i + 1 == n) None else Some(backward(i + 1))
      val remaining = k - now
      if (remaining >= 0) {
        val indexOfValueGreaterThanRemaining =
          findCeiling(forward, remaining, 0, n.toInt - 1 - i)

        val other = forward(indexOfValueGreaterThanRemaining - 1)

        val weightRight = i + 1
        val weightLeft = indexOfValueGreaterThanRemaining

        val rIndex = n.toInt - 1 - i
        val lIndex = indexOfValueGreaterThanRemaining - 1

        val subtraction = if (lIndex < rIndex) {
          val consideration = k - now - other
          if (weightRight < weightLeft) {
            nowPlusOne
              .map(nextToNow => (nextToNow - now).min(consideration))
              .map(
                rightUsed =>
                  rightUsed / weightRight + (consideration - rightUsed + rightUsed % weightRight) / weightLeft
              )
              .getOrElse(consideration / weightLeft)
          } else
            consideration / weightLeft
        } else 0

        val ans: Long = math.abs(sorted(lIndex) - sorted(rIndex)) - subtraction
        ans max 0
      } else Long.MaxValue
    }.min

    println { leftResult min rightResult }

  }

  /**
    * @return index of value greater than value
    */
  @tailrec
  def findCeiling(seq: Seq[Long], value: Long, l: Int, r: Int): Int = {
    if (l <= r) {
      val m = (l + r) / 2
      if (seq(m) <= value)
        findCeiling(seq, value, m + 1, r)
      else
        findCeiling(seq, value, l, m - 1)
    } else l
  }
}
