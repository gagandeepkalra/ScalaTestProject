package codeforces.contests._1359

/*
https://codeforces.com/contest/1359/problem/D

[Rectangle in a histogram]

we consider each index, calculating it's left view  (how far we can look left not obstructed by someone bigger than me) and
right view then max this by eaxh index.
 */
object YetAnotherYetAnotherTask {

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()

    val seq = 0 +: io.StdIn.readLine.split(" ").map(_.toInt) // 1 based
    val prefix = seq.scanLeft(0)(_ + _).tail

    @scala.annotation.tailrec
    def calculateLeftView(idx: Int = 1,
                          decreasingStack: List[(Int, Int)] = Nil, // stack for indices and there max sub array sum ending at them
                          resultList: List[Int] = Nil): List[Int] = {
      if (idx > n) resultList
      else {
        val (toConsider, rest) = decreasingStack.partition { case (i, _) => seq(i) <= seq(idx) }

        val resultSum = toConsider.foldLeft(seq(idx)) { case (acc, (i, maxSum)) =>
          val rangeSum = prefix(idx) - prefix(i) // without i
          acc max ((maxSum + rangeSum) max (seq(i) + rangeSum))
        }

        calculateLeftView(idx + 1, (idx, resultSum) :: rest, resultSum :: resultList)
      }
    }

    @scala.annotation.tailrec
    def calculateRightView(idx: Int = n,
                           decreasingStack: List[(Int, Int)] = Nil, // stack for indices and there max sub array sum ending at them
                           resultList: List[Int] = Nil): List[Int] = {
      if (idx == 0) resultList
      else {
        val (toConsider, rest) = decreasingStack.partition { case (i, _) => seq(i) <= seq(idx) }

        val resultSum = toConsider.foldLeft(seq(idx)) { case (acc, (i, maxSum)) =>
          val rangeSum = prefix(i - 1) - prefix(idx - 1)
          acc max ((maxSum + rangeSum) max (seq(i) + rangeSum))
        }

        calculateRightView(idx - 1, (idx, resultSum) :: rest, resultSum :: resultList)
      }
    }

    val leftView = calculateLeftView().reverse
    val rightView = calculateRightView()
    println {
      leftView.zip(rightView).zipWithIndex.foldLeft(0) { case (acc, ((lSum, rSum), i)) => acc max (lSum + rSum - 2 * seq(i + 1)) }
    }
  }
}
