package codeforces.contests._1367

/*
https://codeforces.com/contest/1367/problem/F2

The solution is the longest sub-array of the sorted array that was also a subsequence of the original (intuition: we try
most stay at their original place, the rest be moved to either side). The elements that are not moved have to form a sorted
subsequence. So if we can maximize the length the sorted subsequence, the number of operation is minimized.

We find the longest sorted sub-array with increasing indices (in the pre-sorted version).

When we iterate from one unique number to the next largest, there are 2 possible cases

case 1: The last index in the previous number is less than the first index of the current number. Then we can just extend
  the subsequence and continue to the next number

case 2: case 1 is not true. In this case, we need to start a new sequence, but there's 3 things we need to look at.

a. We take the earliest index of the current number that is greater than the last index of the current number and add
  those number to the end of the current sequence. (we try include the breakage (multiple occurrences) but then we cannot
  include the next of breakage, the breakage occurrence moves to the last, we don't count it)

b. We then also look for the longest sequence of just the previous and current numbers. For each index of the previous number,
  we find the earliest index of the current number that is after it, and we have a subsequence with just those indices
  (this happens when we try include both sides of the breakage, again multiple occurrences case, each of the ignored values move
  to there respective sides).

c. We find the latest index of the previous number that is smaller than the first index of the current number and start
  our new subsequence with them (We start anew, trying to take as much from the past, the breakage is shifted to the left, again
  ignored values will move to the left)
 */
object FlyingSort {

  import java.io.{BufferedReader, InputStreamReader}
  import java.util.StringTokenizer

  val br = new BufferedReader(new InputStreamReader(System.in))

  def readInt: Int = br.readLine().toInt

  def readIntArrayZipped(n: Int): Array[(Int, Int)] = {
    val st = new StringTokenizer(br.readLine())
    val arr = new Array[(Int, Int)](n)
    var i = 0
    while (st.hasMoreElements) {
      arr(i) = (st.nextToken().toInt, i)
      i += 1
    }
    arr
  }

  def main(args: Array[String]): Unit = {
    println {
      {
        for (_ <- 1 to readInt) yield {
          val n = readInt
          val original: Array[(Int, Int)] = readIntArrayZipped(n)
          val sorted = original.sortBy(_._1)

          @scala.annotation.tailrec
          def loop(idx: Int, acc: Int = 0, accM: Int = 0): Int = {
            if (idx >= n) acc max accM
            else {
              val (currValue, current) = sorted(idx)
              val (prevValue, prev) = sorted(idx - 1)

              def forwardCount(starting: Int, value: Int) = (starting until n).takeWhile(i => sorted(i)._1 == value).length

              def backwardCount(starting: Int, value: Int) = (starting to 0 by -1).takeWhile(i => sorted(i)._1 == value).length

              if (current > prev) loop(idx + 1, acc + 1, acc max accM)
              else {
                val rightTotalCount = forwardCount(idx, currValue)
                val leftTotalCount = backwardCount(idx - 1, prevValue)

                val leftRange = idx - leftTotalCount until idx

                val (considerOnlyBothAnswer, _, _, rRemaining) = leftRange.foldLeft((0, 0, idx, rightTotalCount)) {
                  case ((maxBoth, leftPassed, rIndex, rRemaining), l) =>
                    val removed = {
                      var r = rIndex
                      while (r < idx + rightTotalCount && sorted(r)._2 < sorted(l)._2) r += 1
                      r - rIndex
                    }

                    val rRemainingU = rRemaining - removed
                    (maxBoth max rRemainingU + leftPassed + 1, leftPassed + 1, rIndex + removed, rRemainingU)
                }

                val leftEndsHereAnswer = acc + rRemaining

                val relevantPrev = leftRange.takeWhile(i => sorted(i)._2 < current)

                loop(idx + 1, relevantPrev.length + 1, acc max accM max leftEndsHereAnswer max considerOnlyBothAnswer)
              }
            }
          }

          n - loop(1, 1, 1)
        }
      }.mkString("\n")
    }
  }
}
