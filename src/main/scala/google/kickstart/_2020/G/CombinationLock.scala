package google.kickstart._2020.G

/*
https://codingcompetitions.withgoogle.com/kickstart/round/00000000001a0069/0000000000414a24

A combination lock has W wheels, each of which has the integer values 1 through N on it, in ascending order.

At any moment, each wheel shows a specific value on it. Xi is the initial value shown on the i-th wheel.

You can use a single move to change a wheel from showing the value X to showing either X+1 or X-1, wrapping around between
1 and N. For example, if a wheel currently shows the value 1, in one move you can change its value to 2 or N.

Given all wheels' initial values, what is the minimum number of moves to get all wheels to show the same value?

-->

order doesn't matter, we sort all values first

we bring all to each of the values one by one and find the minimum cost

for each w, we find the index of closest value to it's threshold, afterwards we bring them all in using leftPrefix and rightSuffix cost precomputed
 */
object CombinationLock {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(w, n) = io.StdIn.readLine().split(" ").map(_.toInt)

      val sorted = io.StdIn.readLine().split(" ").map(_.toLong).sorted

      @scala.annotation.tailrec
      def upperBoundBinarySearch(l: Int = 0, r: Int = w - 1, end: Long): Int = { // index of value just greater than end
        if (l > r) l
        else if (l == r) {
          val curr = sorted(l)
          if (curr <= end) l + 1 else l
        }
        else {
          val m = (l + r) / 2
          val curr = sorted(m)

          if (curr <= end) upperBoundBinarySearch(m + 1, r, end)
          else upperBoundBinarySearch(l, m, end)
        }
      }

      @scala.annotation.tailrec
      def lowerBoundBinarySearch(l: Int = 0, r: Int = w - 1, start: Long): Int = { // index of value just smaller than start
        if (l > r) r
        else if (l == r) {
          val curr = sorted(l)
          if (start <= curr) l - 1 else l
        }
        else {
          val m = (l + r) / 2
          val curr = sorted(m)

          if (start <= curr) lowerBoundBinarySearch(l, m - 1, start)
          else lowerBoundBinarySearch(m + 1, r, start)
        }
      }

      val threshold = (n + 1) / 2

      val first = { // cost to bring all to 1
        sorted.map(i => if (i <= threshold) i - 1 else n - i + 1).sum
      }

      val leftSum = sorted.scanLeft(0L)(_ + _).tail // cost to bring [0..i] to 0
      val rightSum = sorted.scanRight(0L)((i, acc) => acc + n - i) // cost to bring [i..w-1] to n

      val result = sorted.zipWithIndex.foldLeft(first) { case (acc, (i, idx)) =>
        acc min {
          if (i <= threshold) {
            var result = 0L

            val currThreshold = i + threshold - 1

            val upper = upperBoundBinarySearch(end = currThreshold)

            result += leftSum(upper - 1) - (if (idx == 0L) 0 else leftSum(idx - 1)) - (upper - idx) * i
            result += rightSum(upper) + (w - upper) * i
            result += rightSum(0) - rightSum(idx + 1) - (n - i) * (idx + 1)

            result
          } else {
            var result = 0L

            result += leftSum(w - 1) - (if (idx == 0L) 0L else leftSum(idx - 1)) - (w - idx) * i

            val currThreshold = i - (if (n % 2 == 1) threshold - 1 else threshold)
            val lower = lowerBoundBinarySearch(start = currThreshold)

            result += rightSum(lower + 1) - rightSum(idx + 1) - (idx - lower) * (n - i)
            result += (if (lower < 0) 0 else leftSum(lower)) + (n - i) * (lower + 1)

            result
          }
        }
      }

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
