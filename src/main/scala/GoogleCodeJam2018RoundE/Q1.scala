package GoogleCodeJam2018RoundE

/*

Problem
Yogurt can be a nutritious part of an appetizer, main course, or dessert, but it must be consumed before it expires, and
it might expire quickly! Moreover, different cups of yogurt might expire on different days.

Lucy loves yogurt, and she has just bought N cups of yogurt, but she is worried that she might not be able to consume
all of them before they expire. The i-th cup of yogurt will expire Ai days from today, and a cup of yogurt cannot be
consumed on the day it expires, or on any day after that.

As much as Lucy loves yogurt, she can still only consume at most K cups of yogurt each day. What is the largest number
of cups of yogurt that she can consume, starting from today?

Input
The first line of the input gives the number of test cases, T. T test cases follow. Each test case starts with one line
containing two integers N and K. Then, there is one more line with N integers Ai.

Output
For each test case, output one line containing Case #x: y, where x is the test case number (starting from 1) and y is
the maximum number of cups of yogurt that Lucy can consume.

 */
object Q1 {

  def printFormattedOutput(i: Int, op: Int): Unit = {
    println(s"Case #$i: $op")
  }

  def main(args: Array[String]): Unit = {
    for (i <- 1 to io.StdIn.readInt) {
      val Array(n, k), expiries = io.StdIn.readLine().split(" ").map(_.toInt)
      val src = expiries.sorted.toStream

      printFormattedOutput(i, count(0, 0, src, 0))

      def count(day: Int, consumedToday: Int, src: Stream[Int], result: Int): Int = {
        if (consumedToday == k) count(day + 1, 0, src, result)
        else {
          src match {
            case Stream() => result
            case expiry #:: tail =>
              if (day >= expiry) count(day, 0, tail, result)
              else count(day, consumedToday + 1, tail, result + 1)
          }
        }
      }

    }
  }

}
