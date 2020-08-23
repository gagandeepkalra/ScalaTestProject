package google.kickstart._2020.D

import algorithms.arrays.Stack
import algorithms.range.SparseTable

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff08/0000000000386d5c

If we find the kth gate then based on s position we have the answer

We begin by calculating the first gate, short if we are looking for only the first. Otherwise we binary search to find the answer.

For each m middle, we find it's rank then search each half respectively. if the rank matches the exact k, this would mean
the answer was on the right side of s, otherwise if l == r and we still don't have the exact match then this simply means
the answer is on the left.

rank

we pre-calculate monotonic left view and a Sparse table for range maximum queries in O(1)

e.g.

5 7 2 4 6 1 3

given firstGate = 4, we find rank of 1

1. find max between 4 and 1, i.e. 8
2. find it's left view, i.e. 7
3. rank = no. of elements between 1 and left view, i.e. 5

idea is simple, rank is monotonic increasing from s on both sides, once we reflect using the max value we can slide forward right
 */
object LockedDoors {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, q) = io.StdIn.readLine.split(" ").map(_.toInt)
      val gates = io.StdIn.readLine.split(" ").map(_.toInt)

      val sparseTable = new SparseTable(n - 1, (x, y) => if (gates(x) > gates(y)) x else y)

      val left: IndexedSeq[Int] = Stack.leftView(gates)

      val result = (1 to q).map { _ =>
        val Array(s, k) = io.StdIn.readLine.split(" ").map(_.toInt - 1)

        // kth gate that opens in absolute terms (1 based)

        val firstGate = {
          if (s == 0) 0
          else if (s == n - 1) n - 2
          else if (gates(s - 1) < gates(s)) s - 1
          else s
        }

        @scala.annotation.tailrec
        def binarySearch(l: Int, r: Int): Int = {
          if (l == r) {
            val m = l

            val mScore = if (m == firstGate) 1 else {
              val maxI = sparseTable.query(firstGate, m)
              val counterpart = left(maxI) + 1
              m - counterpart + 1
            }

            if (mScore == k) m
            else if (mScore < k) m - k + 1
            else m - k
          } else {
            val m = (l + r) / 2

            val mScore = if (m == firstGate) 1 else {
              val maxI = sparseTable.query(firstGate, m)
              val counterpart = left(maxI) + 1
              m - counterpart + 1
            }

            if (mScore == k) m
            else if (mScore < k) binarySearch(m + 1, r)
            else binarySearch(l, m)
          }
        }

        {
          if (k == 0) s
          else {
            val kthGate = binarySearch(firstGate, n - 2)
            if (s <= kthGate) kthGate + 1
            else kthGate
          }
        } + 1 // 0 based world
      }.mkString(" ")

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
