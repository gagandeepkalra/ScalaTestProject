package codeforces.contests._1409

/*
https://codeforces.com/contest/1409/problem/E

we maintain left and right view arrays, each corresponding to number of indices a platform would cover if placed at current
index looking left and right direction respectively

then we calculate a scanned version of each direction, afterwards just maxBy in one iteration
 */
object TwoPlatforms {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)

      val arr = io.StdIn.readLine.split(" ").map(_.toInt).sorted

      io.StdIn.readLine.split(" ").map(_.toInt) // y coordinates, dirty read

      val right: IndexedSeq[Int] = {
        val right = new Array[Int](n)

        @scala.annotation.tailrec
        def loopR(i: Int = 0, j: Int = 0): Unit = {
          if (i != n) {
            if (j == n || arr(i) + k < arr(j)) {
              right(i) = j - i
              loopR(i + 1, j max (i + 1))
            }
            else loopR(i, j + 1)
          }
        }

        loopR()
        right
      }

      val rightMax = right.scanRight(Int.MinValue)(_ max _)

      val left: IndexedSeq[Int] = {
        val left = new Array[Int](n)

        @scala.annotation.tailrec
        def loopL(i: Int = n - 1, j: Int = n - 1): Unit = {
          if (i >= 0) {
            if (j < 0 || arr(i) - k > arr(j)) {
              left(i) = i - j
              loopL(i - 1, j min (i - 1))
            }
            else loopL(i, j - 1)
          }
        }

        loopL()
        left
      }

      val leftMax = left.scanLeft(Int.MinValue)(_ max _).tail

      @scala.annotation.tailrec
      def loop(i: Int = 0, acc: Int = Int.MinValue): Int = {
        if (i == n) acc
        else {
          loop(
            i + 1,
            acc max {
              right(i) + {
                val rightResult = if (i + right(i) + 1 < n) rightMax(i + right(i) + 1) else Int.MinValue
                val leftResult = if (i > 0) leftMax(i - 1) else Int.MinValue
                val result = leftResult max rightResult

                if (result == Int.MinValue) 0 else result
              }
            }
          )
        }
      }

      println {
        loop()
      }
    }
  }
}
