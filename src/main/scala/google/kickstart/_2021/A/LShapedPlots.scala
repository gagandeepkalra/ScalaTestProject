package google.kickstart._2021.A

/**
 * https://codingcompetitions.withgoogle.com/kickstart/round/0000000000436140/000000000068c509
 */
object LShapedPlots {

  import scala.collection.immutable

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(rows, cols) = io.StdIn.readLine.split(" ").map(_.toInt)
      val grid: immutable.IndexedSeq[immutable.IndexedSeq[Int]] =
        (0 until rows).map(_ => io.StdIn.readLine.split(" ").map(_.toInt).toIndexedSeq)

      val leftView, rightView, topView, bottomView = Array.ofDim[Int](rows, cols)

      for (r <- 0 until rows) {
        var count = 0
        for (c <- 0 until cols) {
          if (grid(r)(c) == 0) count = 0 else count += 1
          leftView(r)(c) = count
        }
      }

      for (r <- 0 until rows) {
        var count = 0
        for (c <- (0 until cols).reverse) {
          if (grid(r)(c) == 0) count = 0 else count += 1
          rightView(r)(c) = count
        }
      }

      for (c <- 0 until cols) {
        var count = 0
        for (r <- 0 until rows) {
          if (grid(r)(c) == 0) count = 0 else count += 1
          topView(r)(c) = count
        }
      }

      for (c <- 0 until cols) {
        var count = 0
        for (r <- (0 until rows).reverse) {
          if (grid(r)(c) == 0) count = 0 else count += 1
          bottomView(r)(c) = count
        }
      }

      def countLShapes(small: Int, large: Int): Int = {
        if (small >= 2 && large >= 2) {
          if (small * 2 <= large) small - 1 else large / 2 - 1
        } else 0
      }

      val result = {
        var result = 0
        for (r <- 0 until rows; c <- 0 until cols if grid(r)(c) != 0) {
          result += {
            countLShapes(leftView(r)(c), topView(r)(c)) + countLShapes(topView(r)(c), leftView(r)(c)) +
              countLShapes(topView(r)(c), rightView(r)(c)) + countLShapes(rightView(r)(c), topView(r)(c)) +
              countLShapes(rightView(r)(c), bottomView(r)(c)) + countLShapes(bottomView(r)(c), rightView(r)(c)) +
              countLShapes(bottomView(r)(c), leftView(r)(c)) + countLShapes(leftView(r)(c), bottomView(r)(c))
          }
        }
        result
      }

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }
}
