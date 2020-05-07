package google.kickstart._2019.F

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050edc/000000000018666c

[Dynamic Programming]

problem can be reduced with this fact- "whenever we rebuild a section of the wall, it is equivalent to removing that section"

let dp(n, k) = min cost to have at most k breakages in 1 ... n, then
let cost(i, j) = min cost to make all same (total - max frequency); precompute

then dp(n, k) = min (dp(i, k-1) + cost(i+1, n) for each i in 1 ... n-1)

with base dp(i, 0) = cost(0, i)
 */
object Flattening {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
      val heights = io.StdIn.readLine.split(" ").map(_.toInt)

      val cost: Array[Array[Int]] = {
        val c = Array.ofDim[Int](n, n)
        (0 until n).foreach { l =>
          val m = collection.mutable.Map.empty[Int, Int];
          (l until n).foreach { r =>
            m.update(heights(r), m.getOrElse(heights(r), 0) + 1)
            c(l)(r) = r - l + 1 - m.values.max
          }
        }
        c
      }

      val dp = Array.ofDim[Int](n, k + 1)

      (0 until n).foreach(i => dp(i)(0) = cost(0)(i))

      for {
        i <- 1 until n
        j <- 1 to k
      } dp(i)(j) = {
        val l = (0 until i).minBy(l => dp(l)(j - 1) + cost(l + 1)(i))
        dp(l)(j - 1) + cost(l + 1)(i)
      }

      printFormattedOutput(t, dp(n - 1)(k))
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }
}
