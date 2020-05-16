package codeforces.contests._1353

/*
https://codeforces.com/contest/1353/problem/D

[Priority Queue]

define a customer ordering prioritising size then position
 */
object ConstructingTheArray {

  import math.abs
  import scala.collection.mutable

  def main(args: Array[String]): Unit = {
    println {
      {
        for (_ <- 1 to io.StdIn.readInt) yield {
          val n = io.StdIn.readInt

          val arr = new Array[Int](n)

          implicit val tuple2Ordering: Ordering[(Int, Int)] = Ordering.fromLessThan[(Int, Int)] {
            case ((x, y), (l, r)) =>
              if (abs(x - y) == abs(l - r)) x > l else abs(x - y) < abs(l - r)
          }

          val queue = mutable.PriorityQueue[(Int, Int)](1 -> n)

          @scala.annotation.tailrec
          def loop(i: Int): Unit =
            if (i <= n) {
              val (l, r) = queue.dequeue()
              val m = (l + r) / 2

              arr(m - 1) = i

              if (l != m)
                queue.enqueue(l -> (m - 1))
              if (l != r)
                queue.enqueue((m + 1) -> r)

              loop(i + 1)
            }

          loop(1)

          arr.mkString(" ")
        }
      }.mkString("\n")
    }

  }
}
