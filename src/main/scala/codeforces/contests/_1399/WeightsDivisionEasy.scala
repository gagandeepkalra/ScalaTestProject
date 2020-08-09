package codeforces.contests._1399

import scala.collection.mutable.ArrayBuffer

/*
https://codeforces.com/contest/1399/problem/E1

[Graph Theory]

We calculate contribution of each edge by calculating number of leaves if we go down it. Afterwards, we use a max heap to
reduce the maximals of the weights only that the heap is ordered by diffs = w*c - w/2*c, courtesy integer division

todo: Doesn't pass; TLE, see java version
 */
object WeightsDivisionEasy {

  import java.io.{BufferedReader, InputStreamReader}

  import scala.collection.mutable

  def main(args: Array[String]): Unit = {

    val br = new BufferedReader(new InputStreamReader(System.in))

    val t = br.readLine().toInt

    println({
      for (_ <- 1 to t) yield {
        val strarr = br.readLine.split("\\s+")

        val n = strarr(0).toInt
        val s = strarr(1).toLong

        val graph = Array.fill[ArrayBuffer[(Int, Int)]](n.toInt + 1)(ArrayBuffer.empty)
        (1 until n.toInt).foreach { _ =>
          val sarr = br.readLine.split("\\s+")
          val u = sarr(0).toInt
          val v = sarr(1).toInt
          val w = sarr(2).toInt

          graph(u).append((v, w))
          graph(v).append((u, w))
        }

        val queue = mutable.PriorityQueue.empty[(Int, Int)](Ordering.by { case (w, c) => (w: Long) * c - (w / 2: Long) * c })

        var sum = 0L

        def dfs(u: Int, p: Int): Int = {
          if (graph(u).size == 1 && graph(u).head._1 == p) 1
          else
            graph(u).foldLeft(0) { case (acc, (v, w)) =>
              acc + {
                if (v != p) {
                  val l = dfs(v, u)
                  sum += (w: Long) * l
                  queue.enqueue((w, l))
                  l
                } else 0
              }
            }
        }

        dfs(1, 0)

        var steps = 0

        while (sum > s) {
          val (w, c) = queue.dequeue()
          queue.enqueue((w / 2, c))
          sum += -(w: Long) * c + (w / 2: Long) * c
          steps += 1
        }

        steps
      }
    }.mkString("\n"))
  }

}
