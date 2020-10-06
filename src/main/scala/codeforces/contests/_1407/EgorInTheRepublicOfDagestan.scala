package codeforces.contests._1407

/*
https://codeforces.com/contest/1407/problem/E

[Breadth First Search]

Think reversely and color greedily.

All edges (u,v,t) are stored at v side. We start BFS from N. Considering node v and edge (u,v,t). If u has been colored to t,
then this edge cannot be cut and we need to enqueue u. Otherwise we set u to the opposite color of t to cut this edge.

This coloring strategy is optimal because a node visited earlier corresponds to a shorter distance to N.

If after the BFS we have not visited 1, then it is possible to make 1 and N not connected. Otherwise dist[1] is just the
maximal shortest path distance we are required to find. The coloring has been determined during BFS.

For those uncolored nodes, either color is OK.
 */
object EgorInTheRepublicOfDagestan {

  import scala.collection.immutable.Queue

  def main(args: Array[String]): Unit = {
    val Array(n, m) = io.StdIn.readLine().split(" ").map(_.toInt)
    val graph = Array.fill[List[(Int, Int)]](n + 1)(Nil)

    (1 to m).foreach { _ =>
      val Array(l, r, color) = io.StdIn.readLine().split(" ").map(_.toInt)
      graph(r) ::= (l, color)
    }

    val visited = new Array[Boolean](n + 1) // true if ever been inside the Queue
    val color, distance = Array.fill[Int](n + 1)(-1)

    @scala.annotation.tailrec
    def bfs(queue: Queue[Int] = Queue(n)): Unit = {
      if (queue.nonEmpty) {
        val (v, q) = queue.dequeue

        val updatedQueue = graph(v).foldLeft(q) { case (q, (u, c)) =>
          color(u) match {
            case -1 => color(u) =
              (c + 1) % 2 // set opposite color, block this edge
              q
            case _ if visited(u) =>
              q
            case `c` =>
              visited(u) = true
              distance(u) = distance(v) + 1
              q.enqueue(u)
            case _ =>
              q
          }
        }

        bfs(updatedQueue)
      }
    }

    color(n) = 1 // doesn't matter
    distance(n) = 0

    visited(n) = true

    bfs()

    println(distance(1))
    println(color.map(x => if (x == -1) 0 else x).view(1, n + 1).mkString(""))
  }
}
