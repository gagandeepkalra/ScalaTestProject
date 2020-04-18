package hackerrank

/*
https://www.hackerrank.com/challenges/crab-graphs/problem

Graph Theory

Ford-Fulkerson Algorithm for Maximum Flow Problem

Reduce input graph to a bipartite one and find maximum flow through it

Bipartite graph to have two sets A, B (each with all the n vertices) apart from source and sink
For each edge u -> v, we join corresponding vertices from both sets with weight = 1
Source connects to A with max weight t for each a in A, similarly each b in B connects to sink with weight = 1

 */
object CrabGraphs {

  type Graph[T] = Array[List[T]]

  @scala.annotation.tailrec
  def dropUntil[A, B](ls: List[A], f: A => Option[B]): Option[(B, List[A])] = {
    ls match {
      case Nil => None
      case h :: tail =>
        f(h) match {
          case Some(b) => Some(b, ls)
          case None => dropUntil(tail, f)
        }
    }
  }

  def solve(n: Int, t: Int, graph: Graph[Int]): Int = {
    val source = 0
    val sink = 2 * n + 1

    val flowGraph: Graph[(Int, Int)] = {
      val arr = Array.fill[List[(Int, Int)]](2 * n + 2)(Nil)
      (1 to n).foreach { i =>
        arr(source) = (i, t min graph(i).length) :: arr(source)
        arr(n + i) = (sink, 1) :: arr(n + i)
        graph(i).foreach { j =>
          arr(i) = (n + j, 1) :: arr(i)
        }
      }
      arr
    }

    def dfs(i: Int, flow: Int = Int.MaxValue, visited: Set[Int] = Set.empty): Option[Int] = {
      if (i == sink)
        Some(flow)
      else if (visited(i)) None
      else {
        val nowVisited = visited + i
        val result = dropUntil[(Int, Int), Int](flowGraph(i), { case (j, w) => dfs(j, flow min w, nowVisited) })

        result match {
          case Some((flow, (h, w) :: tail)) =>
            flowGraph(i) = if (w - flow > 0) (h, w - flow) :: tail else tail
            flowGraph(h) = (i, flow) :: flowGraph(h)

            Some(flow)
          case _ =>
            flowGraph(i) = Nil
            None
        }
      }
    }

    @scala.annotation.tailrec
    def findMaxFlow(maxFlow: Int = 0): Int =
      dfs(source) match {
        case Some(flow) => findMaxFlow(flow + maxFlow)
        case None => maxFlow
      }

    findMaxFlow()
  }

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val Array(n, t, m) = io.StdIn.readLine().split(" ").map(_.toInt)

      val graph: Graph[Int] = {
        val arr = Array.fill[List[Int]](n + 1)(Nil)
        (1 to m).foreach { _ =>
          val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
          arr(x) = y :: arr(x)
          arr(y) = x :: arr(y)
        }
        arr
      }

      println(solve(n, t, graph))
    }
  }

}
