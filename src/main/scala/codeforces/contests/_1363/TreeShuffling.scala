package codeforces.contests._1363

/*

https://codeforces.com/contest/1363/problem/E

[Iterative DFS]

We shuffle at each node with cost min of root to node cost

Regular DFS does StackOverflow with Scala and memory explosion if used with trampolined.

Trick is that we remove node from stack only if all the children have been processed
 */
object TreeShuffling {


  type Graph[T] = Array[List[T]]

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()

    val count = Array.ofDim[Int](n + 1, 2) // zeroes and ones for subtree rooted at each of the nodes

    val cost = new Array[Int](n + 1)
    (1 to n).foreach { idx =>
      val Array(c, i, f) = io.StdIn.readLine.split(" ").map(_.toInt)
      if (i != f) {
        if (i == 0) count(idx)(0) = 1 else count(idx)(1) = 1
      }
      cost(idx) = c
    }

    val graph: Graph[Int] = {
      val arr = Array.fill[List[Int]](n + 1)(Nil)
      (1 until n).foreach { _ =>
        val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
        arr(x) ::= y
        arr(y) ::= x
      }
      arr
    }

    var total: Long = 0L

    val parent = new Array[Int](n + 1)
    val visited = new Array[Boolean](n + 1)

    @scala.annotation.tailrec
    def iterativeDFS(stack: List[Int]): Unit = {
      if (stack.nonEmpty) {
        val i = stack.head
        val p = parent(i)

        if (!visited(i)) {
          visited(i) = true
          val uStack = graph(i).foldLeft(stack)((acc, c) => if (c == p) acc else {
            cost(c) = cost(c) min cost(i)
            parent(c) = i
            c :: acc
          })
          iterativeDFS(uStack)
        } else {
          //node is done, update parent
          val toRemove = count(i)(0) min count(i)(1)

          total += 2L * toRemove * cost(i)
          count(p)(0) += count(i)(0) - toRemove
          count(p)(1) += count(i)(1) - toRemove

          iterativeDFS(stack.tail)
        }
      }
    }

    println {
      iterativeDFS(List(1))
      val zeroes = count(0)(0)
      val ones = count(0)(1)
      if (zeroes == 0 && ones == 0) total else -1
    }
  }

}