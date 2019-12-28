package hackerrank

/*
https://www.hackerrank.com/challenges/kruskalmstrsub/problem
Kruskal's algorithm for Minimum Spanning Tree
 */
object ReallySpecialSubtree {

  def main(args: Array[String]): Unit = {

    val Array(n, m) = io.StdIn.readLine().split(" ").map(_.toInt)
    val edgesList = (1 to m)
      .map { _ =>
        val Array(l, r, w) = io.StdIn.readLine.split(" ").map(_.toInt)
        (l min r, l max r, w)
      }
      .sortBy(_._3)
      .toList

    val parent = new Array[Int](n + 1)

    def findParent(i: Int): Int =
      if (i == parent(i))
        i
      else {
        parent(i) = findParent(parent(i)) // path compression
        parent(i)
      }

    (1 to n).foreach { i =>
      parent(i) = i
    }

    def include(l: Int, r: Int, w: Int) = {
      val lp = findParent(l)
      val rp = findParent(r)

      if (lp != rp) {
        parent(rp) = lp
        w
      } else 0
    }

    println {
      edgesList.foldLeft(0)(_ + (include _).tupled(_))
    }

  }
}
