package google.kickstart._2019.E

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050edb/0000000000170721

[Graph Theory]

Connected components
 */
object CherriesMesh {

  import scala.collection.mutable

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(v, e) = io.StdIn.readLine().split(" ").map(_.toInt)

      val edges = mutable.HashMap[Int, Set[Int]]().withDefaultValue(Set.empty)

      for (_ <- 1 to e) {
        val Array(l, r) = io.StdIn.readLine().split(" ").map(_.toInt)
        edges.update(l, edges(l) + r)
        edges.update(r, edges(r) + l)
      }

      println {
        def findConnected(current: Int, visited: Set[Int]): (Int, Set[Int]) = {
          edges(current).foldLeft((1, visited + current)) { (acc, next) =>
            acc match {
              case (result, set) if !set(next) =>
                val (nextResult, nextSet) = findConnected(next, set)
                (result + nextResult, nextSet)
              case _ => acc
            }
          }
        }

        val (connectedSizesList, _) =
          (1 to v).foldLeft((List[Int](), Set[Int]())) { (acc, i) =>
            acc match {
              case (list, visited) if !visited(i) =>
                val (size, updatedVisited) = findConnected(i, visited)
                (size :: list, updatedVisited)
              case _ => acc
            }
          }

        val result = {
          val connectedLength = connectedSizesList.length
          val totalConnected = connectedSizesList.sum
          (totalConnected - connectedLength) + (connectedLength - 1) * 2
        }

        s"Case #$t: $result"
      }
    }
  }
}
