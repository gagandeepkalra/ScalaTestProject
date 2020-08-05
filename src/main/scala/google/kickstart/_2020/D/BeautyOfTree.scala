package google.kickstart._2020.D

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff08/0000000000386edd

[Iterative DFS]

We calculate contribution from each node to the total expected value,

contribution = p(red or blue) of that node
             = p(red) + p(blue) - p(red) * p(blue)

p(node) = times this node would be hit = 1 + a|b steps down and so on

we use DFS to accumulate up the change for a node then update it's ath and bth parent respectively
 */
object BeautyOfTree {

  import scala.collection.mutable.ArrayBuffer

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt()) {
      val Array(n, a, b) = io.StdIn.readLine.split(" ").map(_.toInt)
      val parent = Array(0, 0) ++ {
        val in = io.StdIn.readLine
        if (n > 1) in.split(" ").map(_.toInt) else Array.empty[Int]
      }

      val children = Array.fill[List[Int]](n + 1)(Nil)
      (2 to n).foreach { i =>
        children(parent(i)) ::= i
      }

      val red = new Array[Long](n + 1)
      val blue = new Array[Long](n + 1)

      val buffer = ArrayBuffer.empty[Int]

      val visited = Array.fill(n + 1)(false)

      @scala.annotation.tailrec
      def iterativeDFS(stack: List[Int]): Unit = {
        if (stack.nonEmpty) {
          val i = stack.head

          if (!visited(i)) {
            visited(i) = true

            buffer += i

            red(i) += 1
            blue(i) += 1

            val uStack = children(i).foldLeft(stack)((acc, c) => c :: acc)
            iterativeDFS(uStack)
          } else {
            //node is done, update parent

            val h = buffer.length

            if (h > a) red(buffer(h - 1 - a)) += red(i)
            if (h > b) blue(buffer(h - 1 - b)) += blue(i)

            buffer.trimEnd(1)
            iterativeDFS(stack.tail)
          }
        }
      }

      iterativeDFS(List(1))

      val answer = (1 to n).foldLeft(0L) { case (acc, i) =>
        acc + red(i) * n + blue(i) * n - red(i) * blue(i)
      }

      printFormattedOutput(t, answer / (n.toLong * n.toLong).toDouble)
    }
  }

  def printFormattedOutput(i: Int, op: Double): Unit = {
    println(s"Case #$i: $op")
  }
}
