package google.kickstart._2021.A

/**
 * https://codingcompetitions.withgoogle.com/kickstart/round/0000000000436140/000000000068cb14
 *
 * [Dijkstra, multiple start nodes]
 */
object RabbitHouse {
  import scala.annotation.tailrec
  import scala.collection.{immutable, mutable}

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(rows, cols) = io.StdIn.readLine.split(" ").map(_.toInt)

      val grid: immutable.IndexedSeq[immutable.IndexedSeq[Int]] =
        (0 until rows).map(_ => io.StdIn.readLine.split(" ").map(_.toInt).toIndexedSeq)

      val queue = mutable.PriorityQueue.empty(Ordering.by[(Int, Int, Int), Int](_._3))

      for (i <- 0 until rows; j <- 0 until cols) {
        queue.enqueue((i, j, grid(i)(j)))
      }

      val score = grid.map(_.toArray)

      val isDone = Array.ofDim[Boolean](rows, cols)

      def isValid(i: Int, j: Int): Boolean = 0 <= i && i < rows && 0 <= j && j < cols

      def relaxScoreAndReturnCost(i: Int, j: Int, height: Int): Int = {
        if (height < score(i)(j))
          throw new RuntimeException(s"Impossible i=$i j=$j prev=$height current=${score(i)(j)}")

        if (height - score(i)(j) <= 1) 0
        else {
          val cost = height - 1 - score(i)(j)
          score(i)(j) = height - 1
          cost
        }
      }

      val deltas = Array((1, 0), (-1, 0), (0, 1), (0, -1))

      @tailrec
      def loop(totalCost: Long = 0L): Long = {
        if (queue.isEmpty) totalCost
        else {
          val (r, c, height) = queue.dequeue()
          if (score(r)(c) != height) loop(totalCost)
          else {
            if (isDone(r)(c)) loop(totalCost)
            else {
              isDone(r)(c) = true

              val cost = deltas.foldLeft(0) {
                case (acc, (dr, dc)) =>
                  val nextR = r + dr
                  val nextC = c + dc
                  acc + {
                    if (isValid(nextR, nextC) && !isDone(nextR)(nextC)) {
                      val prevScore = score(nextR)(nextC)
                      val cost      = relaxScoreAndReturnCost(nextR, nextC, height)
                      val newScore  = score(nextR)(nextC)

                      if (prevScore != newScore) queue.enqueue((nextR, nextC, score(nextR)(nextC)))
                      cost
                    } else 0
                  }
              }

              loop(totalCost + cost)
            }
          }
        }
      }

      printFormattedOutput(t, loop())

    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }
}
