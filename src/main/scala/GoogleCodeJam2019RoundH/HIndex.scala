package GoogleCodeJam2019RoundH

import scala.collection.mutable

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050edd/00000000001a274e
 */
object HIndex {

  type HIndex = Int

  def main(args: Array[String]): Unit = {
    println {
      (1 to io.StdIn.readInt())
        .map { t =>
          val _ = io.StdIn.readInt()
          val seq = io.StdIn.readLine.split(" ").map(_.toInt)

          val queue: mutable.PriorityQueue[HIndex] =
            mutable.PriorityQueue[Int]()(Ordering.Int.reverse)

          def findNextHIndex(prevHIndex: HIndex): HIndex = {
            while (queue.nonEmpty && queue.head <= prevHIndex) queue.dequeue()
            queue.headOption
              .map(_ min queue.size)
              .filter(_ > prevHIndex)
              .getOrElse(prevHIndex)
          }

          val hIndexScores = seq
            .scanLeft(0) {
              case (prevHIndex, citations) =>
                queue.enqueue(citations)
                findNextHIndex(prevHIndex)
            }
            .tail
            .mkString(" ")

          s"Case #$t: " + hIndexScores
        }
        .mkString("\n")
    }
  }

}
