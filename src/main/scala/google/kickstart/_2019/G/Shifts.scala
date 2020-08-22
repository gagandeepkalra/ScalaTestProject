package google.kickstart._2019.G

import algorithms.range.SegmentTree

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050e02/000000000018fd5e

[Meet In The Middle] [Segment Trees]
 */
object Shifts {

  def mapToRange(values: Set[Long]): Map[Long, Int] = {
    values.toArray.sorted.zipWithIndex.map { case (v, i) => v -> i }.toMap
  }

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt()) {
      val Array(n, h) = io.StdIn.readLine.split(" ").map(_.toInt)
      val pointsA, pointsB = io.StdIn.readLine.split(" ").map(_.toInt)

      def findPairs(start: Int, endExclusive: Int): List[(Long, Long)] = {
        var pairs = List[(Long, Long)]()

        def findPairs(i: Int = 0, accA: Long = 0L, accB: Long = 0L): Unit =
          if (i == endExclusive)
            pairs = ((accA, accB)) :: pairs
          else {
            findPairs(i + 1, accA + pointsA(i), accB)
            findPairs(i + 1, accA, accB + pointsB(i))
            findPairs(i + 1, accA + pointsA(i), accB + pointsB(i))
          }

        findPairs(start)
        pairs.sorted
      }

      val firstHalf = findPairs(start = 0, endExclusive = n / 2)
      val secondHalfReversed =
        findPairs(start = n / 2, endExclusive = n).reverse

      // first half for querying, second half for inserting
      val possibleValues = (firstHalf.map(h - _._2) ++ secondHalfReversed.map(_._2)).toSet
      val size = possibleValues.size
      val mappedToRange = mapToRange(possibleValues)

      val tree = SegmentTree[Int](0, size, (_: Int) + (_: Int))

      @scala.annotation.tailrec
      def loop(fList: List[(Long, Long)], sList: List[(Long, Long)], result: Long = 0L): Long = {
        fList match {
          case Nil => result
          case (fa, fb) :: fTail =>
            val lowerA = h - fa
            val lowerB = h - fb
            // find values >= lower
            val (higherA, sTail) = sList.span(_._1 >= lowerA)
            higherA.foreach {
              case (_, sb) => tree.update(mappedToRange(sb), _ + 1)
            }

            // add to result values greater than or equal to lowerB
            loop(fTail, sTail, result + tree.query(mappedToRange(lowerB), size - 1))
        }
      }

      println {
        s"Case #$t: ${loop(firstHalf, secondHalfReversed)}"
      }
    }
  }
}
