package GoogleCodeJam2019RoundC

import scala.collection.immutable.SortedSet

object WiggleWalk {

  type Interval = (Int, Int)

  implicit object Tuple2Ordering extends Ordering[Interval] {
    override def compare(x: Interval, y: Interval): Int =
      if (x._2 < y._1)
        -1
      else if (y._2 < x._1)
        +1
      else
        0
  }

  def main(args: Array[String]): Unit = {
    val testCases = io.StdIn.readInt()

    for (t <- 1 to testCases) {

      val Array(_, rows, cols, sr, sc) = io.StdIn.readLine().split(" ").map(_.toInt)

      val directionOffset = Map(
        'N' -> (-1, 0),
        'S' -> (1, 0),
        'E' -> (0, 1),
        'W' -> (0, -1)
      )

      // sets of non-overlapping intervals
      val rowSets: Array[SortedSet[(Int, Int)]] = (0 to rows).map(_ => SortedSet[Interval]()).toArray
      val colSets: Array[SortedSet[(Int, Int)]] = (0 to cols).map(_ => SortedSet[Interval]()).toArray

      rowSets(sr) += ((sc, sc))
      colSets(sc) += ((sr, sr))

      def loop(ls: List[Char], current: Interval): Interval = {
        ls match {
          case Nil => current
          case direction :: tail =>

            def computeNext(proposedNext: Interval): Interval = {
              val (proposedRow, proposedCol) = proposedNext

              val (nextRow, nextCol) = direction match {
                case 'N' =>
                  (colSets(proposedCol).findIntervalContaining(proposedRow).map(_._1 - 1).getOrElse(proposedRow), proposedCol)
                case 'S' =>
                  (colSets(proposedCol).findIntervalContaining(proposedRow).map(_._2 + 1).getOrElse(proposedRow), proposedCol)
                case 'E' =>
                  (proposedRow, rowSets(proposedRow).findIntervalContaining(proposedCol).map(_._2 + 1).getOrElse(proposedCol))
                case 'W' =>
                  (proposedRow, rowSets(proposedRow).findIntervalContaining(proposedCol).map(_._1 - 1).getOrElse(proposedCol))
              }

              colSets(nextCol) = colSets(nextCol).insert(nextRow)
              rowSets(nextRow) = rowSets(nextRow).insert(nextCol)

              (nextRow, nextCol)
            }

            loop(tail, computeNext(proposedNext = tupleAdd(current, directionOffset(direction))))
        }
      }

      val (resR, resC) = loop(io.StdIn.readLine().toList, (sr, sc))

      println(s"Case #$t: $resR $resC")
    }
  }

  def tupleAdd(p: Interval, t: Interval): Interval = (p._1 + t._1, p._2 + t._2)

  implicit class IntervalSet(val set: SortedSet[Interval]) extends AnyVal {

    def insert(element: Int): SortedSet[Interval] = {
      val maybeLeft = findIntervalContaining(element - 1)
      val maybeRight = findIntervalContaining(element + 1)

      (maybeLeft, maybeRight) match {
        case (Some(left), None) =>
          set - left + ((left._1, element))
        case (None, Some(right)) =>
          set - right + ((element, right._2))
        case (Some(left), Some(right)) =>
          set - left - right + ((left._1, right._2))
        case (None, None) =>
          set + ((element, element))
      }
    }

    def findIntervalContaining(element: Int): Option[Interval] = {
      set.from((element, element)).headOption.filter { case (l, r) => l <= element && element <= r }
    }

  }

}
