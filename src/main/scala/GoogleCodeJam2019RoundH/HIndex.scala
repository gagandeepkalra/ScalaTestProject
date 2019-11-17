package GoogleCodeJam2019RoundH

object HIndex {

  type HIndex = Int

  def main(args: Array[String]): Unit = {
    println {
      (1 to io.StdIn.readInt())
        .map { t =>
          val _ = io.StdIn.readInt()
          val seq = io.StdIn.readLine.split(" ").map(_.toInt)

          val MAX = seq.max

          val store = new RangedSegmentTree(MAX, _ + _)

          @scala.annotation.tailrec
          def findNextHIndex(prevHIndex: HIndex): HIndex = {
            val next = prevHIndex + 1
            if (store.query(next, MAX) >= next)
              findNextHIndex(next)
            else
              prevHIndex
          }

          val hIndexScores = seq
            .scanLeft(0) {
              case (prevHIndex, citations) =>
                store.update(citations, _ + 1)
                findNextHIndex(prevHIndex)
            }
            .tail
            .mkString(" ")

          s"Case #$t: " + hIndexScores
        }
        .mkString("\n")
    }
  }

  class RangedSegmentTree private (segmentArr: Array[Int],
                                   max: Int,
                                   f: (Int, Int) => Int) {

    def this(max: Int, f: (Int, Int) => Int) = {
      this(new Array[Int](4 * (max + 1)), max, f)
    }

    def update(idx: Int, g: Int => Int): RangedSegmentTree = {
      def update(l: Int, r: Int, i: Int): Unit = {
        if (l == r) segmentArr(i) = g(segmentArr(i))
        else {
          val m = (l + r) / 2
          if (idx <= m) update(l, m, 2 * i + 1) else update(m + 1, r, 2 * i + 2)
          segmentArr(i) = f(segmentArr(2 * i + 1), segmentArr(2 * i + 2))
        }
      }
      update(0, max, 0)
      this
    }

    /*
    both index inclusive
     */
    def query(left: Int, right: Int): Int = {
      def query(l: Int, r: Int, i: Int, x: Int, y: Int): Int = {
        if (l == x && r == y) segmentArr(i)
        else {
          val m = (l + r) / 2

          if (y <= m) query(l, m, 2 * i + 1, x, y)
          else if (m < x) query(m + 1, r, 2 * i + 2, x, y)
          else {
            f(
              query(l, m, 2 * i + 1, x, m),
              query(m + 1, r, 2 * i + 2, m + 1, y)
            )
          }
        }
      }
      query(0, max, 0, left, right)
    }
  }

}
