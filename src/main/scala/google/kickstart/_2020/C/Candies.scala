package google.kickstart._2020.C

import algorithms.range.PersistentSegmentTree

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff43/0000000000337b4d

for sequence, a b c d e

e.g. Q 3 5 then result = 1c - 2d + 3e

s = Seq a -b c -d e
ms = Seq 1a -2b 3c -4d 5e

ans = ms(l..r).sum - (l-1)* s(l..r).sum

We use segment trees to do range query and updates
 */
object Candies {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, q)        = io.StdIn.readLine.split(" ").map(_.toInt)
      val a: IndexedSeq[Int] = 0 +: io.StdIn.readLine.split(" ").map(_.toInt) // 1 based

      def minusOnePower(p: Int): Int = if ((p & 1) == 1) -1 else 1

      val alternatingSeqPrefixSum         = (0 to n).map(i => minusOnePower(i + 1) * a(i) * 1L)
      val alternatingWeightedSeqPrefixSum = (0 to n).map(i => minusOnePower(i + 1) * a(i) * i * 1L)

      val result = {
        var alternatingSumTree = PersistentSegmentTree[Long](alternatingSeqPrefixSum, (_: Long) + (_: Long))
        var alternatingWeightedSumTree =
          PersistentSegmentTree[Long](alternatingWeightedSeqPrefixSum, (_: Long) + (_: Long))

        (1 to q).foldLeft(0L) { (acc, _) =>
          val Array(c, x, y) = io.StdIn.readLine.split(" ")
          acc + {
            c match {
              case "Q" =>
                val l = x.toInt
                val r = y.toInt
                minusOnePower(l - 1) * {
                  alternatingWeightedSumTree.query(l, r)._1 - (l - 1) * alternatingSumTree.query(l, r)._1
                }
              case "U" =>
                val i      = x.toInt
                val newVal = y.toInt
                alternatingSumTree = alternatingSumTree.update(i, _ => minusOnePower(i + 1) * newVal)
                alternatingWeightedSumTree = alternatingWeightedSumTree.update(i, _ => minusOnePower(i + 1) * newVal * i)
                0
            }
          }
        }
      }

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }

}
