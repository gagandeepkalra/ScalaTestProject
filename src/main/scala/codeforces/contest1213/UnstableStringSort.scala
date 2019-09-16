package codeforces.contest1213

/*
https://codeforces.com/contest/1213/problem/F
 */
object UnstableStringSort {

  val allChars = (0 until 26).map(i => (i + 'a').toChar.toString)

  def main(args: Array[String]): Unit = {
    val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)

    val seq1, seq2 = io.StdIn.readLine.split(" ").map(_.toInt)

    @scala.annotation.tailrec
    def loop(i: Int,
             l1: List[Int],
             l1Seen: Set[Int],
             l1Expected: Set[Int],
             l2: List[Int],
             l2Seen: Set[Int],
             l2Expected: Set[Int],
             result: List[Int]): List[Int] = {
      if (l1Expected.isEmpty && l2Expected.isEmpty) {
        if (i == n)
          result
        else {
          val l1NewSeen = Set(l1.head)
          val l1NewExpected = Set(l2.head) - l1.head
          val l2NewSeen = Set(l2.head)
          val l2NewExpected = Set(l1.head) - l2.head
          loop(
            i + 1,
            l1.tail,
            l1NewSeen,
            l1NewExpected,
            l2.tail,
            l2NewSeen,
            l2NewExpected,
            i :: result
          )
        }
      } else {
        val l1NewSeen = l1Seen + l1.head
        val l1NewExpected = (if (l1NewSeen.contains(l2.head)) l1Expected
                             else l1Expected + l2.head) - l1.head
        val l2NewSeen = l2Seen + l2.head
        val l2NewExpected = (if (l2NewSeen.contains(l1.head)) l2Expected
                             else l2Expected + l1.head) - l2.head
        loop(
          i + 1,
          l1.tail,
          l1NewSeen,
          l1NewExpected,
          l2.tail,
          l2NewSeen,
          l2NewExpected,
          result
        )
      }
    }

    println {
      val subsequenceIndices = (n :: loop(
        0,
        seq1.toList,
        Set.empty,
        Set.empty,
        seq2.toList,
        Set.empty,
        Set.empty,
        Nil
      )).reverse
      if (subsequenceIndices.size - 1 < k)
        "NO"
      else {
        println("YES")
        val sortedString = subsequenceIndices
          .sliding(2)
          .zipWithIndex
          .map { case (List(l, r), i) => allChars((k - 1) min i) * (r - l) }
          .mkString("")

        val p = new Array[Char](n)

        (0 until n).foreach { i =>
          p(seq1(i) - 1) = sortedString(i)
        }

        p.mkString("")
      }
    }

  }

}
