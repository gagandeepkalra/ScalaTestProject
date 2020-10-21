package codeforces.contests._1433

/*

https://codeforces.com/contest/1433/problem/D

1  2  3
1  2  3
1  2
1
1

Connect all 1's to 2a, all 2's to 3a, all 3's to 1a
 */
object DistrictsConnection {
  def main(args: Array[String]): Unit = {
    println{
      {
        for (_ <- 1 to io.StdIn.readInt()) yield {
          val n = io.StdIn.readInt()

          val digits = io.StdIn.readLine.split(" ").map(_.toInt)

          val groups = (1 to n).groupBy(i => digits(i - 1)).values.toIndexedSeq

          if (groups.size <= 1) "NO" else {
            val first = groups.head
            val last = groups.last

            val temp = groups.tail
              .foldLeft((new StringBuilder("YES"), first)) { case ((sb, prevG), currG) =>
                (sb.append(prevG.map("\n" + _ + " " + currG.head).mkString("")), currG)
              }

            temp._1.append(last.tail.map("\n" + _ + " " + first.head).mkString("")).toString()
          }
        }
      }.mkString("\n")
    }
  }
}
