package GoogleCodeJam2019Qualifications

import scala.collection.immutable

object RobotProgrammingStrategy {
  def main(args: Array[String]): Unit = {
    val testCases = io.StdIn.readInt()

    for (t <- 1 to testCases) {
      val n = io.StdIn.readInt()

      val input: immutable.Seq[String] = for (_ <- 1 to n) yield io.StdIn.readLine()

      val maxLengthString: String = input.maxBy(_.length)

      def expandToFill500Slots(string: String): String = {
        val times = 500 / string.length + 1
        (string * times).take(500)
      }

      val beatenBy = Map(
        'R' -> 'P',
        'P' -> 'S',
        'S' -> 'R'
      )

      val result = new StringBuilder

      def recur(programs: Set[String], index: Int): String = {
        if (programs.isEmpty) result.result()
        else if (index >= 500) "IMPOSSIBLE"
        else {
          val movesSet = programs.map(_.charAt(index))

          movesSet match {
            case x if x.size == 3 => "IMPOSSIBLE"
            case x if x.size == 1 => result.append(x.map(beatenBy).head).result()
            case x if x.size == 2 =>

              if (x == Set('R', 'P')) {
                result.append('P')
                recur(programs.filter(_.charAt(index) == 'P'), index + 1)
              } else if (x == Set('P', 'S')) {
                result.append('S')
                recur(programs.filter(_.charAt(index) == 'S'), index + 1)
              } else {
                result.append('R')
                recur(programs.filter(_.charAt(index) == 'R'), index + 1)
              }


          }
        }
      }

      println(s"Case #$t: ${recur(input.toSet.map(expandToFill500Slots), 0)}")
    }
  }
}
