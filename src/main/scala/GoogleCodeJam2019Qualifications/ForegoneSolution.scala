package GoogleCodeJam2019Qualifications

object ForegoneSolution {

  def main(args: Array[String]): Unit = {
    val testCases = io.StdIn.readInt()

    for (t <- 1 to testCases) {
      val n = io.StdIn.readLine()

      if (n.contains('4')) {
        val a = n.map(c => if (c == '4') '3' else c).dropWhile(_ == '0')
        val b = n.map(c => if (c == '4') '1' else '0').dropWhile(_ == '0')

        println(s"Case #$t: $a $b")
      } else {

        println(s"Case #$t: ${findSolution(n).getOrElse("3" * n.length + " " + "2" * n.length)}")
      }


    }
  }

  def findSolution(n: String): Option[String] = {
    val idx = n.indexWhere(_ != '5')

    if (idx == -1) None
    else {
      val a = n.updated(idx, (n.charAt(idx) - 1).toChar).dropWhile(_ == '0').mkString("")
      val b = ("0" * n.length).updated(idx, '1').dropWhile(_ == '0').mkString("")

      Some(a + " " + b)
    }
  }
}
