package google.kickstart._2020.F

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff48/00000000003f4b8b

Greedy Robot deployment, try cover as much from the previous action, only after exhaustion do summon another.
 */
object MetalHarvest {
  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
      val intervals = (1 to n).map(_ => io.StdIn.readLine.split(" ").map(_.toInt)).sortBy(_ (0))

      @scala.annotation.tailrec
      def loop(i: Int = 0, covered: Int = 0, acc: Int = 0): Int = {
        if (i == n) acc
        else {
          val l = intervals(i)(0)
          val r = intervals(i)(1)

          val req = (r - (l max covered)) / k
          val (coveredU, accU) = {
            val temp = (l max covered) + req * k

            if (temp >= r) (temp, acc + req) else (temp + k, acc + req + 1)
          }
          loop(i + 1, coveredU, accU)
        }
      }

      printFormattedOutput(t, loop())
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
