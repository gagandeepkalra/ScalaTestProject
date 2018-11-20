package GoogleCodeJam2018RoundH

object BigButtons {
  def main(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt()).foreach(t => {

      val Array(n, p) = io.StdIn.readLine.split(" ").map(_.toInt)
      val strSet: Set[String] = (0 until p).foldLeft(Set[String]())((res, _) => res + io.StdIn.readLine)

      val result = strSet.foldLeft(math.pow(2, n).toLong)((acc, elem) => {
        strSet.find(s => !s.equals(elem) && elem.startsWith(s)) match {
          case Some(_) => acc // I start with somebody
          case None => acc - math.pow(2, n - elem.length).toLong
        }
      })

      println(s"Case #$t: " + result.max(0))

    })
  }
}
