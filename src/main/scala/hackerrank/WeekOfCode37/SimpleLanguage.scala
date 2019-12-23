package hackerrank.WeekOfCode37

object SimpleLanguage {

  def main(args: Array[String]): Unit = {

    var ans: Long = 0

    (1 to io.StdIn.readInt).foreach(_ => {
      io.StdIn.readLine.split(" ").grouped(2).map(x => (x.head, x.last.toInt)).foreach {
        case (cmd: String, i: Int) => cmd match {
          case "add" => if (i > 0) ans += i
          case "set" => if (i > ans) ans = i
        }
      }
    })

    println(ans)
  }
}
