object Mangoes {

  case class pair(a: Long, h: Long) {
    def value(p: Int): Long = a + (p - 1) * h
  }

  def main(args: Array[String]): Unit = {
    val Array(n, max) = io.StdIn.readLine().split(" ").map(_.toLong)

    val appetite = io.StdIn.readLine().split(" ").map(_.toLong)
    val happiness = io.StdIn.readLine().split(" ").map(_.toLong)

    var elements = appetite.zip(happiness).map { case (a, h) => pair(a, h) }

    if (elements.map(_.value(elements.length)).sum <= max) {
      println(elements.length)
      return
    }

    var l = 0
    var r = elements.length

    while (l < r) {
      val m = (l + r) / 2
      elements = elements.sortBy(_.value(m))
      val s = elements.slice(0, m).map(_.value(m)).sum

      if (s > max) r = m else l = m + 1
    }

    println(l - 1)
  }

}
