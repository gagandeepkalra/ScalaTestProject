package GoogleCodeJam2018RoundH

object Mural {
  def main(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt).foreach(t => {

      val n = io.StdIn.readInt
      val beauty = io.StdIn.readLine

      val window = (n + 1) / 2

      var result = beauty.take(window).map(c =>  c - '0' ).sum
      var sum = result
      var i = 1

      while (i + window <= n) {
        sum -= beauty(i - 1) - '0'
        sum += beauty(i + window - 1) - '0'

        result = result max sum
        i += 1
      }

      println(s"Case #$t: " + result)
    })
  }
}
