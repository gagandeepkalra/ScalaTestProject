object CodeJamQ1 {
  def main(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt).foreach(j => {
      val str = io.StdIn.readLine

      var index = 0
      while (index < str.length && str.charAt(index).toInt % 2 == 0) index += 1

      if (index != str.length) {
        val remaining = str.length - index - 1

        val org: String = str.substring(0, index) + "0" * (remaining + 1)

        val x = str.charAt(index) - '0'

        val inc: String = (if (x == 9) 20 else x + 1) + "0" * remaining
        val dec: String = (x - 1) + "8" * remaining

        println(s"Case #$j: " + math.min(org.toLong + inc.toLong - str.toLong, str.toLong - (org.toLong + dec.toLong)))
      } else println(s"Case #$j: 0")


    })
  }
}
