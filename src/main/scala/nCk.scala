object nCk {

  def input(args: Array[String]): Unit = {
    var arr = Array.ofDim[Int](1001, 1001) // n- row, k- column

    for (i <- 1 to 1000;
         j <- 0 to i) {
      if (j == i || j == 0) arr(i)(j) = 1 else arr(i)(j) = (arr(i - 1)(j - 1) + arr(i - 1)(j)) % 100000007
    }

    (0 until io.StdIn.readInt).foreach(_ => {
      io.StdIn.readLine.split(" ").map(_.toInt).grouped(2).foreach(x => {
        println(arr(x.head)(x.last))
      })
    })
  }

  def main(args: Array[String]): Unit = {
    io.StdIn.readInt
    val A: Map[Int, Int] = io.StdIn.readLine.split(" ").map(_.toInt).groupBy(identity).mapValues(_.size)

    io.StdIn.readInt
    val B: Map[Int, Int] = io.StdIn.readLine.split(" ").map(_.toInt).groupBy(identity).mapValues(_.size)

    B.map { case (k, v) => (k, v - A.getOrElse(k, 0)) }.filter(_._2 != 0).keys.toArray.sorted.foreach(i => print(i + " "))
  }

}
