object SuperDigit {
  def main(args: Array[String]): Unit = {
    val (x, y) = scala.io.StdIn.readf2("{0} {1}")

    val sum: Long = x.asInstanceOf[String].foldLeft[Long](0l)((res, c) => res + c - '0') * y.toString.toInt

    println(super_digit(sum))
  }

  def super_digit(x: Long): Int = {
    if (x <= 9) x.toInt
    else super_digit(x.toString.foldLeft[Long](0l)((res, c) => res + c - '0'))
  }
}
