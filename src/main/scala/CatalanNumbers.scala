object CatalanNumbers {

  val catalan = new Array[Long](1001)
  catalan(0) = 1
  catalan(1) = 1

  val MOD = 100000007

  def compute(n: Int): Long = {
    if (n == 0 || catalan(n) != 0) return catalan(n)

    var result: Long = 0
    for (i <- 1 to n) {
      result = (result + compute(i - 1) * compute(n - i)) % MOD
    }
    catalan(n) = result
    result
  }

  def main(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt()).foreach(_ => println(compute(io.StdIn.readInt)))
  }
}