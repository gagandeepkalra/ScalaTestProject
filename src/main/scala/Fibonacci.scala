object Fibonacci {

  def fibonacci(n: Int): Int = {
    def fib(n: Int, a: Int, b: Int): Int = {
      n match {
        case 1 => a
        case 2 => b
        case _ => fib(n - 1, b, a + b)
      }
    }

    fib(n, 0, 1)
  }

  def computeFibonacci(n: Int, mod: Int): Array[Int] = {
    val arr = new Array[Int](n + 1)

    arr(0) = 0
    arr(1) = 1

    (2 to n).foreach {
      i => arr(i) = (arr(i - 1) + arr(i - 2)) % mod
    }

    arr
  }

  def input2(args: Array[String]): Unit = {
    val arr = computeFibonacci(1001, 100000007)
    (1 to readInt()).foreach(_ => println(arr(readInt())))
  }

  def input() {
    val n: Int = io.StdIn.readInt()
    println(fibonacci(n))

  }

  def main(args: Array[String]): Unit = {

    val arr = new Array[Long](100001)

    arr(1) = 1
    arr(2) = 5
    (3 to 100000).par.foreach(i => {
      arr(i) = 2 * arr(i - 1) - arr(i - 2) + 3
    })

    def asUnsigned(unsignedLong: Long) =
      (BigInt(unsignedLong >>> 1) << 1) + (unsignedLong & 1)

    (0 until scala.io.StdIn.readInt).foreach(_ => println(asUnsigned(arr(io.StdIn.readInt))))
  }
}
