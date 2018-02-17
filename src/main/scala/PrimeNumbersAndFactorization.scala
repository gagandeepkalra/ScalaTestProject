import scala.collection.mutable.ListBuffer

object PrimeNumbersAndFactorization {


  def main(args: Array[String]): Unit = {
    val n: Int = scala.io.StdIn.readInt
    (1 to n).foreach { _ =>
      var res = true
      var r = 0
      var g = 0
      var b = 0
      var y = 0
      scala.io.StdIn.readLine.foreach {
        c =>
          c match {
            case 'R' => r += 1
            case 'G' => g += 1
            case 'B' => b += 1
            case 'Y' => y += 1
          }
          if (math.abs(r - g) > 1 || math.abs(b - y) > 1) res = false
      }
      if (r == g && b == y && res) println("True") else println("False")
    }
  }

  def gcdFactorised(args: Array[String]): Unit = {
    val n: Int = scala.io.StdIn.readInt

    var resultMap = io.StdIn.readLine.split(" ").map(_.toInt).grouped(2).map(e => (e.head, e.last)).toMap

    (1 until n).foreach { _ =>
      val tempMap = io.StdIn.readLine.split(" ").map(_.toInt).grouped(2).map(e => (e.head, e.last)).toMap

      resultMap = resultMap.keySet.intersect(tempMap.keySet).map(key => (key, math.min(resultMap(key), tempMap(key)))).toMap
    }

    resultMap.keySet.toSeq.sorted.foreach(key => print(key + " " + resultMap(key) + " "))
  }


  def countDivisors(n: Int): Int = {
    (1 to math.sqrt(n).floor.toInt).filter(n % _ == 0).map(i => if (i * i == n) 1 else 2).sum
  }

  def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  def hugeGcd(): Unit = {

    val primes: Array[Int] = computePrimes(10000)

    val n: Int = scala.io.StdIn.readInt
    val A: Array[Int] = scala.io.StdIn.readLine.split(" ").map(_.toInt)
    val factorsA = new ListBuffer[Int]()

    for (a <- A) {
      getFactors(a, primes).foreach(factorsA += _)
    }

    val m: Int = scala.io.StdIn.readInt
    val B: Array[Int] = scala.io.StdIn.readLine.split(" ").map(_.toInt)
    val factorsB = new ListBuffer[Int]

    for (b <- B) {
      getFactors(b, primes).foreach(factorsB += _)
    }

    println(factorsA.intersect(factorsB).foldLeft[Long](1l)((acc, i) => (acc * i) % 1000000007))
  }

  def computePrimes(n: Int): Array[Int] = {
    val factors = new Array[Int](n + 1)

    (2 to n).foreach { i =>
      if (factors(i) == 0) {
        (2 * i to n by i).foreach { j =>
          factors(j) = i
        }
      }
    }

    factors
  }

  def getFactors(n: Int, factors: Array[Int]): List[Int] = {
    val ls: ListBuffer[Int] = ListBuffer[Int]()

    var i = n
    while (factors(i) != 0) {
      ls += factors(i)
      i = i / factors(i)
    }
    ls += i
    ls.toList
  }
}
