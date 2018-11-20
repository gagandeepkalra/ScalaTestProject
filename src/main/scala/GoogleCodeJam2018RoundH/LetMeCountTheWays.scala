package GoogleCodeJam2018RoundH

/*

https://www.careerbless.com/qna/discuss.php?questionid=530
Inclusion Exclusion Principle

 */
object LetMeCountTheWays {

  val MOD = 1000000007
  val MAX = 200002

  val factorialOf: Array[Long] = new Array[Long](MAX)
  factorialOf(0) = 1

  val powerOf2: Array[Long] = new Array[Long](MAX)
  powerOf2(0) = 1

  for (i <- 1 until MAX) {
    factorialOf(i) = (i * factorialOf(i - 1)) % MOD
    powerOf2(i) = (2 * powerOf2(i - 1)) % MOD
  }

  def power(x: Long, y: Long): Long = {
    if (x == 0) 0
    else {
      var ans: Long = 1
      var temp: Long = x

      var i = y
      while (i > 0) {
        if ((i & 1) == 1) {
          ans = (ans * temp) % MOD
        }
        temp = (temp * temp) % MOD
        i = i / 2
      }

      ans
    }
  }

  def inverseMod(a: Long): Long = {
    power(a, MOD - 2)
  }

  def nCr(n: Int, r: Int): Long = {
    (((factorialOf(n) * inverseMod(factorialOf(r))) % MOD) * inverseMod(factorialOf(n - r))) % MOD
  }

  def add(x: Long, y: Long): Long = {
    val r = (x + y) % MOD
    if (r < 0) r + MOD else r
  }

  def mult(x: Long, y: Long): Long = {
    (x * y) % MOD
  }

  def main(args: Array[String]): Unit = {

    (1 to io.StdIn.readInt()).foreach(t => {

      val Array(n, m) = io.StdIn.readLine().split(" ").map(_.toInt)

      val permutations = factorialOf(2 * n)

      val permutationsWhenAtleastOneOfMCoupleSitsTogether = (1 to m).foldLeft[Long](0) {
        (acc, i) =>
          val r = mult(mult(factorialOf(2 * n - i), powerOf2(i)), nCr(m, i)) * (if ((i - 1) % 2 == 0) 1 else -1)
          add(acc, r)
      }

      println(s"Case #$t: " + add(permutations, -permutationsWhenAtleastOneOfMCoupleSitsTogether))

    })
  }
}
