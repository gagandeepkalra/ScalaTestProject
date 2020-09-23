package codeforces.contests._1407

/*
https://codeforces.com/contest/1407/problem/C

Interactive problem, Scala fails me :(

The solution is done in 2*n -2 asks, trivial

todo: Doesn't pass
 */
object ChocolateBunny {

  import java.io._
  import java.util.StringTokenizer

  class FastReader() {
    var br: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    var st: StringTokenizer = null

    def next: String = {
      if (st == null || !st.hasMoreElements) st = new StringTokenizer(br.readLine)
      st.nextToken
    }

    def nextInt: Int = next.toInt
  }

  val reader = new FastReader
  val writer = new BufferedWriter(new OutputStreamWriter(System.out))

  def ask(i: Int, j: Int): Int = {
    writer.write(s"? $i $j\n")
    writer.flush()
    val res = reader.nextInt
    if (res == -1) System.exit(0)
    res
  }

  def main(args: Array[String]): Unit = {

    val n = reader.nextInt

    val arr = new Array[Int](n + 1)

    @scala.annotation.tailrec
    def loop(i: Int = 1, j: Int = 2): Unit = {
      if (j > n) {
        arr(i) = n
      } else {
        val iModj = ask(i, j)
        val jModi = ask(j, i)

        if (iModj > jModi) {
          arr(i) = iModj
          loop(j, j + 1)
        } else { // if (jModi > iModj) {
          arr(j) = jModi
          loop(i, j + 1)
        }
      }
    }

    loop()

    writer.write("! " + arr.view(1, n + 1).mkString(" "))
    writer.flush()
  }

}
