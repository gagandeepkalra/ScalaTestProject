import scala.collection.mutable

object SkiingInSingapore {

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }.asInstanceOf[I => O]

  case class Solution(start: Int, end: Int, len: Int) extends Ordered[Solution] {
    override def compare(that: Solution): Int =
      if (this.len == that.len) {
        that.end - this.end
      } else this.len - that.len
  }

  def main(args: Array[String]): Unit = {
    val delta = List((-1, 0), (0, 1), (1, 0), (0, -1))

    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)
    val grid = Array.ofDim[Int](n, m)
    (0 until n).foreach(i => {
      grid(i) = io.StdIn.readLine.split(" ").map(_.toInt)
    })

    def isValid: (Int, Int) => Boolean = (x, y) => x >= 0 && x < n && y >= 0 && y < m

    lazy val update: ((Int, Int)) => Solution = memoize[(Int, Int), Solution] {
      case (i, j) =>
        val cell = grid(i)(j)
        val solutions: List[Solution] = delta
          .map(d => (i + d._1, j + d._2))
          .filter { case (x, y) => isValid(x, y) && cell > grid(x)(y) }
          .map(update)

        val bestSolution = if (solutions.nonEmpty) solutions.max else Solution(cell, cell, 0)
        Solution(cell, bestSolution.end, bestSolution.len + 1)
    }

    val result = {
      for {
        i <- 0 until n
        j <- 0 until m
      } yield update(i, j)
    }.max

    println(result.len + "" + (result.start - result.end))
  }
}
