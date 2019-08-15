/**
 * Given a series of intervals, count the number that overlaps
 */
object ConfilictingIntervals {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val pairs = (1 to n)
      .map { _ =>
        val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
        (x, y)
      }
      .sorted
      .toList

    @scala.annotation.tailrec
    def loop(ls: List[(Int, Int)],
             maxR: Int,
             carriage: Int,
             result: Int): Int = {
      ls match {
        case Nil => result
        case (l, r) :: tail =>
          loop(
            tail,
            maxR max r,
            if (l >= maxR) 1 else 0,
            result + (if (l < maxR) carriage + 1 else 0)
          )
      }
    }

    println(loop(pairs, 0, 0, 0))
  }

}
