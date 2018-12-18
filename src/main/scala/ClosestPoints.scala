/*

Find closest distance between points from a given set of 2-d coordinates

 */
object ClosestPoints {


  def divide(pts: IndexedSeq[(Int, Int)], l: Int, r: Int): Int = { // exclusive
    if (r - l <= 3) bruteForce(pts.slice(l, r))
    else {
      val m = (l + r) / 2
      val lR = divide(pts, l, m)
      val rR = divide(pts, m, r)

      val res = lR min rR

      val t = pts.view.slice(l, r).filter { case (x, _) => math.abs(x - pts(m)._1) <= res }.sortBy(_._2).toIndexedSeq

      conquer(t, res)
    }
  }

  def conquer(pts: IndexedSeq[(Int, Int)], res: Int): Int = {
    var i, j = 0
    var result = res

    while (i < pts.length) {
      j = i + 1
      while (j < pts.length && (pts(j)._2 - pts(i)._2) < res) {
        result = result min distance(pts(j), pts(i))
        j += 1
      }
      i += 1
    }
    result
  }

  def distance(first: (Int, Int), second: (Int, Int)): Int = {
    math.abs(first._1 - second._1) max math.abs(first._2 - second._2)
  }

  def bruteForce(pts: IndexedSeq[(Int, Int)]): Int = pts.length match {
    case 2 => distance(pts(0), pts(1))
    case 3 => distance(pts(0), pts(1)) min (distance(pts(1), pts(2)) min distance(pts(2), pts(0)))
  }

  def solution(x: Array[Int], y: Array[Int]): Int = {
    val s = x.indices.map(i => (x(i), y(i))).sorted.toArray
    divide(s, 0, x.length)/2
  }

  def main(args: Array[String]): Unit = {
    println(solution(Array(0, 0, 10, 10, 5), Array(0, 10, 0, 10, 5)))
  }
}
