import java.util.Scanner
import scala.collection.mutable.ListBuffer

/**
 * https://www.hackerrank.com/challenges/convex-hull-fp/problem
 */
object PolygonPerimeter {

  case class Point(x: Int, y: Int) {
    def distance(a: Point) = math.hypot(x - a.x, y - a.y)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    val pts = Array.fill(sc.nextInt)(Point(sc.nextInt, sc.nextInt))

    if (isConcave(pts)) println("YES") else println("NO")
  }

  def isConcave(pts: Array[Point]): Boolean = {
    val p0: Point = pts.reduce((acc, pt) => math.signum(acc.y - pt.y) match {
      case 1 => pt
      case -1 => acc
      case 0 => if (acc.x < pt.x) acc else pt
    })

    val pts_sorted: List[Point] = p0 :: pts.filter(!_.equals(p0)).sortWith(compare(p0)).toList

    pts_sorted.sliding(3).map(ls => orientation(ls(0), ls(1), ls(2)) == 1).reduce[Boolean]((res, value) => res || value)
  }

  def orientation(p0: Point, p1: Point, p2: Point): Int = {
    val value = (p1.y - p0.y) * (p2.x - p1.x) - (p2.y - p1.y) * (p1.x - p0.x)
    if (value == 0) 0 else if (value > 0) 1 else 2 //collinear, clockwise anti-clockwise
  }

  def compare(p0: Point)(p1: Point, p2: Point): Boolean = { // is a < b
    val o = orientation(p0, p1, p2)
    if (o == 0) p1.distance(p0).<(p2.distance(p0)) else if (o == 1) false else true
  }

  def graham_scan(pts: Array[Point]): Double = {

    val p0: Point = pts.reduce((acc, pt) => math.signum(acc.y - pt.y) match {
      case 1 => pt
      case -1 => acc
      case 0 => if (acc.x < pt.x) acc else pt
    }) // leftmost bottommost point

    val pts_sorted: List[Point] = p0 :: pts.filter(!_.equals(p0)).sortWith(compare(p0)).toList


    val stack = scala.collection.mutable.Stack[Point]()
    (0 to 2).foreach(i => stack.push(pts_sorted(i)))

    (3 until pts_sorted.size).foreach(i => {
      var current = pts_sorted(i)
      var first = stack.pop
      var second = stack.top

      while (orientation(second, first, current) == 1 && stack.nonEmpty) {
        first = stack.pop
        second = stack.top
      }
      stack.push(first)
      stack.push(current)
    })

    val lB = ListBuffer[Point]()

    while (stack.nonEmpty) lB += stack.pop()

    perimeterPolygon(lB.toList, lB.size - 1)
  }

  def perimeterPolygon(pts: List[Point], current: Int): Double = {
    pts(current).distance(pts((current + 1) % pts.size)) + (if (current == 0) 0.0 else perimeterPolygon(pts, current - 1))
  }

  def areaPolygon(pts: List[Point]): Double = {
    pts.zipWithIndex.foldLeft[Double](0.0) {
      case (acc, (point, index)) => {
        val p1: Point = point
        val p2: Point = pts((index + 1) % pts.size)

        acc + p1.x * p2.y - p2.x * p1.y
      }
    } * 0.5
  }

  def areaTriangle(a: Point, b: Point, c: Point): Double = {
    math.abs(a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y)) * 0.5
  }


}