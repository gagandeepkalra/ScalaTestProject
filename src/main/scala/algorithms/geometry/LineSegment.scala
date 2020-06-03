package algorithms.geometry

import scala.collection.mutable

case class LineSegment(x1: Double, y1: Double, x2: Double, y2: Double) {

  import LineSegment.EPSILON

  def getY(x: Double): Double = if (math.abs(x1 - x2) < EPSILON) y2 else y1 + (x - x1) * (y2 - y1) / (x2 - x1)

  def intersect(that: LineSegment): Boolean = {
    val x3 = that.x1
    val y3 = that.y1
    val x4 = that.x2
    val y4 = that.y2

    intersectOneD(x1, x2, x3, x4) && intersectOneD(y1, y2, y3, y4) &&
      orientation(x1, y1, x2, y2, x3, y3) * orientation(x1, y1, x2, y2, x4, y4) <= 0 &&
      orientation(x3, y3, x4, y4, x1, y1) * orientation(x3, y3, x4, y4, x2, y2) <= 0
  }

  private def intersectOneD(a: Double, b: Double, c: Double, d: Double): Boolean = {
    val aa = a min b
    val bb = a max b
    val cc = c min d
    val dd = c max d

    aa.max(cc) <= bb.min(dd) + EPSILON
  }

  private def orientation(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double): Double = {
    val value = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)

    if (math.abs(value) < EPSILON) 0 else math.signum(value) // 0 => collinear, 1 => clockwise, -1 => anti-clockwise
  }
}

object LineSegment {
  val EPSILON: Double = 1E-9;

  implicit val ordering: Ordering[LineSegment] = (l1: LineSegment, l2: LineSegment) => {
    val x = (l1.x1 min l1.x2) max (l2.x1 min l2.x2)
    val o = l1.getY(x) - l2.getY(x)

    if (math.abs(o) < EPSILON) 0 else if (o < 0) -1 else 1
  }

  def doIntersect(lines: Array[LineSegment]): Boolean = {

    val abscissas: mutable.TreeSet[(Double, Int, Int)] = mutable.TreeSet.empty[(Double, Int, Int)]
    lines.indices.foreach { i =>
      val lineSegment = lines(i)
      abscissas += ((lineSegment.x1 min lineSegment.x2, 1, i))
      abscissas += ((lineSegment.x1 max lineSegment.x2, 2, i))
    }

    val ordinates = mutable.TreeSet.empty[LineSegment]

    // line should exist
    def next(l: LineSegment): Option[LineSegment] = {
      val itr = ordinates.iteratorFrom(l)
      itr.next()
      if (itr.hasNext) Some(itr.next()) else None
    }

    // line should exist
    def prev(l: LineSegment): Option[LineSegment] = {
      ordinates.until(l).lastOption
    }

    abscissas.iterator.exists { case (_, event, i) =>
      val line = lines(i)
      event match {
        case 1 =>
          ordinates(line) || {
            ordinates += line
            val p = prev(line).exists(_.intersect(line))
            val n = next(line).exists(_.intersect(line))
            p || n
          }
        case 2 =>
          val ans = (for {
            p <- prev(line)
            n <- next(line)
          } yield p intersect n).getOrElse(false)
          ordinates -= line
          ans
      }
    }
  }
}
