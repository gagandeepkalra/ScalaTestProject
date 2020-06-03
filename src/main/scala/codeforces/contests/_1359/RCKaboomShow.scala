package codeforces.contests._1359

/*
https://codeforces.com/contest/1359/problem/F

[Precision, Line Sweep]

We use a sweeping line across the x axis with discrete events, start and end of line segments. Meanwhile, we maintain
the order of line segments under consideration, if the order of lines changes after a event, this would mean that the
segments intersect, we stop after the first intersection (though we can also generate a list)

Refer, https://cp-algorithms.com/geometry/intersecting_segments.html
 */
object RCKaboomShow {

  import scala.collection.mutable

  val EPSILON: Double = 1E-9;

  case class Kinematics private(x: Int, y: Int, xDir: Double, yDir: Double, s: Int) {

    def lineSegment(time: Double): LineSegment = {
      LineSegment(x, y, x + s * xDir * time, y + s * yDir * time)
    }
  }

  object Kinematics {
    def apply(x: Int, y: Int, dx: Int, dy: Int, s: Int): Kinematics = {
      val hypot = math.hypot(dx, dy)
      val xDir: Double = dx / hypot
      val yDir: Double = dy / hypot
      new Kinematics(x, y, xDir, yDir, s)
    }
  }

  case class LineSegment(x1: Double, y1: Double, x2: Double, y2: Double) {
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
    implicit val ordering: Ordering[LineSegment] = (l1: LineSegment, l2: LineSegment) => {
      val x = (l1.x1 min l1.x2) max (l2.x1 min l2.x2)
      val o = l1.getY(x) - l2.getY(x)

      if (math.abs(o) < EPSILON) 0 else if (o < 0) -1 else 1
    }
  }

  def doIntersect(kinematics: Array[Kinematics], t: Double): Boolean = {
    val lines: Array[LineSegment] = kinematics.map(_.lineSegment(t))

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


  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val kinematics = {
      val arr = new Array[Kinematics](n)
      (0 until n).foreach { i =>
        val input = io.StdIn.readLine.split(" ").map(_.toInt)
        arr(i) = Kinematics(input(0), input(1), input(2), input(3), input(4))
      }
      arr
    }

    val ERROR = 1e-7

    @scala.annotation.tailrec
    def find(start: Double, end: Double): Double = {
      val t = start + (end - start) / 2

      if (math.abs(start - end) <= ERROR || start == t || t == end)
        t
      else if (doIntersect(kinematics, t))
        find(start, t)
      else
        find(t, end)
    }

    val dMax = 8e9 + 7

    println {
      if (doIntersect(kinematics, dMax))
        find(0.0, dMax)
      else
        "No show :("
    }

  }
}
