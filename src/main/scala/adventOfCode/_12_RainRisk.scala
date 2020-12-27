package adventOfCode

import scala.annotation.tailrec


/**
 *
 * part: 01
 *
 * Action N means to move north by the given value.
 * Action S means to move south by the given value.
 * Action E means to move east by the given value.
 * Action W means to move west by the given value.
 * Action L means to turn left the given number of degrees.
 * Action R means to turn right the given number of degrees.
 * Action F means to move forward by the given value in the direction the ship is currently facing.
 *
 * part: 02
 *
 * Almost all of the actions indicate how to move a waypoint which is relative to the ship's position:
 *
 * Action N means to move the waypoint north by the given value.
 * Action S means to move the waypoint south by the given value.
 * Action E means to move the waypoint east by the given value.
 * Action W means to move the waypoint west by the given value.
 * Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
 * Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
 * Action F means to move forward to the waypoint a number of times equal to the given value.
 */
object _12_RainRisk {

  val pattern = "^([NSEWLRF])(\\d+)$".r

  @tailrec
  def rotateRight(dx: Int, dy: Int, times: Int = 1): (Int, Int) = {
    if (times == 0) (dx, dy) else rotateRight(dy, -dx, times - 1)
  }

  @tailrec
  def translate_part_01(
      instructions: List[String],
      x: Int,
      y: Int,
      dx: Int,
      dy: Int
  ): (Int, Int) = {
    instructions match {
      case Nil => (x, y)
      case pattern("N", v) :: tail =>
        translate_part_01(tail, x, y + v.toInt, dx, dy)
      case pattern("S", v) :: tail =>
        translate_part_01(tail, x, y - v.toInt, dx, dy)
      case pattern("E", v) :: tail =>
        translate_part_01(tail, x + v.toInt, y, dx, dy)
      case pattern("W", v) :: tail =>
        translate_part_01(tail, x - v.toInt, y, dx, dy)
      case pattern("L", "90") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 3)
        translate_part_01(tail, x, y, ddx, ddy)
      case pattern("L", "180") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 2)
        translate_part_01(tail, x, y, ddx, ddy)
      case pattern("L", "270") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 1)
        translate_part_01(tail, x, y, ddx, ddy)
      case pattern("R", "90") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 1)
        translate_part_01(tail, x, y, ddx, ddy)
      case pattern("R", "180") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 2)
        translate_part_01(tail, x, y, ddx, ddy)
      case pattern("R", "270") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 3)
        translate_part_01(tail, x, y, ddx, ddy)
      case pattern("F", v) :: tail =>
        translate_part_01(tail, x + dx * v.toInt, y + dy * v.toInt, dx, dy)
    }
  }

  @tailrec
  def translate_part_02(
      instructions: List[String],
      x: Int,
      y: Int,
      dx: Int,
      dy: Int
  ): (Int, Int) = {
    instructions match {
      case Nil => (x, y)
      case pattern("N", v) :: tail =>
        translate_part_02(tail, x, y, dx, dy + v.toInt)
      case pattern("S", v) :: tail =>
        translate_part_02(tail, x, y, dx, dy - v.toInt)
      case pattern("E", v) :: tail =>
        translate_part_02(tail, x, y, dx + v.toInt, dy)
      case pattern("W", v) :: tail =>
        translate_part_02(tail, x, y, dx - v.toInt, dy)
      case pattern("L", "90") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 3)
        translate_part_02(tail, x, y, ddx, ddy)
      case pattern("L", "180") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 2)
        translate_part_02(tail, x, y, ddx, ddy)
      case pattern("L", "270") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 1)
        translate_part_02(tail, x, y, ddx, ddy)
      case pattern("R", "90") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 1)
        translate_part_02(tail, x, y, ddx, ddy)
      case pattern("R", "180") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 2)
        translate_part_02(tail, x, y, ddx, ddy)
      case pattern("R", "270") :: tail =>
        val (ddx, ddy) = rotateRight(dx, dy, 3)
        translate_part_02(tail, x, y, ddx, ddy)
      case pattern("F", v) :: tail =>
        translate_part_02(tail, x + dx * v.toInt, y + dy * v.toInt, dx, dy)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = """F99
                  |L180
                  |W1
                  |W3
                  |R90
                  |E5
                  |R180
                  |S4
                  |F55
                  |L90
                  |E5
                  |S3
                  |R180
                  |N2
                  |W3
                  |S1
                  |F64
                  |W4
                  |F76
                  |N2
                  |F7
                  |L180
                  |S2
                  |E5
                  |S2
                  |F87
                  |N4
                  |L90
                  |F46
                  |R90
                  |F47
                  |W5
                  |N4
                  |L270
                  |N5
                  |F89
                  |L180
                  |W2
                  |N5
                  |F69
                  |E3
                  |L90
                  |F73
                  |L90
                  |N1
                  |F28
                  |N4
                  |F72
                  |L90
                  |F24
                  |R90
                  |S1
                  |F52
                  |L90
                  |W2
                  |F39
                  |E1
                  |L180
                  |S4
                  |W1
                  |S2
                  |F61
                  |E3
                  |F37
                  |S1
                  |W3
                  |F8
                  |S5
                  |L90
                  |N5
                  |E2
                  |L180
                  |F44
                  |W3
                  |S1
                  |R90
                  |F88
                  |N1
                  |F75
                  |S2
                  |W2
                  |F37
                  |N4
                  |E3
                  |R90
                  |F47
                  |N5
                  |W5
                  |L90
                  |S4
                  |E5
                  |F7
                  |R90
                  |E5
                  |R90
                  |W2
                  |F56
                  |S2
                  |R270
                  |S1
                  |W3
                  |S2
                  |W2
                  |F79
                  |W4
                  |N5
                  |W3
                  |F84
                  |L90
                  |N5
                  |E2
                  |F48
                  |E1
                  |L180
                  |E4
                  |L90
                  |W4
                  |E4
                  |S4
                  |W4
                  |F36
                  |F93
                  |L180
                  |W5
                  |S1
                  |E5
                  |F96
                  |N3
                  |E3
                  |N2
                  |R270
                  |W5
                  |L180
                  |F24
                  |W5
                  |S5
                  |R180
                  |W1
                  |N5
                  |F6
                  |R90
                  |E1
                  |F52
                  |F77
                  |W4
                  |F34
                  |S3
                  |R90
                  |E1
                  |E5
                  |N5
                  |E1
                  |S5
                  |R90
                  |W5
                  |N4
                  |E3
                  |W3
                  |L90
                  |E2
                  |L90
                  |W3
                  |W5
                  |F49
                  |N1
                  |F48
                  |W3
                  |N4
                  |E4
                  |F100
                  |E1
                  |R90
                  |F25
                  |S1
                  |W2
                  |N2
                  |W1
                  |F25
                  |L90
                  |E2
                  |F96
                  |W2
                  |S1
                  |S4
                  |F91
                  |N2
                  |R90
                  |W4
                  |L90
                  |E4
                  |F78
                  |L90
                  |S1
                  |W3
                  |F56
                  |R90
                  |W1
                  |E4
                  |L90
                  |N5
                  |E1
                  |R90
                  |F53
                  |E5
                  |L90
                  |E2
                  |F82
                  |E4
                  |L90
                  |W2
                  |L180
                  |F51
                  |R270
                  |F37
                  |N5
                  |F15
                  |E4
                  |F16
                  |E2
                  |S1
                  |E4
                  |F91
                  |N4
                  |E5
                  |N5
                  |L90
                  |F9
                  |W2
                  |F64
                  |S4
                  |F72
                  |W2
                  |F31
                  |S4
                  |R90
                  |F40
                  |W3
                  |R90
                  |F50
                  |S1
                  |F61
                  |W3
                  |F90
                  |N5
                  |F76
                  |S1
                  |L90
                  |E3
                  |R180
                  |F19
                  |L90
                  |W3
                  |F70
                  |E3
                  |F35
                  |R90
                  |L90
                  |N1
                  |W5
                  |R180
                  |E2
                  |N5
                  |F34
                  |W4
                  |S1
                  |E1
                  |N5
                  |E1
                  |N2
                  |R90
                  |W4
                  |S1
                  |L180
                  |E5
                  |F59
                  |E1
                  |R90
                  |S2
                  |L90
                  |S4
                  |R90
                  |W2
                  |N5
                  |F60
                  |W1
                  |R90
                  |F35
                  |R270
                  |W5
                  |F100
                  |W2
                  |R90
                  |N2
                  |S5
                  |S3
                  |E1
                  |S2
                  |F36
                  |W1
                  |F90
                  |R90
                  |S3
                  |E3
                  |F5
                  |S4
                  |R90
                  |E3
                  |R90
                  |N3
                  |L90
                  |S1
                  |F74
                  |S2
                  |R180
                  |E4
                  |S5
                  |F13
                  |S3
                  |E2
                  |F92
                  |N5
                  |F2
                  |N4
                  |F3
                  |E2
                  |L180
                  |W3
                  |N3
                  |L90
                  |E4
                  |F21
                  |W1
                  |F76
                  |W5
                  |F56
                  |E4
                  |R180
                  |F100
                  |E1
                  |F29
                  |L90
                  |F96
                  |N2
                  |F43
                  |R90
                  |F26
                  |N3
                  |F15
                  |L180
                  |E3
                  |R180
                  |N3
                  |E3
                  |R90
                  |F7
                  |L90
                  |W2
                  |F33
                  |R90
                  |E5
                  |S4
                  |E2
                  |E3
                  |F34
                  |F66
                  |N4
                  |F14
                  |W3
                  |N2
                  |R270
                  |F66
                  |E4
                  |S5
                  |W5
                  |R90
                  |S5
                  |W5
                  |L90
                  |S4
                  |L90
                  |F69
                  |E5
                  |R90
                  |W5
                  |S5
                  |W4
                  |R90
                  |N3
                  |E4
                  |F14
                  |W3
                  |F24
                  |S3
                  |F48
                  |L180
                  |E1
                  |R90
                  |F36
                  |N4
                  |L90
                  |F68
                  |E4
                  |F59
                  |E5
                  |R90
                  |N2
                  |E4
                  |N5
                  |F56
                  |R90
                  |R90
                  |F91
                  |W3
                  |F24
                  |L90
                  |F56
                  |S5
                  |F59
                  |S4
                  |F36
                  |N1
                  |W3
                  |R270
                  |L90
                  |F86
                  |L180
                  |F33
                  |N5
                  |E4
                  |L90
                  |F14
                  |S3
                  |L90
                  |N5
                  |L270
                  |E5
                  |S4
                  |L180
                  |W5
                  |N2
                  |L90
                  |E3
                  |F69
                  |R90
                  |S5
                  |R270
                  |N3
                  |F70
                  |E2
                  |N2
                  |F3
                  |R90
                  |S5
                  |L180
                  |F59
                  |W3
                  |F67
                  |R180
                  |F74
                  |E4
                  |F4
                  |L90
                  |N4
                  |R90
                  |S5
                  |F6
                  |S1
                  |E3
                  |N4
                  |W3
                  |F24
                  |E1
                  |S1
                  |E4
                  |S2
                  |F82
                  |N3
                  |L90
                  |E1
                  |S2
                  |R270
                  |N4
                  |E1
                  |N3
                  |F32
                  |R270
                  |W4
                  |R90
                  |E4
                  |F33
                  |N5
                  |W3
                  |F34
                  |E2
                  |S1
                  |W3
                  |W4
                  |E2
                  |L90
                  |N5
                  |W3
                  |S1
                  |F86
                  |E4
                  |F99
                  |S1
                  |L90
                  |S2
                  |E2
                  |L90
                  |F100
                  |N5
                  |F19
                  |L180
                  |W1
                  |F25
                  |N3
                  |F25
                  |R90
                  |W4
                  |N4
                  |R180
                  |N2
                  |E5
                  |R90
                  |F66
                  |N5
                  |E2
                  |L180
                  |L90
                  |E1
                  |S2
                  |R90
                  |W3
                  |S1
                  |S2
                  |L90
                  |F93
                  |L90
                  |W2
                  |R270
                  |F73
                  |N4
                  |L90
                  |F44
                  |R90
                  |N1
                  |W3
                  |R90
                  |F69
                  |N5
                  |L90
                  |N1
                  |R90
                  |F35
                  |F89
                  |E4
                  |F31
                  |W3
                  |R270
                  |W1
                  |N2
                  |E5
                  |L90
                  |W4
                  |W1
                  |F93
                  |E4
                  |F18
                  |N4
                  |F31
                  |W5
                  |N4
                  |L90
                  |N1
                  |E2
                  |R270
                  |F34
                  |N5
                  |W2
                  |L270
                  |W3
                  |F44
                  |S3
                  |F47
                  |W2
                  |F86
                  |N4
                  |R90
                  |S5
                  |R90
                  |F4
                  |E3
                  |L180
                  |W5
                  |R180
                  |F89
                  |L180
                  |F46
                  |N3
                  |F76
                  |R270
                  |S2
                  |F62
                  |R90
                  |S2
                  |F28
                  |R180
                  |F47
                  |E4
                  |N4
                  |R90
                  |S4
                  |E3
                  |R180
                  |N1
                  |F92
                  |R180
                  |F86
                  |N3
                  |R270
                  |F47
                  |E4
                  |S4
                  |W2
                  |F67
                  |N5
                  |W3
                  |N1
                  |F98
                  |S1
                  |E4
                  |N3
                  |F97
                  |E3
                  |F69
                  |E3
                  |F38
                  |E3
                  |S4
                  |E5
                  |F81
                  |E3
                  |F5
                  |E3
                  |R90
                  |W2
                  |N4
                  |W3
                  |F94
                  |L180
                  |F30
                  |N1
                  |F91
                  |S3
                  |F89
                  |E1
                  |F68
                  |N5
                  |E5
                  |F89
                  |L90
                  |W1
                  |N5
                  |F79
                  |R90
                  |F44
                  |E4
                  |R90
                  |S1
                  |L90
                  |E5
                  |N3
                  |R90
                  |W3
                  |N4
                  |F83
                  |E5
                  |S4
                  |F82
                  |S2
                  |F29
                  |R90
                  |S1
                  |R180
                  |F49
                  |E5
                  |N1
                  |L180
                  |R180
                  |S3
                  |L180
                  |S3
                  |F55
                  |S3
                  |F70
                  |W5
                  |N2
                  |W2
                  |R90
                  |W1
                  |F93
                  |L180
                  |L90
                  |R90
                  |L90
                  |E3
                  |L90
                  |S3
                  |R90
                  |W5
                  |N4
                  |R90
                  |S1
                  |R90
                  |W5
                  |F100
                  |W4
                  |N2
                  |F84
                  |F76
                  |E5
                  |L180
                  |S4
                  |F85
                  |R90
                  |L90
                  |F68
                  |R90
                  |S4
                  |W2
                  |L90
                  |E1
                  |F19
                  |N2
                  |L90
                  |N4
                  |E5
                  |N1
                  |L90
                  |F75
                  |F42
                  |R90
                  |S5
                  |E4
                  |N1
                  |E4
                  |F7
                  |N5
                  |L180
                  |N4
                  |E5
                  |L270
                  |F6
                  |R180
                  |W5
                  |F93
                  |L180
                  |S1
                  |R90
                  |F66
                  |N4
                  |F83
                  |N1
                  |F10
                  |S2
                  |L90
                  |F80
                  |W1
                  |R180
                  |E2
                  |L90
                  |S4
                  |F53""".stripMargin.split("\n")

    println {
      val (x, y) = translate_part_01(input.toList, 0, 0, 1, 0)
      x.abs + y.abs
    }

    println {
      val (x, y) = translate_part_02(input.toList, 0, 0, 10, 1)
      x.abs + y.abs
    }
  }
}
