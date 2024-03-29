package adventOfCode

import scala.annotation.tailrec

/**
 * part: 01
 *
 * If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
 * If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
 * Otherwise, the seat's state does not change.
 *
 * part: 02
 *
 * Now, instead of considering just the eight immediately adjacent seats, consider the first seat in each of those eight directions.
 * It now takes five or more visible occupied seats for an occupied seat to become empty (rather than four or more from the previous rules).
 * The other rules still apply
 */
object _11_SeatingSystem {

  sealed trait State

  case object Floor extends State
  case object Occupied extends State
  case object Vacant extends State

  val delta = Vector(-1, 0, 1)

  def isValidIndex(x: Int, y: Int)(
    implicit current: Array[Array[State]]
  ): Boolean = 0 <= x && x < current.length && 0 <= y && y < current(0).length

  @tailrec
  def findFirstSeatInDirection(x: Int, y: Int, dx: Int, dy: Int)(
    implicit current: Array[Array[State]]
  ): Option[State] = {
    if (isValidIndex(x, y)) {
      current(x)(y) match {
        case Floor    => findFirstSeatInDirection(x + dx, y + dy, dx, dy)
        case rest @ _ => Some(rest)
      }
    } else None
  }

  def computeNextState_part_01(x: Int, y: Int)(
      implicit current: Array[Array[State]]
  ): State = {
    val occupiedSeats = {
      var count = 0

      for {
        dx <- delta
        dy <- delta
        if !(dx == 0 && dy == 0)
        if isValidIndex(x + dx, y + dy) && current(x + dx)(y + dy) == Occupied
      } count += 1

      count
    }

    current(x)(y) match {
      case Occupied if occupiedSeats >= 4 => Vacant
      case Occupied                       => Occupied
      case Vacant if occupiedSeats == 0   => Occupied
      case Vacant                         => Vacant
      case Floor                          => Floor
    }
  }

  def computeNextState_part_02(x: Int, y: Int)(
      implicit current: Array[Array[State]]
  ): State = {
    val occupiedSeats = {
      var count = 0

      for {
        dx <- delta
        dy <- delta
        if !(dx == 0 && dy == 0)
        if isValidIndex(x + dx, y + dy)
        if findFirstSeatInDirection(x + dx, y + dy, dx, dy).contains(Occupied)
      } count += 1

      count
    }

    current(x)(y) match {
      case Occupied if occupiedSeats >= 5 => Vacant
      case Occupied                       => Occupied
      case Vacant if occupiedSeats == 0   => Occupied
      case Vacant                         => Vacant
      case Floor                          => Floor
    }
  }

  @tailrec
  def loop(current: Array[Array[State]])(
      implicit nextStateFunction: (Int, Int, Array[Array[State]]) => State
  ): Int = {

    val next = Array.ofDim[State](current.length, current(0).length)

    val updatedCount = {
      var u = 0
      for {
        i <- current.indices
        j <- current(i).indices
      } {
        next(i)(j) = nextStateFunction(i, j, current)
        if (next(i)(j) != current(i)(j)) u += 1
      }
      u
    }

    if (updatedCount == 0) {
      var u = 0
      for {
        i <- current.indices
        j <- current(i).indices
        if current(i)(j) == Occupied
      } u += 1
      u
    } else loop(next)
  }

  def main(args: Array[String]): Unit = {
    val input =
      """LLLLLLL.LLLLLLLLLLLL.LL.L.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
                  |LLLLLLL.LLLLLLLLLLLL.LLL..LLLLLLLLLLLLLLLLLLLLLLL..LLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL
                  |LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLL
                  |LLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLL.LLLL
                  |LLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLL.LLLLLL.L.LLLLLLLL.L.LLLLLL.LLLLLL.LL.LLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
                  |.L.....L...L.....LL..L...LLL.L.LL..LLL..LL.LLL...LLLL..L......L..........L...L..LL..LLL.L...L
                  |LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLL.LLLLLL.LLLLL.L.LLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LL.LLLLLLLLLLLLL
                  |LLLL.LL.LLLLLL.LLLL.LLLLL.LLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LL.LLLLL.LLLLLLLLL.LLLL.
                  |LLLLLLL.LLLLLL.LLLLL..LLL.LLLLLL.L.LLLLLLLLL.LLLL..LLLLLLL.L.L.LLLLL.LLLLLLLLLL.LLLLLLLLLLLLL
                  |LLL.LLL.LLLLLLLLLL.L.LLLL..LLLLLLLLLL.LL.LL.LLLLLL.LLLLL.LLLLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL.L.LLLLL.LLLLLLL.LL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLL.
                  |LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLL
                  |L...L..LLL.....L.LL..L.LLLL....LLLL.....L.L.LLL..L.L...LL.LL....L..LLLL..L..L.LL...L.L.....L.
                  |LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLL.L.LLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLL.LLLLLLL.LLLLLLLLLL.LLLLL
                  |LLLLLLL.LLLLLLLLLLLL.LLLL.LLLL.LLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.L.LL.LLLLL.LLLLLLLLLLLLL
                  |LLLLLLL..LLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL
                  |.........LL..LL..LL..LL.....L..L..LL.............L...L....LLLL...LL...LLL..L...LLL.....L....L
                  |LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLL..LLLLLL.L.LLL
                  |LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL
                  |LLLLLLL.LL.LLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLL.LLL.LLLLLLLLLL
                  |LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLL.LL.LL..LLLLLLLLLLLL
                  |LLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLLLLLL.L.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL
                  |...L.L..L.L.L....L....L.LLL.L.L.L..L..L...L....L......L.......L..L.L.L..L..LL...L....L....LL.
                  |LLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.L.LL.LL.LL.LLLLLLLLLLLLL
                  |LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLL.L.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL..LLL..LLLL.LLLL.LL.LLLLL
                  |LLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLL.LLLLL.L.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL
                  |LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLL..LLLL
                  |LLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LL.LLLLLLLLLL.LLLLLLLLL.LLLLLLLLLL.LLLLLLL.LLLLL
                  |..LLLLL...LL...L..L.....L.LL.L....L.L..LL.L......L.L.L..L...L.L..L..L..L.LL..L.L.L..L.....L..
                  |LLLLLLL.LLLLLL.LLL.L.LLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLL.LLL.LL.LLLLL.LLLL.LLLLLLLL.LLLLLL.LL.LLLL..LLLLLL.LLLLLLLLLL.LLLL..LLLL.LLLLLLL.LLLLL
                  |LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLL.LLLLLLL.LLLLL
                  |.LLLLLL.LLLLLL.LLLLL.LLLLLLLLL.LLL.LLLLLLL.L.LLLLLLLLLLLLLLLL.LLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
                  |LLLLLLL.LLL.LL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLL.LL.LL.LLLLLLLLLLLLLL.LL.LLLL.LL.LLLL.
                  |LLLLLLL.LLLLLL.LLLLLLLLLL.L..LLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLL
                  |LL......LLL...L.LL...LL..LLLL.L..L.........L..L.....L..L......L..L...LLL.....LL.......LL.....
                  |LLLLLLL.LLLLLL.LLLLL.L.LL.LLLLLLLL.LLLL.LLLLL.L.LLLLLLLLLL.LLLLLLLLL.LLLLLLLLL..LLLLLLLLLLLLL
                  |LLLLLLL.L.LLLL.LLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LL.LL.LLLLLLLLLLLLL
                  |LLLLLLL.LLLLLL.LLLLLLLLLL..LLLLLLLLLLLLLLLLLLLLLLL.L.LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL
                  |LLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.L.LLLLLLLLLLL
                  |LLLLLLL.LLLLLL.LLLLL.LLLL.LLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL
                  |LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLL.LLLLLLLLLLLLLLLLLLLL
                  |LLLLLLL.LLLLLL.LLL.L.LLLL.LLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
                  |L....L.L..L.L....L..L.L...L.....L..LL...L.L.L.L.LL..L....LL..LL....L.L.......LLL..LLL...LL..L
                  |LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLL..LLLLLLLLL.LLLL..LLLL.LLLLLLLLLLLLL
                  |LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLL
                  |LLLLLLL.LLLLL..LLLLL.LLLLLLLLLLLLL.LLLLLL.LL..LLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLLLL.LLL
                  |L.L.LLLLLLLLLL.LLLLL.LLLL.LLLLL.LL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLL.LLL.LLLL.LLLLLLLLLLLLLLLLLLL
                  |LLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLL
                  |LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLL.L.LLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLL.
                  |LL.L.LLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLL
                  |...L..L..L..L....LLL..L.LL....L........L.......L.L.LL.L........L...LLLL....LL.......L.LL..L.L
                  |LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL
                  |LLLLLLL.LLLLLL.LL.LL.LLLLLLLLLL.LL.LLLLLLLLL.LLLLL.LLL..LLLLLLLLLLLL.LLLL..LL.L.LLLLLLL.LLLLL
                  |LLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLL.L.LL.LL.LLLLLLL.LLLLLLLLLL.LLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLLLLLL.LL.L.LLL.LLLL.LLLLLLL..LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLL.LLLL..LLLLLLLLLLLL.LLLLL
                  |LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLL.L.LLLLL
                  |LLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLL.L.LLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
                  |LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL.L.LLL.LLLLLLL..LLLL
                  |LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.L.LL.L.LLL.LLLLLLL.LLLL.
                  |....LLLL.L..L....LL..L.....LL..........LL.....L.L..LL.LL......L.L.L.L....L.LL.L.L..L......L..
                  |LLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLL
                  |LLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLLLL.LLL.LLLLL.LLLL..LLLLLLLLLLLLLLLLL.LLLL.LLLLL..LLLLLL.LLLLL
                  |LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLL.LL.LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LL.LL
                  |LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLL.LLLLLLLLLL.
                  |LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL..LLLLLL.LLLLLLL.LL.LLLLLLLLLLLLLLLLLLLLLLLLL.L.LLLLL..LLLL
                  |LLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LL.LLLLLLL.LLLLLLL.L.LLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLL.LLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLLLL.LLLLLLL.LLLLL
                  |LLLLLL..LLLLLLLLLL.L.LLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLL.LLLLLLL.L.LLL
                  |LLLLLLLLLL.LLLLLLLLL.LL.L.LLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLL.LLL.LLLLL
                  |L.L...L....LL.L....LL..L...L...LL.L.............LLLLL..LL........LLL.L....L.L...L.L...LLL.LL.
                  |LLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLL.LLLLLL.LLLLL
                  |LL.LLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLL.L.LLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLL
                  |LLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
                  |LLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLL.LLLLLLLLLLL.LLL
                  |.LLL..LL.L.LL.L...L.L..L...L....L.L................L....L.......L..L.....L...LLL....L....LL..
                  |LLLLL.LLLLLLLL.LLLLL.L.LL.LLLLLLLL.LLLLLLLLL..LLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
                  |LLLLLLL.LLLLLL.LLLLLLLLLL.LL.L.LLL.LLLLLL.LL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLL.LLLLLLLLL.LLLLL
                  |LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLLLL.L.LLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL
                  |LLLLLL..LLLLLLLLLLLLLLLLL.LLLLLLLL.L.LLL.LLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLL
                  |LLLLLLL.LLLL.L.LLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLL.LLLLLLLL
                  |LLLLLLL.LLLLLLLLLLLL..LLL.LLLLLLLL..LLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLL""".stripMargin

    val initial: Array[Array[State]] = input
      .split("\n")
      .map(_.split("").map {
        case "L" => Vacant: State
        case "#" => Occupied
        case "." => Floor
      })

    println(loop(initial)(computeNextState_part_01(_, _)(_)))
    println(loop(initial)(computeNextState_part_02(_, _)(_)))
  }
}
