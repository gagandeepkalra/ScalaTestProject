package adventOfCode
import scala.annotation.tailrec
import scala.collection.immutable

/**
 * Game of Life
 *
 * During a cycle, all cubes simultaneously change their state according to the following rules:
 *
 * If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
 * If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
 */
object _17_Conway_Cubes {

  def threeDimensionalConway(input: Array[Array[Array[Boolean]]]): Int = {
    val (xMax, yMax, zMax) = (input.length, input(0).length, input(0)(0).length)

    val deltas: immutable.Seq[(Int, Int, Int)] = {
      for {
        dx <- -1 to 1
        dy <- -1 to 1
        dz <- -1 to 1
        if !(dx == 0 && dy == 0 && dz == 0)
      } yield (dx, dy, dz)
    }

    @tailrec
    def loop(
      finalTurn: Int,
      current: Array[Array[Array[Boolean]]],
      turn: Int = 1
    ): Array[Array[Array[Boolean]]] =
      if (turn > finalTurn) current
      else {

        val X = xMax + 2 * turn
        val Y = yMax + 2 * turn
        val Z = zMax + 2 * turn

        val next = Array.ofDim[Boolean](X, Y, Z)

        val prevX = X - 2
        val prevY = Y - 2
        val prevZ = Z - 2

        def isValid(i: Int, j: Int, k: Int): Boolean =
          0 <= i && i < prevX && 0 <= j && j < prevY && 0 <= k && k < prevZ

        for {
          i <- 0 until X
          j <- 0 until Y
          k <- 0 until Z
        } {
          val active = {
            deltas.foldLeft(0) {
              case (active, (dx, dy, dz)) if isValid(i - 1 + dx, j - 1 + dy, k - 1 + dz) =>
                if (current(i - 1 + dx)(j - 1 + dy)(k - 1 + dz)) active + 1 else active
              case (active, _) => active
            }
          }

          val state = isValid(i - 1, j - 1, k - 1) && current(i - 1)(j - 1)(k - 1)

          next(i)(j)(k) = (state, active) match {
            case (true, 2)  => true
            case (true, 3)  => true
            case (false, 3) => true
            case _          => false
          }
        }

        loop(finalTurn, next, turn + 1)
      }

    val state  = loop(6, input)
    var result = 0
    for {
      i <- state.indices
      j <- state(i).indices
      k <- state(i)(j).indices
    } result += (if (state(i)(j)(k)) 1 else 0)

    result
  }

  def fourDimensionalConway(input: Array[Array[Array[Array[Boolean]]]]): Int = {
    val (xMax, yMax, zMax, wMax) = (input.length, input(0).length, input(0)(0).length, input(0)(0)(0).length)

    val deltas: immutable.Seq[(Int, Int, Int, Int)] = {
      for {
        dx <- -1 to 1
        dy <- -1 to 1
        dz <- -1 to 1
        dw <- -1 to 1
        if !(dx == 0 && dy == 0 && dz == 0 && dw == 0)
      } yield (dx, dy, dz, dw)
    }

    @tailrec
    def loop(
      finalTurn: Int,
      current: Array[Array[Array[Array[Boolean]]]],
      turn: Int = 1
    ): Array[Array[Array[Array[Boolean]]]] =
      if (turn > finalTurn) current
      else {

        val X = xMax + 2 * turn
        val Y = yMax + 2 * turn
        val Z = zMax + 2 * turn
        val W = wMax + 2 * turn

        val next = Array.ofDim[Boolean](X, Y, Z, W)

        val prevX = X - 2
        val prevY = Y - 2
        val prevZ = Z - 2
        val prevW = W - 2

        def isValid(i: Int, j: Int, k: Int, w: Int): Boolean =
          0 <= i && i < prevX && 0 <= j && j < prevY && 0 <= k && k < prevZ && 0 <= w && w < prevW

        for {
          i <- 0 until X
          j <- 0 until Y
          k <- 0 until Z
          w <- 0 until W
        } {
          val active = {
            deltas.foldLeft(0) {
              case (active, (dx, dy, dz, dw)) if isValid(i - 1 + dx, j - 1 + dy, k - 1 + dz, w - 1 + dw) =>
                if (current(i - 1 + dx)(j - 1 + dy)(k - 1 + dz)(w - 1 + dw)) active + 1 else active
              case (active, _) => active
            }
          }

          val state = isValid(i - 1, j - 1, k - 1, w - 1) && current(i - 1)(j - 1)(k - 1)(w - 1)

          next(i)(j)(k)(w) = (state, active) match {
            case (true, 2)  => true
            case (true, 3)  => true
            case (false, 3) => true
            case _          => false
          }
        }

        loop(finalTurn, next, turn + 1)
      }

    val state  = loop(6, input)
    var result = 0
    for {
      i <- state.indices
      j <- state(i).indices
      k <- state(i)(j).indices
      w <- state(i)(j)(k).indices
    } result += (if (state(i)(j)(k)(w)) 1 else 0)

    result
  }

  def main(args: Array[String]): Unit = {
    val input3 =
      """.##...#.
        |.#.###..
        |..##.#.#
        |##...#.#
        |#..#...#
        |#..###..
        |.##.####
        |..#####.""".stripMargin
        .split("\n")
        .map(_.toArray.map {
          case '#' => Array(true)
          case '.' => Array(false)
        })

    println(threeDimensionalConway(input3))

    val input4 =
      """.##...#.
        |.#.###..
        |..##.#.#
        |##...#.#
        |#..#...#
        |#..###..
        |.##.####
        |..#####.""".stripMargin
        .split("\n")
        .map(_.toArray.map {
          case '#' => Array(Array(true))
          case '.' => Array(Array(false))
        })
    println(fourDimensionalConway(input4))

  }
}
