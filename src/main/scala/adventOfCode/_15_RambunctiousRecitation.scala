package adventOfCode

import scala.annotation.tailrec

/**
 * In this game, the players take turns saying numbers. They begin by taking turns reading from a list of starting numbers
 * (your puzzle input). Then, each turn consists of considering the most recently spoken number:
 *
 * If that was the first time the number has been spoken, the current player says 0.
 * Otherwise, the number had been spoken before; the current player announces how many turns apart the number is from when
 * it was previously spoken.
 */
object _15_RambunctiousRecitation {

  @tailrec
  def loop(
      finalTurn: Int,
      turn: Int,
      prev: Int,
      memory: Map[Int, List[Int]]
  ): Int = {

    val current: Int = {
      val occurrences = memory.getOrElse(prev, Nil)

      (for {
        last <- occurrences.headOption
        secondLast <- occurrences.tail.headOption
      } yield last - secondLast).getOrElse(0)
    }

    val updatedMemory: Map[Int, List[Int]] = {
      val update = turn :: memory.getOrElse(current, Nil)
      memory + (current -> update)
    }

    if (finalTurn == turn) current
    else loop(finalTurn, turn + 1, current, updatedMemory)
  }

  def main(args: Array[String]): Unit = {
    val input = "10,16,6,0,1,17".split(",").map(_.toInt)

    val memory = input.zip(Stream.from(1)).groupBy(_._1).map {
      case (k, values) => k -> values.map(_._2).sorted.reverse.toList
    }

    println(loop(2020, input.length + 1, input.last, memory))
    println(loop(30000000, input.length + 1, input.last, memory))

  }
}
