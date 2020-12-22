package adventOfCode

import scala.annotation.tailrec

/**
 * Fix one instruction for program to end, jmp -> nop | nop -> jmp
 *
 * Sample Instructions:
 *
 * nop +0
 * acc +1
 * jmp +4
 * acc +3
 * jmp -3
 * acc -99
 * acc +1
 * jmp -4
 * acc +6
 */
object _08_HandheldHalting {
  val pattern = "(acc|jmp|nop) ([+-])(\\d+)".r

  // part 01: execute to end or stop when repeating

  def executeOrStop(instructions: Seq[String]): Int = {
    val visited = new Array[Boolean](instructions.length)

    @tailrec
    def loop(i: Int = 0, acc: Int = 0): Int = {
      if (visited(i) | i >= instructions.length) acc
      else {
        visited(i) = true

        instructions(i) match {
          case pattern("acc", "+", v) => loop(i + 1, acc + v.toInt)
          case pattern("acc", "-", v) => loop(i + 1, acc - v.toInt)
          case pattern("jmp", "+", v) => loop(i + v.toInt, acc)
          case pattern("jmp", "-", v) => loop(i - v.toInt, acc)
          case pattern("nop", _, _)   => loop(i + 1, acc)
        }
      }
    }

    loop()
  }

  // part 02: change one instruction

  def breakRepetition(instructions: Array[String]): Option[Int] = {

    val visited = new Array[Boolean](instructions.length)

    def backtrack(i: Int = 0, acc: Int = 0): Option[Int] = {
      if (i >= instructions.length) Some(acc)
      else if (visited(i)) None
      else {
        visited(i) = true

        val first = instructions(i) match {
          case pattern("acc", "+", v) => backtrack(i + 1, acc + v.toInt)
          case pattern("acc", "-", v) => backtrack(i + 1, acc - v.toInt)
          case pattern("jmp", "+", v) => backtrack(i + v.toInt, acc)
          case pattern("jmp", "-", v) => backtrack(i - v.toInt, acc)
          case pattern("nop", _, _)   => backtrack(i + 1, acc)
        }

        val result = first.orElse {
          instructions(i) match {
            case pattern("acc", _, _) => first
            case pattern("jmp", _, _) => backtrack(i + 1, acc)
            case pattern("nop", "+", v) => backtrack(i + v.toInt, acc)
            case pattern("nop", "-", v) => backtrack(i - v.toInt, acc)

          }
        }

        visited(i) = false

        result
      }
    }

    backtrack()
  }

  def main(args: Array[String]): Unit = {
    val inputs = """nop +0
                   |acc +1
                   |jmp +4
                   |acc +3
                   |jmp -3
                   |acc -99
                   |acc +1
                   |jmp -4
                   |acc +6""".stripMargin.split("\n")

    println(executeOrStop(inputs))
    println(breakRepetition(inputs))
  }
}
