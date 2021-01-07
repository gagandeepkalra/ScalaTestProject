package adventOfCode

import scala.annotation.tailrec
import scala.util.matching.Regex

/**
 * mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
 * mem[8] = 11
 * mem[7] = 101
 * mem[8] = 0
 *
 * The bitmask is always given as a string of 36 bits, written with the most significant bit (representing 2^35) on the
 * left and the least significant bit (2^0, that is, the 1s bit) on the right. The current bitmask is applied to values
 * immediately before they are written to memory: a 0 or 1 overwrites the corresponding bit in the value, while an X leaves
 * the bit in the value unchanged.
 *
 * part 01: Sum all the values in memory afterwards.
 *
 * part 02:
 * A version 2 decoder chip doesn't modify the values being written at all. Instead, it acts as a memory address decoder.
 * Immediately before a value is written to memory, each bit in the bitmask modifies the corresponding bit of the destination
 * memory address in the following way:
 *
 * If the bitmask bit is 0, the corresponding memory address bit is unchanged.
 * If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
 * If the bitmask bit is X, the corresponding memory address bit is floating (0|1).
 *
 * Sum all the values in memory afterwards.
 */
object _14_DockingData {

  val updateMask: Regex = "^mask = (.*)$".r
  val updateMemory: Regex = "^mem\\[(\\d+)] = (\\d+)$".r

  @tailrec
  def parseRecurVersion01(
      input: List[String],
      mask: Map[Int, Char] = Map.empty,
      memory: Map[Int, Long] = Map.empty
  ): Long = {
    input match {
      case Nil =>
        memory.values.sum
      case updateMask(newMask) :: tail =>
        parseRecurVersion01(
          tail,
          newMask.reverse.zipWithIndex.collect {
            case (c, i) if c != 'X' => i -> c
          }.toMap,
          memory
        )
      case updateMemory(address, value) :: tail =>
        val updatedBinary = mask
          .foldLeft {
            val binary = value.toLong.toBinaryString
            ("0" * (63 - binary.length) + binary).reverse
          } {
            case (acc, (i, c)) => acc.updated(i, c)
          }
        val updatedValue = java.lang.Long.parseLong(updatedBinary.reverse, 2)
        parseRecurVersion01(tail,
                            mask,
                            memory + (address.toInt -> updatedValue))
    }
  }

  private def generateAllAddresses(
      mask: String,
      input: Array[Char],
      i: Int = 0
  ): Set[Long] = {
    if (i >= input.length)
      Set(java.lang.Long.parseLong(input.mkString(""), 2))
    else {
      mask(i) match {
        case '0' => generateAllAddresses(mask, input, i + 1)
        case '1' =>
          input(i) = '1'
          generateAllAddresses(mask, input, i + 1)
        case 'X' =>
          input(i) = '0'
          val result0 = generateAllAddresses(mask, input, i + 1)
          input(i) = '1'
          result0 union generateAllAddresses(mask, input, i + 1)
      }
    }
  }

  @tailrec
  def parseRecurVersion02(
      input: List[String],
      mask: String,
      memory: Map[Long, Long] = Map.empty
  ): Long = {
    input match {
      case Nil =>
        memory.values.sum
      case updateMask(newMask) :: tail =>
        parseRecurVersion02(tail, newMask, memory)
      case updateMemory(address, value) :: tail =>
        val addressInput = {
          val binary = address.toLong.toBinaryString
          ("0" * (36 - binary.length) + binary).toCharArray
        }

        val valueLong = value.toLong

        val allAddresses = generateAllAddresses(mask, addressInput)

        val updatedMemory = allAddresses.foldLeft(memory)(
          (acc, address) => acc + (address -> valueLong)
        )

        parseRecurVersion02(tail, mask, updatedMemory)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = """mask = 000000000000000000000000000000X1001X
                  |mem[42] = 100
                  |mask = 00000000000000000000000000000000X0XX
                  |mem[26] = 1""".stripMargin.split("\n")

    println(parseRecurVersion02(input.toList, ""))
  }

}
