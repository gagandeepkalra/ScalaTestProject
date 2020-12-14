package adventOfCode

import scala.util.matching.Regex

/**
 * 01:
 *
 * 1-3 a: abcde
 * 1-3 b: cdefg
 * 2-9 c: ccccccccc
 * Each line gives the password policy and then the password. The password policy indicates the lowest and highest number
 * of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain \
 * a at least 1 time and at most 3 times.
 *
 * 02:
 *
 * Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character,
 * and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain
 * the given letter. Other occurrences of the letter are irrelevant for the purposes of policy enforcement.
 */
object _02_PasswordPhilosophy {

  val pattern: Regex = "^(\\d+)-(\\d+) (\\w): (\\w+)$".r

  def part_01(input: List[String]): Int = {
    input.count {
      case pattern(lower, upper, char, password) =>
        val l = lower.toInt
        val u = upper.toInt
        val c = char(0)
        val actual: Int = password.count(_ == c)

        l <= actual && actual <= u
    }
  }

  def part_02(input: List[String]): Int = {
    input.count {
      case pattern(lower, upper, char, password) =>
        val l = lower.toInt -1
        val u = upper.toInt -1
        val c = char(0)

        (password(l) == c || password(u) == c) && !(password(l) == c && password(u) == c)
    }
  }
}
