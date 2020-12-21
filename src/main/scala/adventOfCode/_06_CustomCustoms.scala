package adventOfCode

/**
 * Each group's answers are separated by a blank line, and within each group, each person's answers are on a single line.
 * Each person's answer gives the questions to which he answered yes.
 */
object _06_CustomCustoms {

  def everyoneAnsweredYes(group: Seq[String]): Int = {
    val ans =
      group.foldLeft(Set.empty[Char])((acc, member) => acc union member.toSet).size
    ans
  }

  def anyoneAnsweredYes(group: Seq[String]): Int = {
    val ans =
      group.foldLeft(('a' to 'z').toSet)((acc, member) => acc intersect member.toSet).size
    ans
  }

  def main(args: Array[String]): Unit = {
    val input = """abc
                  |
                  |a
                  |b
                  |c
                  |
                  |ab
                  |ac
                  |
                  |a
                  |a
                  |a
                  |a
                  |
                  |b""".stripMargin

    val groups = input
      .split("\\n[\\n]+")
      .map(_.split("\\n"))

    val first = groups.map(everyoneAnsweredYes(_)).sum
    val second = groups.map(anyoneAnsweredYes(_)).sum

    println(first, second)
  }
}
