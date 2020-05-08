package google.kickstart._2019.F

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050edc/00000000001864bc

[Set Theory]

we find people that cannot mentor me, total minus that gives us people that can mentor me, we sum afterwards
someone cannot mentor me if and only if there skills are an exact subset of mine

then for each subset of my skills we count people with that same subset of skills
 */
object TeachMe {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, _) = io.StdIn.readLine.split(" ").map(_.toInt)

      val peopleWithSkill: collection.mutable.Map[Set[Int], Int] = {
        val m = collection.mutable.Map.empty[Set[Int], Int].withDefaultValue(0)
        (1 to n).foreach { _ =>
          val s = io.StdIn.readLine.split(" ").map(_.toInt).tail.toSet
          m.update(s, m(s) + 1)
        }
        m
      }

      val result = peopleWithSkill.foldLeft(0L) {
        case (acc, (s, v)) => acc + v * (n - 1 - (s.subsets.map(peopleWithSkill).sum - 1))
      }

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }

}
