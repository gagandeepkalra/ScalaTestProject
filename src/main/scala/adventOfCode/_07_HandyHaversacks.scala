package adventOfCode

/**
  * example Bag rules:
  *
  * light red bags contain 1 bright white bag, 2 muted yellow bags.
  * dark orange bags contain 3 bright white bags, 4 muted yellow bags.
  * bright white bags contain 1 shiny gold bag.
  * muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
  * shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
  * dark olive bags contain 3 faded blue bags, 4 dotted black bags.
  * vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
  * faded blue bags contain no other bags.
  * dotted black bags contain no other bags.
  */
object _07_HandyHaversacks {

  // part 01: find all bags that may contain shiny gold bag

  def reverseTranslate(inputs: Seq[String]): Map[String, List[String]] = {

    val graph = collection.mutable.Map.empty[String, List[String]]

    val pattern =
      "^(\\w* ?\\w*) bags contain (no other bags|(?:\\d+ \\w* ?\\w* bags?(?:, )?)+)\\.$".r

    val subPattern = "(?:\\d+ (\\w* ?\\w*) bags?(?:, )?)".r

    inputs.foreach {
      case pattern(_, "no other bags") =>
      case pattern(key, sub) =>
        val values = subPattern
          .findAllIn(sub)
          .flatMap(text => subPattern.unapplySeq(text))
          .flatten

        values.foreach { value =>
          graph.update(value, key :: graph.getOrElseUpdate(value, Nil))
        }
    }

    graph.toMap
  }

  def findAllBagsThatMayContain(root: String, graph: Map[String, List[String]]): Set[String] =
    graph.getOrElse(root, Nil).foldLeft(Set.empty[String]) {
      case (set, value) =>
        set ++ findAllBagsThatMayContain(value, graph) + value
    }

  // part 02: count all possible bags under shiny gold bag

  def translate(inputs: Seq[String]): Map[String, List[(Int, String)]] = {

    val graph = collection.mutable.Map.empty[String, List[(Int, String)]]

    val pattern =
      "^(\\w* ?\\w*) bags contain (no other bags|(?:\\d+ \\w* ?\\w* bags?(?:, )?)+)\\.$".r

    val subPattern = "(?:(\\d+) (\\w* ?\\w*) bags?(?:, )?)".r

    inputs.foreach {
      case pattern(_, "no other bags") =>
      case pattern(key, sub) =>
        subPattern
          .findAllIn(sub)
          .foreach {
            case subPattern(count, value) =>
              graph.update(
                key,
                (count.toInt, value) :: graph.getOrElseUpdate(key, Nil)
              )
          }
    }

    graph.toMap
  }

  def countTotalBagsInside(root: String, graph: Map[String, List[(Int, String)]]): Int = {
    graph.getOrElse(root, Nil).foldLeft(0) {
      case (acc, (count, value)) =>
        acc + count + count * countTotalBagsInside(value, graph)
    }
  }

  def main(args: Array[String]): Unit = {
    val inputs =
      """light red bags contain 1 bright white bag, 2 muted yellow bags.
        |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
        |bright white bags contain 1 shiny gold bag.
        |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
        |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
        |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
        |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
        |faded blue bags contain no other bags.
        |dotted black bags contain no other bags.""".stripMargin
        .split("\n")

    println {
      findAllBagsThatMayContain("shiny gold", reverseTranslate(inputs)).size
    }

    println {
      countTotalBagsInside("shiny gold", translate(inputs))
    }
  }
}
