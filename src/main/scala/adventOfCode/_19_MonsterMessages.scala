package adventOfCode

import scala.annotation.tailrec
import scala.util.matching.Regex

/**
  * The rules for valid messages (the top part of your puzzle input) are numbered and build upon each other. For example:
  *
  * 0: 1 2
  * 1: "a"
  * 2: 1 3 | 3 1
  * 3: "b"
  * Some rules, like 3: "b", simply match a single character (in this case, b).
  *
  * The remaining rules list the sub-rules that must be followed; for example, the rule 0: 1 2 means that to match rule 0,
  * the text being checked must match rule 1, and the text after the part that matched rule 1 must then match rule 2.
  *
  * Some of the rules have multiple lists of sub-rules separated by a pipe (|). This means that at least one list of sub-rules
  * must match. (The ones that match might be different each time the rule is encountered.) For example, the rule 2: 1 3 | 3 1 means
  * that to match rule 2, the text being checked must match rule 1 followed by rule 3 or it must match rule 3 followed by rule 1.
  *
  * part 02:
  *
  * Some recursive rules are added, find number of inputs satisfying this updated grammar
  * 8: 42 | 42 8
  * 11: 42 31 | 42 11 31
  *
  */
object _19_MonsterMessages {
  val withCharValue: Regex            = "^(\\d+): \"([a-z])\"$".r
  val withSingleIntValue: Regex       = "^(\\d+): (\\d+)$".r
  val withDoubleIntValue: Regex       = "^(\\d+): (\\d+) (\\d+)$".r
  val withTripleIntValue: Regex       = "^(\\d+): (\\d+) (\\d+) (\\d+)$".r
  val withSingleIntDoubleValue: Regex = "^(\\d+): (\\d+) \\| (\\d+)$".r
  val withDoubleIntDoubleValue: Regex = "^(\\d+): (\\d+) (\\d+) \\| (\\d+) (\\d+)$".r

  sealed trait Value
  case class Character(c: Char)                            extends Value
  case class SingleInteger(i: Int)                         extends Value
  case class DoubleInteger(i: Int, j: Int)                 extends Value
  case class TripleInteger(i: Int, j: Int, k: Int)         extends Value
  case class SingleIntegerOr(i: Int, j: Int)               extends Value
  case class DoubleIntegerOr(i: (Int, Int), j: (Int, Int)) extends Value

  @tailrec
  def parseRules(input: List[String], acc: Map[Int, Value] = Map.empty): Map[Int, Value] = {
    input match {
      case Nil                                 => acc
      case withCharValue(n, c) :: tail         => parseRules(tail, acc + (n.toInt -> Character(c.head)))
      case withSingleIntValue(n, i) :: tail    => parseRules(tail, acc + (n.toInt -> SingleInteger(i.toInt)))
      case withDoubleIntValue(n, i, j) :: tail => parseRules(tail, acc + (n.toInt -> DoubleInteger(i.toInt, j.toInt)))
      case withTripleIntValue(n, i, j, k) :: tail =>
        parseRules(tail, acc + (n.toInt -> TripleInteger(i.toInt, j.toInt, k.toInt)))
      case withSingleIntDoubleValue(n, i, j) :: tail =>
        parseRules(tail, acc + (n.toInt -> SingleIntegerOr(i.toInt, j.toInt)))
      case withDoubleIntDoubleValue(n, i, j, k, l) :: tail =>
        parseRules(tail, acc + (n.toInt -> DoubleIntegerOr((i.toInt, j.toInt), (k.toInt, l.toInt))))
    }
  }

  /**
    * First approach:
    * Generate all possible strings satisfying this grammar.
    */
  def generateAllPossibilities(v: Value)(implicit store: Map[Int, Value]): Set[String] = {
    v match {
      case Character(c)     => Set(c.toString)
      case SingleInteger(i) => generateAllPossibilities(store(i))
      case DoubleInteger(i, j) =>
        val first  = generateAllPossibilities(store(i))
        val second = generateAllPossibilities(store(j))
        for {
          f <- first
          s <- second
        } yield f + s
      case TripleInteger(i, j, k) =>
        val first  = generateAllPossibilities(store(i))
        val second = generateAllPossibilities(store(j))
        val third  = generateAllPossibilities(store(k))
        for {
          f <- first
          s <- second
          t <- third
        } yield f + s + t
      case SingleIntegerOr(i, j) =>
        val A = generateAllPossibilities(store(i))
        val B = generateAllPossibilities(store(j))
        A ++ B
      case DoubleIntegerOr((i, j), (k, l)) =>
        val p1F = generateAllPossibilities(store(i))
        val p1S = generateAllPossibilities(store(j))
        val A = for {
          f <- p1F
          s <- p1S
        } yield f + s

        val p2F = generateAllPossibilities(store(k))
        val p2S = generateAllPossibilities(store(l))
        val B = for {
          f <- p2F
          s <- p2S
        } yield f + s

        A ++ B
    }
  }

  /**
    * Second approach:
    * Generate Regex satisfying this grammar.
    */
  def generateRegex(key: Int)(implicit store: Map[Int, Value]): Regex = {
    def generate(k: Int): String = {
      store(k) match {
        case Character(c)           => c.toString
        case SingleInteger(i)       => generate(i)
        case DoubleInteger(i, j)    => generate(i) + generate(j)
        case TripleInteger(i, j, k) => generate(i) + generate(j) + generate(k)
        case SingleIntegerOr(i, j)  => s"(?:${generate(i)}|${generate(j)})"
        case DoubleIntegerOr((i, j), (k, l)) =>
          s"(?:${generate(i)}${generate(j)}|${generate(k)}${generate(l)})"
      }
    }

    s"^${generate(key)}$$".r
  }

  /**
    * 8: 42 | 42 8
    * 11: 42 31 | 42 11 31
    */
  def generateRegexWithRulesOverride(key: Int)(implicit store: Map[Int, Value]): Regex = {

    def generate(k: Int): String = {
      (k, store(k)) match {
        case (_, Character(c))      => c.toString
        case (8, SingleInteger(42)) => s"(?:${generate(42)})+"
        case (_, SingleInteger(i))  => generate(i)
        case (11, DoubleInteger(42, 31)) =>
          val f = generate(42)
          val s = generate(31)
          s"(?:(?:$f$s)|(?:$f$f$s$s)|(?:$f$f$f$s$s$s)|(?:$f$f$f$f$s$s$s$s))"
        case (_, DoubleInteger(i, j))    => generate(i) + generate(j)
        case (_, TripleInteger(i, j, k)) => generate(i) + generate(j) + generate(k)
        case (_, SingleIntegerOr(i, j))  => s"(?:${generate(i)}|${generate(j)})"
        case (_, DoubleIntegerOr((i, j), (k, l))) =>
          s"(?:${generate(i)}${generate(j)}|${generate(k)}${generate(l)})"
      }
    }

    s"^${generate(key)}$$".r
  }

  def solveRegex(input: List[String])(implicit regex: (Int, Map[Int, Value]) => Regex): Int = {
    val (rulesInput, rest) = input.span(_.nonEmpty)
    val rules = {
      parseRules(rulesInput)
    }
    val tests = rest.tail

    val validStrings = regex(0, rules)

    tests.count(_.matches(validStrings.toString))
  }

  /**
    * Third approach:
    * Convert the given grammar to Chomsky Normal Form
    *
    * A context free grammar (CFG) is in Chomsky Normal Form (CNF) if all production rules satisfy one of the following conditions:
    *
    * A non-terminal generating a terminal (e.g.; X->x)
    * A non-terminal generating two non-terminals (e.g.; X->YZ)
    * Start symbol generating ε. (e.g.; S-> ε)
    */
  sealed trait CNFValue
  case class Terminal(c: Char)    extends CNFValue
  case class Pair(i: Int, j: Int) extends CNFValue

  def CNFGrammar(rules: Map[Int, List[Value]]): Map[Int, List[CNFValue]] = {
    val singlesFlat: Map[Int, List[Int]] = rules.toList
      .flatMap {
        case (k, ls) =>
          ls.collect {
            case SingleInteger(i)      => List(k -> i)
            case SingleIntegerOr(i, j) => List(k -> i, k -> j)
          }.flatten
      }
      .groupBy(_._1)
      .map { case (k, vs) => k -> vs.map(_._2) }

    val rest: Map[Int, List[Value]] = rules
      .map {
        case (k, ls) =>
          k -> ls.collect {
            case SingleInteger(i)      => None
            case SingleIntegerOr(i, j) => None
            case default               => Some(default)
          }.flatten
      }

    val order: List[Int] = {
      val visited = collection.mutable.Set.empty[Int]

      var stack = List.empty[Int]

      // we use topological sort to find order required for unit rules removal
      def topologicalSort(i: Int): Unit =
        if (!visited(i)) {
          singlesFlat.getOrElse(i, Nil).foreach(topologicalSort)
          visited.add(i)
          stack ::= i
        }

      singlesFlat.keys.foreach(topologicalSort)

      stack.reverse
    }

    val unitRemoval = order.foldLeft(rest) {
      case (acc, i) =>
        val ks = singlesFlat.getOrElse(i, Nil).flatMap(acc.getOrElse(_, Nil)) // i -> j -> k's

        acc + (i -> (ks ::: acc.getOrElse(i, Nil)).distinct)
    }

    @tailrec
    def loop(store: List[(Int, Value)], acc: List[(Int, CNFValue)] = Nil)(extra: Int = -1): Map[Int, List[CNFValue]] =
      store match {
        case Nil =>
          acc.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2).distinct }
        case (key, Character(c)) :: tail =>
          loop(tail, (key -> Terminal(c)) :: acc)(extra)
        case (key, DoubleInteger(i, j)) :: tail =>
          loop(tail, (key -> Pair(i, j)) :: acc)(extra)
        case (key, TripleInteger(i, j, k)) :: tail =>
          loop(tail, (key -> Pair(i, extra)) :: (extra -> Pair(j, k)) :: acc)(extra - 1)
        case (key, DoubleIntegerOr((i, j), (k, l))) :: tail =>
          loop(tail, (key -> Pair(i, j)) :: (key -> Pair(k, l)) :: acc)(extra)
      }

    val tripleRemoval = loop(unitRemoval.toList.flatMap { case (k, vs) => vs.map(k -> _) })()

    tripleRemoval
  }

  /**
    * CYK algorithm is a parsing algorithm for context free grammar.
    * In order to apply CYK algorithm to a grammar, it must be in Chomsky Normal Form. It uses a dynamic programming algorithm to tell whether a string is in the language of a grammar (Recognizer).
    */
  def cyk(input: String)(implicit reverseRules: Map[CNFValue, List[Int]]): List[Int] = {
    val n = input.length

    val grid = Array.fill[List[Int]](n, n)(Nil) // grid(i)(j) -  starting at index i going j steps

    for {
      i <- 0 until n
    } grid(i)(0) = reverseRules(Terminal(input(i)))

    for {
      j <- 1 until n
      i <- 0 until n if i + j < n
    } grid(i)(j) = {
      for {
        k <- 0 until j
        f <- grid(i)(k)
        s <- grid(i + k + 1)(j - k - 1)
        r <- reverseRules(Pair(f, s))
      } yield r
    }.toList

    grid(0)(n - 1)
  }

  def solveCYKPart01(input: List[String]): Int = {
    val (rulesInput, rest) = input.span(_.nonEmpty)
    val rules = {
      parseRules(rulesInput)
    }

    val CNFRules = CNFGrammar(rules.map { case (k, v) => k -> List(v) })
    val reverseCNFRules: Map[CNFValue, List[Int]] =
      CNFRules.toList
        .flatMap { case (k, v) => v.map(_ -> k) }
        .groupBy(_._1)
        .map { case (k, vs) => k -> vs.map(_._2) }
        .withDefaultValue(Nil)

    val tests = rest.tail

    tests.count(cyk(_)(reverseCNFRules).contains(0))
  }

  /**
    * Overrides-
    * 8: 42 | 42 8
    * 11: 42 31 | 42 11 31
    */
  def solveCYKPart02(input: List[String]): Int = {
    val (rulesInput, rest) = input.span(_.nonEmpty)
    val rules = {
      parseRules(rulesInput)
    }

    val updatedRules = rules.map { case (k, v) => k -> List(v) } +
      (8  -> List(SingleInteger(42), DoubleInteger(42, 8))) +
      (11 -> List(DoubleInteger(42, 31), TripleInteger(42, 11, 31)))

    val CNFRules = CNFGrammar(updatedRules)

    val reverseCNFRules: Map[CNFValue, List[Int]] =
      CNFRules.toList
        .flatMap { case (k, v) => v.map(_ -> k) }
        .groupBy(_._1)
        .map { case (k, vs) => k -> vs.map(_._2) }
        .withDefaultValue(Nil)

    val tests = rest.tail

    tests.count(cyk(_)(reverseCNFRules).contains(0))
  }

  def main(args: Array[String]): Unit = {
    val input = """136: 83 66 | 102 116
                  |120: 83 66 | 58 116
                  |26: 56 116 | 105 66
                  |42: 37 66 | 100 116
                  |103: 116 33 | 66 47
                  |35: 116 21 | 66 105
                  |82: 113 116 | 93 66
                  |96: 66 60 | 116 61
                  |25: 116 68 | 66 34
                  |4: 116 66
                  |90: 66 14 | 116 91
                  |63: 119 66 | 27 116
                  |71: 5 116 | 103 66
                  |61: 54 66 | 50 116
                  |2: 119 116 | 4 66
                  |12: 92 116 | 102 66
                  |86: 116 92 | 66 83
                  |100: 66 18 | 116 10
                  |91: 66 46 | 116 45
                  |117: 66 92 | 116 131
                  |22: 66 15 | 116 96
                  |130: 58 116 | 21 66
                  |58: 116 66 | 66 66
                  |97: 116 119 | 66 27
                  |45: 136 66 | 30 116
                  |76: 80 66 | 109 116
                  |10: 66 71 | 116 82
                  |124: 66 116 | 116 66
                  |116: "a"
                  |47: 4 116
                  |110: 116 23 | 66 86
                  |43: 66 17 | 116 21
                  |126: 111 116 | 108 66
                  |134: 66 58 | 116 17
                  |132: 116 105 | 66 119
                  |49: 116 116 | 116 66
                  |107: 131 62
                  |108: 116 41 | 66 41
                  |34: 66 128 | 116 107
                  |53: 116 121 | 66 72
                  |33: 66 41 | 116 102
                  |31: 22 66 | 44 116
                  |9: 130 116 | 19 66
                  |113: 66 73 | 116 51
                  |50: 116 132 | 66 28
                  |122: 105 66 | 119 116
                  |66: "b"
                  |87: 66 3 | 116 2
                  |106: 24 116 | 26 66
                  |65: 66 49 | 116 58
                  |32: 116 125 | 66 16
                  |84: 39 66 | 70 116
                  |3: 41 66 | 56 116
                  |8: 42
                  |62: 116 | 66
                  |55: 4 66 | 83 116
                  |1: 56 66 | 17 116
                  |60: 38 116 | 79 66
                  |133: 4 66 | 4 116
                  |123: 117 116 | 134 66
                  |121: 66 112 | 116 127
                  |38: 66 30 | 116 114
                  |36: 66 131 | 116 21
                  |135: 66 67 | 116 32
                  |70: 98 116 | 97 66
                  |41: 66 66
                  |119: 66 66 | 116 62
                  |83: 116 62 | 66 116
                  |11: 42 31
                  |101: 17 66 | 92 116
                  |28: 116 17 | 66 49
                  |98: 66 83 | 116 41
                  |93: 116 99 | 66 133
                  |72: 116 74 | 66 110
                  |69: 116 92 | 66 17
                  |20: 116 87 | 66 13
                  |57: 56 116 | 119 66
                  |5: 116 3 | 66 111
                  |54: 66 43 | 116 81
                  |37: 116 53 | 66 90
                  |13: 122 66 | 85 116
                  |16: 66 2 | 116 120
                  |81: 4 116 | 17 66
                  |73: 119 66 | 92 116
                  |88: 57 116 | 55 66
                  |92: 66 116 | 62 66
                  |18: 116 20 | 66 25
                  |77: 4 66 | 58 116
                  |56: 66 116 | 116 116
                  |95: 49 116 | 27 66
                  |7: 116 126 | 66 88
                  |59: 66 4 | 116 58
                  |127: 116 101 | 66 65
                  |48: 29 66 | 129 116
                  |52: 119 62
                  |15: 84 116 | 7 66
                  |24: 17 116 | 83 66
                  |102: 116 116 | 66 62
                  |40: 118 66 | 106 116
                  |85: 92 116 | 131 66
                  |109: 41 116
                  |114: 116 124 | 66 105
                  |30: 66 105 | 116 105
                  |67: 48 66 | 75 116
                  |131: 66 66 | 66 116
                  |105: 116 116
                  |111: 66 119 | 116 4
                  |51: 4 116 | 49 66
                  |78: 66 131 | 116 58
                  |104: 66 6 | 116 63
                  |129: 102 66 | 58 116
                  |74: 66 12 | 116 1
                  |89: 123 116 | 104 66
                  |27: 62 62
                  |6: 131 66 | 27 116
                  |99: 66 56 | 116 105
                  |68: 66 132 | 116 52
                  |0: 8 11
                  |17: 66 66 | 116 116
                  |39: 66 78 | 116 115
                  |80: 116 56 | 66 49
                  |21: 66 116
                  |29: 102 66 | 4 116
                  |23: 116 21 | 66 56
                  |115: 49 62
                  |46: 95 116 | 65 66
                  |125: 77 66 | 55 116
                  |112: 66 98 | 116 69
                  |128: 66 119 | 116 58
                  |64: 66 41 | 116 27
                  |79: 116 59 | 66 64
                  |118: 116 129 | 66 35
                  |75: 30 66 | 36 116
                  |14: 9 66 | 76 116
                  |19: 116 4 | 66 92
                  |94: 66 40 | 116 89
                  |44: 135 116 | 94 66
                  |
                  |babbbabababbababbabbabba
                  |aaaaaababaaaabbabbabbbbb
                  |babbaabbbabaabbbbabbaaaababbaabbbaabbbab
                  |babaabbbbbbabbaaaabaaaaa
                  |babaabbabbbbabbbababbbbbbababaaaaabaaabbbaaaaababbaababa
                  |abbabbabbaaabbbbbaabbaab
                  |abbbbabbaaabbabbaabaabaabaabbbbb
                  |bbbbaaaabaabaababbabaabaaabababb
                  |abaaaaaaaaaabbaabbabaaba
                  |bbbababbabbabbabbabaaaaa
                  |abbbbbabababbbaababababb
                  |baabaaaaababaaaabbbbbababbbbabbbabbaabba
                  |baaaabaabbabbaaababbabba
                  |babaabbbabbaaaaaaaababbabbababaaabaabbaaabababaabbbbbaabbbbbbaba
                  |ababbbbbababbbbbbbbaaaaa
                  |aabbbabbaaaabbababbbbaba
                  |bbaaaaaaaabaaabbabbaabba
                  |baabaabbbababbaaababababaaaabaababaaabaaaababaabbaababbbbbabbaabbaaabbab
                  |bbbbabbaababbbbabbabbbaa
                  |aaabbabababbaabbbbabbbaa
                  |bbbbaabbbbaabbbabbbbbabb
                  |abaaaaaabaabaaaabbabbaab
                  |ababababbabaabbbaaaaabba
                  |bababaaabbabbbbabaaabaaa
                  |aaaabaabbaabbaaaaabbabab
                  |baabbabbabaabbbaabbbaabaaabbaabaabbbaaababbbabba
                  |bbbbbbbbaabbabbbaabbbbba
                  |aaabbbbbaabbaababbaaabbb
                  |aabaabaaaaaaaabaaabbabab
                  |bbbbabbaaaaaabababbbaaba
                  |bbbbabbababaaabaabbaaaba
                  |aababbaaaaaabbaaabbaabaabbbbaaaa
                  |baaaabaabbbbbabaaaababaa
                  |abababababbaababaababbbbababaaaabbabaaabbbbbbabb
                  |aababbbabbbaababbbabaaba
                  |ababbbabababaaaaaaabbbbb
                  |ababaaaaaaaaabaaababbaabbabaababbaabbbaaaabbaababbaababa
                  |aaabbaabaaaabaaaaaababbbabbabbbbbbaabbab
                  |babaabbbaaabaabaaaabbaabbaaaaaab
                  |aaabbabbbbbabbbaaababbbbbbbbabbaaababbabbaabbbaa
                  |ababbbbaaaababbbbbbbbbbbaababbababababaa
                  |abbabbabaabbbaaabbaaabbb
                  |babaaabaaaaaaabbbabbababbbbbababbbababaa
                  |baabbbbaababbbabbbbbabaa
                  |abbabaababaaaaabaaaababb
                  |bbbabbbbbbaaabaaabbabbba
                  |ababaabbbbaaababaabbbaabaaabbbba
                  |ababbbaababaaaabaaaaabbaabbababbaabbabba
                  |ababbbbaaabbbaaabbbbbbaa
                  |bbaaabbabbababababaabbbb
                  |babababababbbbaabaaababb
                  |ababbabbaaaaabaaabbabbaa
                  |babaabbbbbabbaaabbaaaaab
                  |ababbbabbbaaababaabbabaa
                  |baabbabbababbbaabbbaabbb
                  |ababbbbbbaaabbaabaabbbab
                  |abaabbbababbaabbabbaaaba
                  |bbbaababbabaaabaabaabbaa
                  |ababababaaabbabbbbbaaaab
                  |baaabbaababababaabbbbbabaaabbbbb
                  |abaaaabaaabbabbbbaabbaba
                  |baabbabbaaabaabaaababbbbaabababbaaaaaaaa
                  |bababbabbabbaabbababababbabbbbabaabbbbbbabbabbbabbbbbaab
                  |aababbbababaaababaaaabaaabbaaabb
                  |bababaabbaaaabaabaaabbba
                  |bbbbaaabbbaaaababbbbbbbbbabbabbbbbabbabaaababbaaaabbabaa
                  |abaababbaabaabaaabaaaaaaabaababaaabaabbbaaababbaaaabbbabaaaaabba
                  |babbababaaabaabbbbabbaabbbbbbaabbbaaabbaaabbabbaaaabaaabbbaabbaabaaaababbbbbbbaaaababbab
                  |aababaaababaaabaabbaabbb
                  |abaabbbaaabbaaaabaabbaab
                  |aabbbbaaaaaabbababbbbbbb
                  |abbbababababaaaaabaaabba
                  |abbbabaaaabbbaaabbbbbaab
                  |bbbabbbbbbaabbbbbbaababa
                  |abbbbaabaabaabababababba
                  |aabbbabbaabababaabaabbbb
                  |abbbabaaaababbbbaabbbaaabaaabaaa
                  |bababababaaaaaaabbbbaaabbbaaaabaababbbaabaaabaab
                  |bbbababaababbbaabaabbaba
                  |aabbbaabaaabbabbbbbaababbaabaabaaabbabab
                  |bbabababbbabbababaaaabab
                  |bbbbbbbbbaaaaaababbbbaba
                  |bbbbaaabababbbabaaabbbba
                  |baaabbbbbaabaaaabbaababb
                  |ababababbaaaaaababbbaaab
                  |babbabbbbbaabbbbbbbababbbbbbbbbbaabbbaaaabaabaababbbbabaaabbababbbbabaab
                  |abbbaaaaaababbbbabbaaaab
                  |bbabababbababbbbaaababba
                  |babaaabababbbbaaabbaaabb
                  |bbababbababaaabbabbaaaabbaababbababbbaab
                  |aaaaaabbaaaabbbbabbabaababbbbaba
                  |bbaaababbbabaaaaaaabaaaa
                  |aabbbabbaabbabbbabbbbaaa
                  |bbaabbbaabaabbbaaababaaaabaaababbabbaaab
                  |bbabbaaababbbababbaabbbabbaababa
                  |bababbbbbbbbbabaabbbaaaa
                  |baaabbbbabbabbabaabaabaabaaaaababbaabbab
                  |bbaabbaaaaabbabbaabbaaab
                  |aaaabbbbaabbbabbbbbbabbaabababbabbbbbbaa
                  |ababbaabbbbababbaabaabaabaabaaba
                  |abbbababbabbbbababbaaaaa
                  |aababaaaabaaaaaaaaaaaaab
                  |aaaabbbbaabaaabbabbabaaa
                  |aabaabbababaabbaaaabababbbbababbaabbabbbbbbbabaabaababbababbabbbbbbababb
                  |bbbabababbaaaaaaaabaabaabbbbbaab
                  |bbabbabbbbbaaabaabababbbbaaabbabbbbaabab
                  |abbbbabbabaaaaaabaababaa
                  |abaaaabbbabbababaabaabba
                  |aabaaabbbababbbbbbabaaab
                  |bbbbbbbaabbbbbababbaabba
                  |aaabbaabbbaabbbbabbbbbba
                  |aaabaabaaaabbabbababaaba
                  |babbbbabbabbbbabaaaaababbabbbbbaaaabbbba
                  |aaaaabbbabbaabbaabaaabaababbaaaabbabbabbbababbbaaaaaabaabaabbbabaabbaaaababbbbbbbbaaaabbbaaaaaaa
                  |ababaabbbbaabbaaaabbbbaaaabababbbabaaababaaabaaabbbbbbaaaabbbbbb
                  |aaabbababbbabbabaaaaabbb
                  |babbabaabaabbaaabbaababa
                  |bbbabbbbbabbaaaabbbbabbabaabbbababbabaaabbaabaaabbabaaabbbbaabbb
                  |aaaabbaababababaabbbbaabaabbaabbbaabbbbb
                  |abbbabaaabbbbabbbaabbabbbbbabbbabaabbaaaaabbaaba
                  |ababbbabbaaaaaaabbaababb
                  |babbbbabbbaabbaaaaaaabaabbaabaaaaaaaaaab
                  |aaaabaaababbbbabbbabaaab
                  |aabbbbaaababbbbbbaabbbbb
                  |abbabbabaababbaaaabbbaaabbbbbbbbbabaaaabbbabbbabbaaaababbbabbbab
                  |ababbbaababaaaabbaababab
                  |bababaabaabaabaababbaaab
                  |bbbbbabaabbabaabaaabbaaa
                  |ababbaabbbaaabaababbaaab
                  |abaaaaaaababaabbbabbabaaaabbabab
                  |bbbababaaabbbbaaabaabbaa
                  |bbbbbbbbaabbbaabbbbaabbb
                  |ababbabbbabbaaaabbbaabba
                  |babaabbbbbbbaaababbabbaa
                  |aabaababbaabbbbabbaaababaaababbbababbababbbaaaaabaababaa
                  |bbabbaaabbbbaabaaabbbabbabbbaabbbbaabbabaabaabba
                  |aaaabbbbabaababaabbbbbabbbbbbaabbbaabaaa
                  |bbbabbababaaaaaabbbbabaa
                  |bbaaaabaabbbaaaabbabbbbb
                  |aabbabbabbbbaabbbbbabaabbabaaaaaaabaaaaabaaabbab
                  |abbbbaabbbaaabaaaaabbbba
                  |ababbbaabbbaaabbbaababaa
                  |abaaaabababaaaabbbbaaaaa
                  |aabbaaaabbbabbaabbbbaabbaabbabbbabbabaaabbbaabaaaaaaaaaa
                  |abbabaababbbbbaaaaababaa
                  |aaaaaabaabbbabbbabbbabbbbbaaababbbaaabaabaabaabaaaabbbabbababbba
                  |abbbabaaaaabaababbabbbaa
                  |aaababbbbaabbbbababbbabb
                  |bbaaababbabaaababbbaabababaabaabaaaaaaaa
                  |baabbaaaabaabababbbbaabaababbbbabaaaabbbbbbabbababaaabbb
                  |abbaababbbbabbbbbbbbaaaa
                  |abbabaabbbbbbabaaababaabaaaababa
                  |babaaaabbbbbaabbabaabbaa
                  |aaaabbaaaaaabbabaababbaababbbabaaabaaaaa
                  |babaaabaaabbaaaabaabbaba
                  |aabbabbbbaaaabbbaaabbabbababaaab
                  |aabaaabbabbaababbabaabaa
                  |bbabbaaaaabbaaaabaabbabbbabbabaabbabbaababbaaaaabbababbb
                  |aababbbbbababbababbbababaabaaaabbaabbbaa
                  |aabbaaaaaababaabbbbbabababbabaaabbbbabaaabbbbbbbbaabbaba
                  |babaaaabbbaaababaabaaabbbaaaaabbabaabaaa
                  |aaabbababbaabbaabbbbabbaababababbbaabbbbabababbaabaabbaabbaabaaabbbaaaba
                  |abbabaabbbbbaaabbbababbb
                  |baaaabbaaababbbbbaababbbabaaaababaaaabbabbaababb
                  |abaabbbabbabbaaabbaababb
                  |bbabbbbabababaabababbbaabaabbaaabaabbaaabbbaaaaabbbaaaabbbaababbaabbabaa
                  |aabaabaaaabbbbaababbbbaabbbbbbbbaabababb
                  |babbbbabbababbbbabbbbbba
                  |bbbbbababbbabbababbbbaba
                  |ababaabbababbbabbbbaaabb
                  |bbbbbaaababbaaaabbaaaaaabbabaabbaaababba
                  |bbbbaababbabbaaabaaaabab
                  |bbbbbababaaaaabaabaaabbbbabaaabbbabbabaaaaaabaabbaaabbababaababaaaaabaabbaabbbabbaabaaabbabbbabb
                  |baabaaaaababbabbbaaaabab
                  |bbaabbbbaaabaabaabababbb
                  |aaaabaaabbaaaabaabbaabbb
                  |aabaabaababbbbaabbbbbbbbbaaabbbababbaaab
                  |babababaaaabaabbbbbabaab
                  |babbbaaaaababaabaabbabaa
                  |aabbabbbbaaabbaabbbaabaa
                  |baabaaabbbaaababbababaabaaaabaaabbaaaaab
                  |bbabbbbaabaababaaaababaa
                  |bbbbbbbbaabaabaaaaabaabbabaabbbabbabbabb
                  |aabaabbbbaaaabaabaabaaababbbbbbababbabba
                  |bbbbbaaaabaaaabbabbaabaabbaabaaaabbabaaa
                  |babbabaaaaaabaabaabaabba
                  |abbabbabaaaaabaabbabbbaa
                  |bababaaaaababbbaabbbabba
                  |ababbbaaaaabbabaabbbaaba
                  |bbbababbaababaaaabaabaaa
                  |ababaaaabbbbaaabbaaabaab
                  |bbaaaababbabbababbabaaba
                  |baabbbabaaaaaaaababbbabbbbabaaaaabaabbaabaaaabbbababbbbabaabaaaabaaabaaaaaaabbab
                  |babbababaabaaaabaaaaaaabbaabbbabaababbaaaaabbbaabababaababbababaabbababa
                  |abaabababaaabbaaabaabbabbabbaaab
                  |babaabbbbababababbababaa
                  |babbbababbabaabbabbabaaa
                  |aaaabaabbbbababaabababaa
                  |bbaabbbbbaaaaabbabaabbaa
                  |bababaabaabbabbbababaaaababbabbbbabbbbbabbabbbab
                  |baaaabbbaabbbbbaababaabaabababbbbbabaabbbbaaaaaababaabaabbabbbbb
                  |aaaabbbabbaabbbababbabaababbabbbbababbbbbbaabaabaabababb
                  |bbbababbbaaaaaabaabaabbbaabbbbba
                  |babbbabaabbabaabbabaaaabaabababb
                  |aaaaaabbbaabaabbaaaabbba
                  |baabbbbaaaababbbaaaabaaabaaaaaba
                  |bbaaaabaaabaabbbabaaabab
                  |baababbbaaaabaaabbabbbaa
                  |aaaaaababbabababaababbbbabaaaabaabaaabbbbabaaaaa
                  |aabaabbbabaaaabbababaabbbababababbabbbab
                  |bbaaaaaaabaababaabababbb
                  |aaabaabbabbabababbbbbbbbbabbaabaaababbab
                  |abaababababbbabaaaaaababbabbbbbabbabbbab
                  |bababbbbababbbbbbbababaa
                  |abbbaaaababbababaababaabababbaaa
                  |bbbabbbbaabaabbbaabbabbb
                  |babbbababbabaabbbabbaaab
                  |baabbabbbabaabbbbaababab
                  |bbbbbabaabbbbabbbbaabaaa
                  |aaaabaaabbbaababaaabaabbbbbbbabb
                  |babababaaabbbabbbbaaabbb
                  |bbaaababbaaabbaabaabbaba
                  |abaaaabaababbbbabbabbaab
                  |aaaaababababaaaaababbbabababaaabbabbaaab
                  |aaaabaabbbbabbababbabaaa
                  |bbaaaabaaaabbabaaabbbaba
                  |babbbbaaaabaabaaabababbb
                  |aabbabbbbbaaabaaaaababbbaababaabbbbbbbbbabbaaaba
                  |bbbabbbaaabaaabbbbabbabb
                  |aaaaabaababaaaababaaabaa
                  |baabaabbabbbaaaaabbaaabb
                  |baabbaaaaaaabbabbaaaabab
                  |bbaaaababbabbbbbaaabaabababbabaaaaabbaabbabaaaaaaaaabbbbabbabbaaaababbbb
                  |ababaaaaabbbabbbaababbbaaaababbb
                  |aaabbabbaaababbbabaaabba
                  |aababababbabaaaaaaaababa
                  |abaabbbaabaaaaabbbbabbababaabbbaaaaababb
                  |babababababbbaaaababbaba
                  |baababaaabbbabbbbbaaabbbbbbbbbbbabbababbaabbbbba
                  |abaababbabaababaabbbaaba
                  |bababaabbbbbaabbabaaabaa
                  |abbbbbbaaababaabbbaabbbbabbaaaaaaaabababaababbaa
                  |baabaaaaaaabbabbbbabaaabbbabaabbbbabbaabaaababba
                  |bbabaaaababbbabaababbbbbabaababaabbaaabbbaaabaaa
                  |baaabbbbababaaaaabbabaaa
                  |aaabbabaabbbabaababbabba
                  |aabbbbaabaabaabbaaababaa
                  |aabbbbaabbbbbbbabbaaababbbbabaab
                  |abbbaabbbbbbaababbbbabab
                  |abbbabbbaababaaaaabaaabbaaaababb
                  |babbabaabababbbbabbbaaba
                  |aabaabbbbababaaabbbabbabbaaabaaa
                  |aaaabbabaabababaaaaabbabbaaaaabbaaaabaabbbabbabbbabbbaab
                  |bababaaabaaabbaaabbababb
                  |abaabbbabaaaaababaabaabbabaabaaabaabbabb
                  |ababababbabbaabbaabaaabbaababababaabbbbbaaabbbaa
                  |aaaabaabababbbaaabaaaabaaaaabbabaaabbbbb
                  |aaabbabbababaabbaabbbbba
                  |aaaabaabaabaabaaaababaaababbbbaabbbaabaa
                  |bababaaabababaaaabaabbbaababaaab
                  |babbaaaaaaaaabaaabaaabba
                  |bbabbabbaabbbbbbaabbabba
                  |aabaababbbabbaaaaababaabbaabbaaaaaaaabaaababaaabaabaaaababbbaabababaabaa
                  |aaaaaaabbbbaaaaaaaabbbbaabbbabba
                  |babbaaaaabbbaaaaaabbbbbb
                  |bbbbbababaaabbbbaabbbbba
                  |bbbbbaaaaababbbaabaabbbabbaaabababbbbbaa
                  |aaabbabbaaabbaabaabaaabbbbbbaaabaaaabaaaaabbbabbabbaaabbabbbaaabbaabaababbaaabbb
                  |ababbaabaababbbbaababbbbabbabbbbbbbabaab
                  |abbbabbbababbabbabbbbbba
                  |ababbbaababbababaaaaabbb
                  |aabbbaabbabaaaabbbababba
                  |aabbbbaaabbabbabbabbbbba
                  |baaaaabbbaaaabbabaabbaaaaaabbaaa
                  |aabaaabbbbabbaaabbababaa
                  |aaabbababaaaaabbabaaabbb
                  |bbaabbbaabbabaabbaaaabaaaabbbaabaaaaaabaaaaaaaaaaaabbaaa
                  |aaabaabbbabbbbaabaabaaabaaaaaabaaaaaaabbaabbbbba
                  |baaaabbaaaaaabbbabbbbbabaababababbbabababaababbabbabbabababbbbaa
                  |babaaaabbbbbbababaababba
                  |babaabbbaabbbbaabbbbabab
                  |bbaabbbbbabbabaaaabaaaaa
                  |abbbabbbbabaabbbaaaaaaab
                  |bbaabbbabbaaaababbbabaab
                  |ababbbbabbbbaabbbaaababa
                  |aabbbaabbabbbababbababaa
                  |aaaabaaaabbbabbbbaababba
                  |abbbbbaabbaabbaaaababbab
                  |babbbaabaaabbbaabbbbabaabbabaabababababaaababbabaabbbaaa
                  |abaabababbabbabaabaabbbabbbbbabbabbbaaab
                  |ababbbabbababbbbaaababbbbbbbbbaa
                  |baaabbaaaaaaabaaabaabaaa
                  |aaaaabaaababbbbabaaabaab
                  |abaababababbabaaabbbbabbbababbaa
                  |bbabbbbaaaabbbababbbaaabbaaababaabbaaabbbbbbaaaa
                  |bababbabaaabbabaabbaaabb
                  |aaaaabababbbabbbaaabbabbaabbabbbbaaaabab
                  |abaaaabbabbbbbabbbbabbbaababaaaa
                  |bababaabaaaabbbbbaabaaba
                  |bbabaabbaaabbbbbbbaabbab
                  |abbbbaaabaaabbabbbabbbbabbbaaaaabbbababaaaabbbababababab
                  |babaababbbbbaabbaaabbaabaabbabba
                  |ababbaabbbbabbaababaaaaa
                  |abbabbabbbbbaabaaabaaaaa
                  |abbabbabbbbbbabababbbbba
                  |bbbaabbbbbbaaabbbaabaabbbabbaababaabbbbabaabbabbbabbbbab
                  |bbbbabbaabbbbabbbbbbabaa
                  |abbbbbaabaabaaaaababbaba
                  |aabababababbbaaaabbbaabbbabaaabbaabbaabb
                  |baaabbaaababaaaaabbbbbbb
                  |aaabaababababbabbbabbabb
                  |aabbbbaabbbabababbbbabab
                  |bbabbbbaabbbbbababbaaabb
                  |aaaabaabaaaabbaabbbabbaaaaabbababbbaaaabbbbaaaaa
                  |bababaaaaaaabaabaabbabbbababbaaa
                  |abbabbabbbbbabbbbaabaaabaabbabab
                  |abaabbbabbbababaaaaaaababbaababa
                  |bbbbabaabbbabbababbbbababbaababbbbababbbbabababaaaababbabababbaa
                  |bbbabbbaaaababbbbbbbbaaabbbbbabb
                  |baaaabbaaaaabbaababaaaaa
                  |babbababbabbbbabbaababba
                  |aabbbabbbaaaabbabaaabaab
                  |aaabaabaabbbbabbaaaabaaaabaabbbb
                  |aaabbabbbbabaabbbaaaaaba
                  |aaaaaababaaabbbbbbbaabaa
                  |aabaabaabaaabbaabbaaabbaaabbaabb
                  |abbababaaaaabbbbbbabaaaabaaaabbbaaabaabb
                  |abbaababababbbabaaabbaabbbaaaababbabbababaabbbbbbbaaaabb
                  |abbaaabbabaaaabaaabaaabbbbabababbbbbbbabaabbbaaaaaabaabb
                  |baaaabbaaaabbaabbabaababbabbbabbabbaaaba
                  |aaaabbaaabaaaaaabaabbbbb
                  |bbbbaabaaabaaabbabbaababbaabbbbbbaabbbab
                  |babaaabaaaaaabaabaaabbab
                  |baaabbaaabbbabbbaaaababa
                  |babbbaabbaaababbbbabbaabbbabaaababaabbabbaaaaaaa
                  |aababbaaaabaaabbbaabbaab
                  |abbbaabbaaabaabbaabababb
                  |aababbaabbabaabbababbbbaaabaabaaaaaabbbbababbbabbbaababa
                  |aabaababbbbbaababaababba
                  |bababababbbbbababbababba
                  |aabbbbabbaabbbbaabbaabbb
                  |baaaaabbbaaaabaaabaabbbb
                  |abbbababaaaabaabbbaabbab
                  |babaabbbbaaaabbbbaaababb
                  |aabbbbababbbbabbbabbbaab
                  |baabaabbbbbabaaaababbabbbbaaababaabaabba
                  |babbaabbaababbbaaabbabba
                  |bbaabbbbaabbbaabbabababb
                  |ababaabbbbabbbbaabababbb
                  |aababaababbbbbabbababbbbaabbabbbaaababbbabbabaaa
                  |bbbbbaaabababbabbbaaabbb
                  |bbaabbbaabbaabaabbbaabba
                  |abbbbabbbbbbabbabbabbbbaabbbbbabaabbababbbaababa
                  |babaababbababaabaabaababbaaaaaaabbabaaaaabaaabbbabbabbba
                  |bbabababbaabbbbaaaaabaaaaaabbbbb
                  |abaabababbaabbbbaaabbaaa
                  |aaabaabaabbbbabbabababba
                  |bbbabbabbbbababbbbaabbab
                  |bbbbabbbaabababbaabaabbbabaaabaa
                  |babaaaabaabaababababbbabbabbabbbaabbaaaabaaababababbbabbabbaaaab
                  |aaaaaabbbbbbbbbababaabbbabbbaaaabbbaabbaaabbbaba
                  |babbababbaababbbababbaaa
                  |baabaabbabbbaaaaabbabaabbaaaabaabbbbbbbbbabbbaab
                  |bbbababbbbaaabaabaaabbba
                  |bbabaabbbabaaababaaaaaba
                  |abaabbabaabbbbaabbabbbaa
                  |abbaababbbbbaababbbbabab
                  |ababaaaaabbbaabbbbaaaabb
                  |babaaaabbabaaabaaaabbaaa
                  |aabababaaabababaabbaabbb
                  |abbbbbabbaabaaabbaaaabab
                  |baaabbaababbaabbaabbabbabaaaaaba
                  |aaaabaaaaabaabbbabbbbaba
                  |abaabbabbaaabbaababbaaaabaaabbbaabbabbaa
                  |aabaaabbababbaabbbaabbbaabaaaabaaaabbbab
                  |abaaaaaabababbbbabbaaaba
                  |abbbbaabaabbbabbbabbababaabbababbababbba
                  |aabaababbabaabbbaababbbaabaababbbbbabbaabbaabaaaabaabaab
                  |bbabaabbaaababbbabbaaaaa
                  |bbbabbbaaabbabbbaabbabab
                  |bbaaaababbbabbaabbbbaaabbabaaaabbbbabbbaaabababababbabba
                  |bbaaaaaabbbabbaaaaabbbbbaababaabababbbbabbabbbbaabbbbbabaabaaaabbbbabbbbbbaaabbbbaaaaaaaaabaaaab
                  |baaaabbaaaaaaababbaabaaa
                  |bbbbaababbaaabaabbababba
                  |abbaabaaabbbaabbaaabbbba
                  |aaabbabababbbbabbabbbababbaaaaaaaaabaaab
                  |baabbbbaaababbbbababbaaa
                  |bbabaaaaababbabbbabaabaa
                  |bbaabbbbabaaaabababbbbbb
                  |abaabbbaaabababaaaaabbba
                  |ababbbbbababbbaaaaaaaaab
                  |aaaaababbabbabaaaabbaabb
                  |bbaaabbabaabaaabaabbbbababbaaabb
                  |ababbbbaabbbbaabaabaaaba
                  |baabbaaabbbbbabaabbaaaaa
                  |baaaabbbbbababababaababababababaaabaababbaabaaaabbbaabba
                  |baabaaaabbbbbababaaaaaba
                  |abbbaabbbabbaaaaaaabbbaa
                  |abbaaabbaabbaabaaabbaabbbbaabaaa
                  |abbbbaabaabbbaabbbabbaab
                  |bbabaaaabbbababbbbaaabbb
                  |abaababbbaaaabbaaababaabaaaabbabbabbbbbb
                  |babaabbaaabababababbbbaabbbbbbab
                  |aaaabbabbbbabaaabbaabbab
                  |abbbabaabbaabbbaaabbaaaabaaabbbbbabaabbbabbbaaab
                  |ababbbbaababaaaabbbaaaba
                  |abaaaaaaaaabaabaaabbabaa
                  |aababaabbaaabbbbbbaababa
                  |aaaabbabbbbbbbbaaabbbaaaaaaaababaabaababaababbabaaaaabba
                  |bbbbaabbaabbbaaaabbaaaab
                  |bbabbbbabbbbaabaabbbbaba
                  |ababaaaaababaaaaababbaaabaaaabaabbababaaaabbbabb
                  |ababaaaabbaaabaaaabaaaba
                  |aababbaaaaabbabaaaaaaaaa
                  |bbaabababbabaaaabbbaabbabbbaaabbbbbbbabbabaaabaa
                  |abbbbabbaabbbbabbaababaa
                  |bbaaaababaabbabbabbabbabbababaaaabaabbaa
                  |aabaabaabbabbbbaabaabbaa
                  |ababababbaaabbaaaabababb
                  |baabbaaabababbabaaabbbaa
                  |baaaaaabbaaaaaaabaaaabbababbaaba
                  |aababbbbaaaabbaabbbabaab
                  |bbaaaabaaaaaababbbaaaababbbaababababbbab
                  |bbabbababbbababbabaabaaa
                  |abbbababbaabbaaaababaaaabbabbbab
                  |baabbaabaaaaaabaabababaabbbaaabbababbabbbabaaaabaaabaaabaaaaabbaababbaaa
                  |babaabbbbbbbbababbbaaaba
                  |babaaabaabaaaaabababbbbaabbbaaab
                  |bbbbbbbbaababbaabaababababbaabaabbbaabaaaabbbbba
                  |aaabaababbbabaaaabababaa
                  |babababababbbaababaaaabbbabaaabbaababbaabbbabbaabbaaabbabbababbaabaabbaa
                  |bababababaabbbbabbbaaaaa
                  |bbbbbaaabaaaaaabbbaabbbaaaaaaabaababaaab
                  |aaaabaabababbbbbabbbbaaa
                  |abbbbbbaaabaababbbaaaaabaaaabbabbabbbbabaabbaabbbbaaababababbbbb
                  |baababbbabbbbabbaaababaa
                  |ababbbbaabbbaabbbbabbaab
                  |bbbbaabbaaabaabaabbabbba
                  |ababaabbbababbbbbabbbabb
                  |baaaabbaaababbaabaaaabaabbaaaaaabbaaaabb
                  |abbbabaaaaaabbabbababbaa
                  |abaaabbbababbbbabbbbabbaababbabbbabbaabbabaabaaaabbaabaa
                  |bbbabbbabaaaabbbbbbbbbab
                  |bbabababbaaaabaabbbaabba
                  |baabbbbaaabbbaaaaaababba
                  |baabaaabbabaaababbaababb
                  |abaaaabbaabaaabbbbbaabbb
                  |bbababababaabbbaaaaabbbbabbbaaaaabbabababbbbbaabaababbab
                  |ababaaaaaabbbaabbbabbaba
                  |aabaabbbbbaaabaabaabbaba
                  |abbbabaababbabaabbaababa
                  |bbabababbbbabababaabaaabbbababaa
                  |bbbbaabbabbaabaaaaaaaaab
                  |aababbbbaaaaaababbbaaaba
                  |aababaabaaabbabaaabaabba
                  |babaabbaaabaabaaaaaaaabaaabaaaba
                  |ababaaaaababaabbbbbaabbb
                  |bbbbabbaaababbbbbbaaabbabaaababa
                  |bbbabbbaaabaabbbbbaabaaa
                  |abbabbabbaabbaaababbbabb""".stripMargin.split("\n")

    println(solveRegex(input.toList)(generateRegex(_)(_)))
    println(solveRegex(input.toList)(generateRegexWithRulesOverride(_)(_)))

    println(solveCYKPart01(input.toList))
    println(solveCYKPart02(input.toList))
  }
}
