package adventOfCode

/**
 *
 * Each of your joltage adapters is rated for a specific output joltage (your puzzle input). Any given adapter can take
 * an input 1, 2, or 3 jolts lower than its rating and still produce its rated output joltage.
 *
 * In addition, your device has a built-in joltage adapter rated for 3 jolts higher than the highest-rated adapter in your
 * bag. (If your adapter list were 3, 9, and 6, your device's built-in adapter would be rated for 12 jolts.)
 *
 * Treat the charging outlet near your seat as having an effective joltage rating of 0.
 *
 * Since you have some time to kill, you might as well test all of your adapters. Wouldn't want to get to your resort
 * and realize you can't even charge your device!
 *
 * If you use every adapter in your bag at once, what is the distribution of joltage differences between the charging outlet,
 * the adapters, and your device?
 *
 * part: 02
 *
 * you'll need to figure out how many different ways they can be arranged. Every arrangement needs to connect the charging
 * outlet to your device. The previous rules about when adapters can successfully connect still apply.
 */
object _10_AdapterArray {

  def solve_part_01(input: Seq[Int]): Int = {
    val sorted = input.sorted

    val map: Map[Int, Int] = (0 +: sorted :+ sorted.last + 3)
      .sliding(2)
      .map { case Seq(a, b) => b - a }
      .toList
      .groupBy(identity)
      .mapValues(_.size)

    map.getOrElse(1, 0) * map.getOrElse(3, 0)
  }

  def solve_part_02(input: Seq[Int]): Long = {

    val max = input.max
    val size = max + 3 + 1
    val dp = new Array[Long](size)

    dp(0) = 1
    input.foreach(dp(_) = 1)
    dp(max + 3) = 1

    (1 until size).foreach { i =>
      dp(i) *= {
        (if (i - 1 >= 0) dp(i - 1) else 0) +
          (if (i - 2 >= 0) dp(i - 2) else 0) +
          (if (i - 3 >= 0) dp(i - 3) else 0)
      }
    }

    dp.last
  }

  def main(args: Array[String]): Unit = {
    val input = """18
                  |47
                  |144
                  |147
                  |124
                  |45
                  |81
                  |56
                  |16
                  |59
                  |97
                  |83
                  |75
                  |150
                  |33
                  |165
                  |30
                  |159
                  |84
                  |141
                  |104
                  |25
                  |164
                  |90
                  |92
                  |88
                  |2
                  |8
                  |51
                  |24
                  |153
                  |63
                  |27
                  |123
                  |127
                  |58
                  |108
                  |52
                  |38
                  |15
                  |149
                  |66
                  |72
                  |21
                  |46
                  |89
                  |135
                  |55
                  |34
                  |37
                  |78
                  |65
                  |134
                  |148
                  |76
                  |138
                  |103
                  |162
                  |114
                  |109
                  |42
                  |77
                  |102
                  |163
                  |7
                  |105
                  |69
                  |39
                  |91
                  |111
                  |131
                  |130
                  |6
                  |137
                  |96
                  |82
                  |64
                  |3
                  |95
                  |136
                  |85
                  |9
                  |116
                  |17
                  |99
                  |12
                  |117
                  |62
                  |50
                  |110
                  |26
                  |115
                  |71
                  |57
                  |156
                  |120
                  |98
                  |1
                  |70""".stripMargin.split("\n").map(_.toInt)

    println(solve_part_01(input))
    println(solve_part_02(input))
  }
}
