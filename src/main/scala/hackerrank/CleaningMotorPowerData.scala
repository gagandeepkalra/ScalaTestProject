package hackerrank

/*
Different power unit conversions to watts
 */
object CleaningMotorPowerData {

  def power_conversion(raw_power: String): String = {
    val pattern = "^(.*?)([gmk])?w$".r

    val result = raw_power.toLowerCase match {
      case pattern(n, "k") =>
        Some(scala.math.BigDecimal(n) * scala.math.BigDecimal(1000))
      case pattern(n, "m") =>
        Some(scala.math.BigDecimal(n) * scala.math.BigDecimal(1000000))
      case pattern(n, "g") =>
        Some(scala.math.BigDecimal(n) * scala.math.BigDecimal(1000000000))
      case pattern(n, null) => Some(scala.math.BigDecimal(n))

      case _ => None
    }

    result
      .map(_.bigDecimal.toPlainString)
      .getOrElse("")
  }

  def main(args: Array[String]): Unit = {
    val raw_power = io.StdIn.readLine

    val result = power_conversion(raw_power)

    println(result)
  }

}
