import scala.annotation.tailrec

/**
  * Created by gkalra on 1/4/18.
  */
object folding {

  def f(arr: List[Int]): List[Int] = arr.foldLeft(List[Int]())((acc, v) => v :: acc)

  def f1(arr:List[Int]):Int = arr.foldLeft[Int](0)((_, count) => count+1)

  def ePowerx(x: Double): Double = {
    (2 to 9).foldLeft[Double](1 + x)((res, i) => res + math.pow(x, i) / factorial(i, i))
  }

  @tailrec
  def factorial(x: Double, result: Double): Double = {
    if (x == 1) result
    else factorial(x - 1, result * (x - 1))
  }
}
