/**
  * Created by gkalra on 1/3/18.
  */
class forAndforeach {

  def filterOddIndexedElements(arr: List[Int]): List[Int] = arr.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)

  def sumOddElements(arr: List[Int]): Int = {
    arr.filter(_ % 2 != 0).sum
  }

  def f(num: Int): List[Int] = {
    (for (i <- 0 to num - 1) yield i) toList
  }

}
