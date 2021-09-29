package google.kickstart._2020.H

import scala.annotation.tailrec

object Test {
  def repeatedly[A](chop: String => (A, String))(trim: String => String)(input: String): List[A] = {

    @tailrec
    def loop(current: String, acc: List[A] = Nil): List[A] = {
      current match {
        case "" =>
          acc.reverse
        case _ =>
          val (a, next) = chop(current)
          loop(trim(next), a :: acc)
      }
    }

    loop(input)
  }

  def repeatedly[S, A](chop: S => (A, S))(trim: S => S)(input: S): Stream[A] = {
    val (a, next) = chop(input)
    a #:: repeatedly(chop)(trim)(trim(next))
  }

}
