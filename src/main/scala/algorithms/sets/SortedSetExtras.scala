package algorithms.sets

import scala.collection.mutable

object SortedSetExtras {

  /**
   * Reverse iterator extension for SortedSet/TreeSet
   */
  implicit class ReverseIterator[A](val set: mutable.SortedSet[A]) extends AnyVal {

    def reverseIterator: Iterator[A] = new Iterator[A] {
      var upNext: Option[A] = None
      var upNextComputed: Boolean = false

      private def recompute(): Unit = {
        if (!upNextComputed) {
          upNext = upNext match {
            case Some(value) => set.until(value).lastOption
            case None => set.lastOption
          }
          upNextComputed = true
        }
      }

      @scala.annotation.tailrec
      override def hasNext: Boolean = if (upNextComputed) upNext.nonEmpty else {
        recompute()
        hasNext
      }

      @scala.annotation.tailrec
      override def next: A = {
        if (upNextComputed) {
          upNext.foreach(_ => upNextComputed = false)
          upNext.get
        } else {
          recompute()
          next()
        }
      }
    }
  }

}
