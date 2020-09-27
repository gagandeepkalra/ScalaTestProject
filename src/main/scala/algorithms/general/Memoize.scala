package algorithms.general

import scala.collection.mutable

object Memoize {
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }.asInstanceOf[I => O]
}
