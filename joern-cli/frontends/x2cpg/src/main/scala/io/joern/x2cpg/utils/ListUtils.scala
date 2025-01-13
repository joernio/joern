package io.joern.x2cpg.utils

object ListUtils {
  extension [T](list: List[T]) {

    /** Return each element in the list up to and including the first element which satisfies the given predicate, or
      * return the empty list if no matching element is found, e.g.
      *
      * List(1, 2, 3, 4, 1, 2).takeUntil(_ >= 3) => List(1, 2, 3)
      *
      * List(1, 2, 3, 4, 1, 2).takeUntil(_ >= 5) => Nil
      */
    def takeUntil(predicate: T => Boolean): List[T] = {
      list.indexWhere(predicate) match {
        case index if index >= 0 => list.take(index + 1)
        case _                   => Nil
      }
    }

    /** Returns the single element, or None if the list is empty or contains more than one element. */
    def singleOrNone: Option[T] = if list.size == 1 then list.headOption else None
  }
}
