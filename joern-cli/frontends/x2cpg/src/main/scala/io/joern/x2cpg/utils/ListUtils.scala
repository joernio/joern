package io.joern.x2cpg.utils

import scala.annotation.tailrec

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
      takeUntilRec(list, Nil, predicate).reverse
    }

    @tailrec private def takeUntilRec(
      originalListSegment: List[T],
      resultAcc: List[T],
      predicate: T => Boolean
    ): List[T] = {
      originalListSegment match {
        case Nil => Nil

        case head :: _ if predicate(head) => head :: resultAcc

        case head :: tail => takeUntilRec(tail, head :: resultAcc, predicate)
      }
    }
  }
}
