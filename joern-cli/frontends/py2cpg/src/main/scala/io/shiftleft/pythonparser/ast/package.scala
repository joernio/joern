package io.shiftleft.pythonparser

import scala.collection.mutable

package object ast {
  type CollType[T] = mutable.Seq[T]
}
