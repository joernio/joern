package io.joern.pythonparser

import scala.collection.mutable

package object ast {
  type CollType[T] = mutable.Seq[T]
}
