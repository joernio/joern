package io.joern.x2cpg.datastructures

import scala.collection.mutable

class MutableQueue[A] extends mutable.Queue[A] {

  def dequeueAll: Seq[A] = dequeueAll(_ => true)
}

object MutableQueue {
  def empty[A]: MutableQueue[A] = new MutableQueue
}
