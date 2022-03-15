package io.joern.jssrc2cpg.datastructures

class OrderTracker(private var _order: Int = 1) {
  def inc(): Unit = {
    _order += 1
  }

  def order: Int = _order
}
