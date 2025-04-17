package io.joern.rubysrc2cpg.utils

class FreshNameGenerator[T](template: Int => T) {
  private var counter: Int = 0
  def fresh: T = {
    val name = template(counter)
    counter += 1
    name
  }

  def current: T = {
    template(counter - 1)
  }
}
