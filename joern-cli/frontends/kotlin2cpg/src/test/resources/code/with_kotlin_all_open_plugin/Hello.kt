package io.joern.placeholder

annotation class AllOpenAnnotation
@AllOpenAnnotation
class WithAnnotation

class AClass : WithAnnotation() // compiler spits out error if `all-open` plugin not set up

fun main() {
  println("Hello, world!")
}

