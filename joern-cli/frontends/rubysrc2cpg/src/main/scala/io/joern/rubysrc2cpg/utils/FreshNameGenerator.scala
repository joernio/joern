package io.joern.rubysrc2cpg.utils

import scala.util.matching.Regex



class FreshNameGenerator[T](template: Int => T, val extract: PartialFunction[T, Int]) {
  private var counter: Int = 0
  def fresh: T = {
    val name = template(counter)
    counter += 1
    name
  }
}

object FreshNameGenerator {
  private def extractFromRegex(r: Regex): PartialFunction[String, Int] = {
    val ParseInt: PartialFunction[String, Int] = ((x: String) => x.toIntOption).unlift
    {
      case r(ParseInt(i)) => i
    }
  }

  def apply[T](template: Int => T, extract: PartialFunction[T, Int] = FreshNameGenerator.noMatch) = new FreshNameGenerator[T](template, extract)
  def apply(template: Int => String, regex: Regex) = new FreshNameGenerator[String](template, extractFromRegex(regex))
  def noMatch[T, U]: PartialFunction[T, U] = ((_: T) => None).unlift
}
