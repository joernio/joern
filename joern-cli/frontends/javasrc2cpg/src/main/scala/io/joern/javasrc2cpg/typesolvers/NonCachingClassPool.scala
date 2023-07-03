package io.joern.javasrc2cpg.typesolvers;

import javassist.{ClassPool, CtClass}
import scala.annotation.nowarn

/** The NonCachingClassPool is meant to be used in conjuction with a type solver that already caches resolved types.
  * This means that caching the intermediate ctClasses is just extra memory use.
  *
  * NonCachingClassPool extends ClassPool(useDefaultPath = false) to avoid adding the system path to the search list.
  */
class NonCachingClassPool extends ClassPool(false) {
  @nowarn override def cacheCtClass(className: String, ctClass: CtClass, dynamic: Boolean): Unit = ()
}
