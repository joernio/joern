package io.joern.jimple2cpg.datastructures

import sootup.core.jimple.basic.Local
import sootup.core.model.{Body, SootClass, SootClassMember, SootField}
import sootup.java.core.JavaSootClass

/** A temporary wrapper class until something like a <a
  * href="https://github.com/soot-oss/SootUp/pull/818">HasPosition</a> interface is merged.
  */
class PositionalElement(maybeSootObject: Object) {

  def getFirstCol: Option[Integer] = maybeSootObject match
    case x: SootClass[_]       => Option(x.getPosition.getFirstCol)
    case x: Body               => Option(x.getPosition.getFirstCol)
    case x: SootClassMember[_] => Option(x.getPosition.getFirstCol)
    case x: Local              => Option(x.getPosition.getFirstCol)
    case _                     => None

  def getFirstLine: Option[Integer] = maybeSootObject match
    case x: SootClass[_]       => Option(x.getPosition.getFirstLine)
    case x: Body               => Option(x.getPosition.getFirstLine)
    case x: SootClassMember[_] => Option(x.getPosition.getFirstLine)
    case x: Local              => Option(x.getPosition.getFirstLine)
    case _                     => None

  def getLastCol: Option[Integer] = maybeSootObject match
    case x: SootClass[_]       => Option(x.getPosition.getLastCol)
    case x: Body               => Option(x.getPosition.getLastCol)
    case x: SootClassMember[_] => Option(x.getPosition.getLastCol)
    case x: Local              => Option(x.getPosition.getLastCol)
    case _                     => None

  def getLastLine: Option[Integer] = maybeSootObject match
    case x: SootClass[_]       => Option(x.getPosition.getLastLine)
    case x: Body               => Option(x.getPosition.getLastLine)
    case x: SootClassMember[_] => Option(x.getPosition.getLastLine)
    case x: Local              => Option(x.getPosition.getLastLine)
    case _                     => None

}

implicit class SootObjectToPositionalElement(sootObject: Object) {

  def toPositional: PositionalElement = PositionalElement(sootObject)

}
