package io.joern.jssrc2cpg.parser

import ujson.AstTransformer
import ujson.Value
import upickle.core.Util
import upickle.core.Visitor
import upickle.core.compat.Factory
import upickle.core.ArrVisitor
import upickle.core.ObjVisitor

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** Custom visitor that mimics the behavior of ujson.Value. Provides a fix for:
  * https://github.com/joernio/joern/issues/2440
  */
object JsValueVisitor extends AstTransformer[Value] {

  private val adapted = Value

  private class JsAstObjVisitor[T](build: T => Value)(implicit factory: Factory[(String, Value), T])
      extends adapted.AstObjVisitor(build) {
    override def subVisitor: JsValueVisitor.type = JsValueVisitor.this
  }

  private class JsAstArrVisitor[T[_]](build: T[Value] => Value)(implicit factory: Factory[Value, T[Value]])
      extends AstArrVisitor(build) {
    override def subVisitor: JsValueVisitor.type = JsValueVisitor.this
  }

  override def transform[T](j: Value, f: Visitor[_, T]): T =
    adapted.transform(j, f)

  override def visitJsonableObject(length: Int, index: Int): ObjVisitor[Value, Value] =
    new JsAstObjVisitor[mutable.LinkedHashMap[String, Value]](xs => ujson.Obj(xs))

  override def visitArray(length: Int, index: Int): ArrVisitor[Value, Value] =
    new JsAstArrVisitor[ArrayBuffer](xs => ujson.Arr(xs))

  override def visitNull(index: Int): Value =
    adapted.visitNull(index)

  override def visitFalse(index: Int): Value =
    adapted.visitFalse(index)

  override def visitTrue(index: Int): Value =
    adapted.visitTrue(index)

  override def visitString(s: CharSequence, index: Int): Value =
    adapted.visitString(s, index)

  override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Value = {
    val value: Double = if (decIndex != -1 || expIndex != -1) {
      s.toString.toDouble
    } else {
      // We accept the loss of precision here.
      // See: https://github.com/joernio/joern/issues/2440
      Try(Util.parseIntegralNum(s, decIndex, expIndex, index)) match {
        case Success(num)                      => num.toDouble
        case Failure(_: NumberFormatException) => s.toString.toDouble
        case Failure(other: Throwable)         => throw other
      }
    }
    ujson.Num(value)
  }

}
