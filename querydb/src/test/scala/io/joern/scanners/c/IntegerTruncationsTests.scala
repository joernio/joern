package io.joern.scanners.c

import io.joern.suites.CQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language._
import io.joern.console.scan._

import scala.collection.immutable.AbstractSeq

class IntegerTruncationsTests extends CQueryTestSuite {

  override def queryBundle = IntegerTruncations

  "find truncation in assignment of `strlen` to `int`" in {
    val results = queryBundle.strlenAssignmentTruncations()(cpg)
    results.foreach(r => println(r.evidence))
    queryBundle.strlenAssignmentTruncations()(cpg) match {
      case List(result) =>
        result.evidence.toList match {
          case List(x: nodes.Identifier) => x.method.name shouldBe "vulnerable"
          case _                         => fail()
        }
      case _ => fail()
    }
  }

}
