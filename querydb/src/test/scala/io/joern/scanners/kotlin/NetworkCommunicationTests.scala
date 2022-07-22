package io.joern.scanners.kotlin

import io.shiftleft.codepropertygraph.generated.nodes.{Call}
import io.joern.suites.KotlinQueryTestSuite
import io.joern.console.scan._

class NetworkCommunicationTests extends KotlinQueryTestSuite {

  override def queryBundle = NetworkCommunication

  "should match on all multi-file positive examples" in {
    val q = queryBundle.nopTrustManagerUsed()
    q.multiFileCodeExamples.positive.filter(_.nonEmpty).foreach { example =>
      val first = example(0)
      val cpg   = code(first.content, first.filename)
      val finalCpg = example.drop(1).foldLeft(cpg) { (c, e) =>
        c.moreCode(e.content, e.filename)
      }
      q(finalCpg).flatMap(_.evidence).collect { case c: Call => c }.size shouldBe 1
    }
  }

  "should not match on all multi-file negative examples" in {
    val q = queryBundle.nopTrustManagerUsed()
    q.multiFileCodeExamples.negative.filter(_.nonEmpty).foreach { codeExample =>
      val first = codeExample(0)
      val cpg   = code(first.content, first.filename)
      val finalCpg = codeExample.drop(1).foldLeft(cpg) { (c, e) =>
        c.moreCode(e.content, e.filename)
      }
      q(finalCpg).flatMap(_.evidence).collect { case c: Call => c }.size shouldBe 0
    }
  }
}
