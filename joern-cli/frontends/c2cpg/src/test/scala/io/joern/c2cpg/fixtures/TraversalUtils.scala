package io.joern.c2cpg.fixtures

import io.shiftleft.codepropertygraph.generated.{NodeTypes, Properties}
import org.scalatest.matchers.should.Matchers
import overflowdb.{Node, _}
import overflowdb.traversal._

trait TraversalUtils extends Matchers {

  protected val fixture: TestProjectFixture

  def getMethod(name: String): List[Node] = {
    val result =
      fixture.traversalSource
        .label(NodeTypes.METHOD)
        .has(Properties.NAME -> name)
        .l

    result.size shouldBe 1
    result
  }

}
