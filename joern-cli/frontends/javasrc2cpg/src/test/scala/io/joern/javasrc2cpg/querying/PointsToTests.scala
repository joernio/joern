package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.iterableToTraversal

import scala.jdk.CollectionConverters.IteratorHasAsScala

class PointsToTests extends JavaSrcCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String =
    """
      |class C {
      |
      | public C f;
      |
      | public void m() {
      |   return;
      | }
      |
      |}
      |
      |class A extends C {}
      |
      |class B extends C {}
      |
      |class Foo {
      |
      | static void foo() {
      |   var p = new A(); // alloc 1
      |   var q = p;
      |   var r = new B(); // alloc 2
      |   p.f = r;
      |   var t = bar(q);
      |   t.m();
      | }
      |
      | static C bar(C s) {
      |   return s.f;
      | }
      |
      |}
      |""".stripMargin

  "foo should contain two allocation sites" in {
    cpg.method("foo").call("<operator>.alloc").size shouldBe 2
    val List(newA, newB) = cpg.method("foo").call("<operator>.alloc").l
    newA.name shouldBe Operators.alloc
    newA.code shouldBe "new A()"
    newB.name shouldBe Operators.alloc
    newB.code shouldBe "new B()"
  }

  "all identifiers should (conservatively) point to their allocation site" in {
    val List(newA, newB) = cpg.method("foo").call("<operator>.alloc").l
    newA.in("DATA_FLOW").asScala.collectAll[AstNode].code.dedup.l shouldBe List("p", "q")
    newB.in("DATA_FLOW").asScala.collectAll[AstNode].code.dedup.l shouldBe List("r")
  }

}
