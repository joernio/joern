package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.iterableToTraversal

import scala.jdk.CollectionConverters.IteratorHasAsScala

class PointsToTests extends JimpleCodeToCpgFixture {

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
      | static void baz() {
      |   int[] p = { 1, 2, 3, 4 }; // alloc 1
      |   int[] r = new int[6]; // alloc 2
      |   var q = p;
      |   p = r;
      | }
      |
      |}
      |""".stripMargin

  "foo should contain two allocation sites" in {
    cpg.method("foo").call(Operators.alloc).size shouldBe 2
    val List(newA, newB) = cpg.method("foo").call(Operators.alloc).l
    newA.name shouldBe Operators.alloc
    newA.code shouldBe "new A"
    newB.name shouldBe Operators.alloc
    newB.code shouldBe "new B"
  }

  "all identifiers should (conservatively) point to their allocation site" in {
    val List(newA, newB) = cpg.method("foo").call(Operators.alloc).l
    newA.in(EdgeTypes.POINTS_TO).asScala.collectAll[AstNode].code.dedup.l shouldBe List("$stack4", "p", "q")
    newB.in(EdgeTypes.POINTS_TO).asScala.collectAll[AstNode].code.dedup.l shouldBe List("$stack5", "r")
  }

  "all identifiers should (conservatively) point to their allocation site for arrays" in {
    val List(newA, newB) = cpg.method("baz").call.name(Operators.alloc, Operators.arrayInitializer).l
    newA.in(EdgeTypes.POINTS_TO).asScala.collectAll[AstNode].code.dedup.l shouldBe List("$stack3", "p", "q")
    newB.in(EdgeTypes.POINTS_TO).asScala.collectAll[AstNode].code.dedup.l shouldBe List("r")
  }

}
