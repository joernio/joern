package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.{JimpleCodeToCpgFixture, JimpleDataflowFixture}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._

class PointsToTests extends JimpleDataflowFixture {

//  implicit val resolver: ICallResolver = NoResolve

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

  it should "foo" in {
    cpg.method("foo").call("<operator>.alloc").size shouldBe 2
    val List(newA, newB) = cpg.method("foo").call("<operator>.alloc").l
    newA.name shouldBe "<operator>.alloc"
    newA.code shouldBe "new A"
    newB.name shouldBe "<operator>.alloc"
    newB.code shouldBe "new B"

//    cpg.method("foo").call("<operator>.alloc").foreach { n =>
//      println((n.label, n.code))
//    }
//     println(cpg.method("foo").dotCfg.head)
  }


}
