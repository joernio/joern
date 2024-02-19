package io.joern.csharpsrc2cpg.querying

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*

class CallGraphTests extends CSharpCode2CpgFixture {

  "calls to methods within the same file" should {

    val cpg = code("""
        |namespace Foo;
        |
        |public class Bar
        |{
        |
        | static void Main(string[] args)
        | {
        |   var bar = new Bar();
        |   int test1 = bar.A(2);
        |   int test2 = Bar.B(2);
        |   int test3 = B(2);
        | }
        | 
        | void Run() {
        |   int test4 = A(2);
        | }
        |
        | int A(int x)
        | {
        |   return x;
        | }
        |
        | static int B(int x)
        | {
        |   return x;
        | }
        |}
        |""".stripMargin)

    "correctly resolve a dynamic dispatch to method `A` with an explicit receiver" in {
      inside(cpg.call.nameExact("A").where(_.method.nameExact("Main")).l) {
        case call :: Nil =>
          call.methodFullName shouldBe "Foo.Bar.A:System.Int32(System.Int32)"
          call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
          call.callee.fullName.headOption shouldBe Option("Foo.Bar.A:System.Int32(System.Int32)")
        case xs => fail(s"Expected one `A` call, got [${xs.code.mkString(",")}]")
      }
    }

    "correctly resolve a static dispatch to method `B` with an explicit receiver" in {
      inside(cpg.call.nameExact("B").where(_.method.nameExact("Main")).sortBy(_.lineNumber).l) {
        case explicitB :: _ :: Nil =>
          explicitB.methodFullName shouldBe "Foo.Bar.B:System.Int32(System.Int32)"
          explicitB.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          explicitB.callee.fullName.headOption shouldBe Option("Foo.Bar.B:System.Int32(System.Int32)")
        case xs => fail(s"Expected two `B` calls, got [${xs.code.mkString(",")}]")
      }
    }

    "correctly resolve a static dispatch to method `B` with an implicit receiver" in {
      inside(cpg.call.nameExact("B").where(_.method.nameExact("Main")).sortBy(_.lineNumber).l) {
        case _ :: implicitB :: Nil =>
          implicitB.methodFullName shouldBe "Foo.Bar.B:System.Int32(System.Int32)"
          implicitB.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          implicitB.callee.fullName.headOption shouldBe Option("Foo.Bar.B:System.Int32(System.Int32)")
        case xs => fail(s"Expected two `B` calls, got [${xs.code.mkString(",")}]")
      }
    }

    "correctly resolve a dynamic dispatch to method `A` with an implicit receiver" in {
      inside(cpg.call.nameExact("A").where(_.method.nameExact("Run")).l) {
        case call :: Nil =>
          call.methodFullName shouldBe "Foo.Bar.A:System.Int32(System.Int32)"
          call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
          call.callee.fullName.headOption shouldBe Option("Foo.Bar.A:System.Int32(System.Int32)")
        case xs => fail(s"Expected one `A` call, got [${xs.code.mkString(",")}]")
      }
    }

  }

}
