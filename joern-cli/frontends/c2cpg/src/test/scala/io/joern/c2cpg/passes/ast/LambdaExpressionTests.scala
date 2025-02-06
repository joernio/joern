package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class LambdaExpressionTests extends AstC2CpgSuite {

  "be correct for simple lambda expressions" in {
    val cpg = code(
      """
        |auto x = [] (int a, int b) -> int
        | { return a + b; };
        |auto y = [] (string a, string b) -> string
        | { return a + b; };
        |""".stripMargin,
      "test.cpp"
    )
    val lambda1FullName = "<lambda>0:int(int,int)"
    val lambda2FullName = "<lambda>1:string(string,string)"

    cpg.local.nameExact("x").order.l shouldBe List(1)
    cpg.local.nameExact("y").order.l shouldBe List(3)

    inside(cpg.assignment.l) { case List(assignment1, assignment2) =>
      assignment1.order shouldBe 2
      inside(assignment1.astMinusRoot.isMethodRef.l) { case List(ref) =>
        ref.methodFullName shouldBe lambda1FullName
      }
      assignment2.order shouldBe 4
      inside(assignment2.astMinusRoot.isMethodRef.l) { case List(ref) =>
        ref.methodFullName shouldBe lambda2FullName
      }
    }

    inside(cpg.method.fullNameExact(lambda1FullName).isLambda.l) { case List(l1) =>
      l1.name shouldBe "<lambda>0"
      l1.code should startWith("[] (int a, int b) -> int")
      l1.signature shouldBe "int(int,int)"
      l1.body.code shouldBe "{ return a + b; }"
    }

    inside(cpg.method.fullNameExact(lambda2FullName).isLambda.l) { case List(l2) =>
      l2.name shouldBe "<lambda>1"
      l2.code should startWith("[] (string a, string b) -> string")
      l2.signature shouldBe "string(string,string)"
      l2.body.code shouldBe "{ return a + b; }"
    }

    inside(cpg.typeDecl(NamespaceTraversal.globalNamespaceName).head.bindsOut.l) {
      case List(bX: Binding, bY: Binding) =>
        bX.name shouldBe "<lambda>0"
        bX.signature shouldBe "int(int,int)"
        inside(bX.refOut.l) { case List(method: Method) =>
          method.name shouldBe "<lambda>0"
          method.fullName shouldBe lambda1FullName
          method.signature shouldBe "int(int,int)"
        }
        bY.name shouldBe "<lambda>1"
        bY.signature shouldBe "string(string,string)"
        inside(bY.refOut.l) { case List(method: Method) =>
          method.name shouldBe "<lambda>1"
          method.fullName shouldBe lambda2FullName
          method.signature shouldBe "string(string,string)"
        }
    }
  }

  "be correct for simple lambda expression in class" in {
    val cpg = code(
      """
        |class Foo {
        | auto x = [] (int a, int b) -> int
        | {
        |   return a + b;
        | };
        |};
        |
        |""".stripMargin,
      "test.cpp"
    )
    val lambdaName     = "<lambda>0"
    val signature      = "int(int,int)"
    val lambdaFullName = s"Foo.$lambdaName:$signature"

    cpg.member.nameExact("x").order.l shouldBe List(1)

    inside(cpg.assignment.l) { case List(assignment1) =>
      inside(assignment1.astMinusRoot.isMethodRef.l) { case List(ref) =>
        ref.methodFullName shouldBe lambdaFullName
      }
    }

    inside(cpg.method.fullNameExact(lambdaFullName).isLambda.l) { case List(l1) =>
      l1.name shouldBe lambdaName
      l1.code should startWith("[] (int a, int b) -> int")
      l1.signature shouldBe signature
    }

    inside(cpg.typeDecl("Foo").head.bindsOut.l) { case List(binding: Binding) =>
      binding.name shouldBe lambdaName
      binding.signature shouldBe signature
      inside(binding.refOut.l) { case List(method: Method) =>
        method.name shouldBe lambdaName
        method.fullName shouldBe lambdaFullName
        method.signature shouldBe signature
      }
    }
  }

  "be correct for simple lambda expression in class under namespaces" in {
    val cpg = code(
      """
        |namespace A { class B {
        |class Foo {
        | auto x = [] (int a, int b) -> int
        | {
        |   return a + b;
        | };
        |};
        |};}
        |""".stripMargin,
      "test.cpp"
    )
    val lambdaName     = "<lambda>0"
    val signature      = "int(int,int)"
    val lambdaFullName = s"A.B.Foo.$lambdaName:$signature"

    cpg.member.nameExact("x").order.l shouldBe List(1)

    inside(cpg.assignment.l) { case List(assignment1) =>
      inside(assignment1.astMinusRoot.isMethodRef.l) { case List(ref) =>
        ref.methodFullName shouldBe lambdaFullName
      }
    }

    inside(cpg.method.fullNameExact(lambdaFullName).isLambda.l) { case List(l1) =>
      l1.name shouldBe lambdaName
      l1.code should startWith("[] (int a, int b) -> int")
      l1.signature shouldBe signature
    }

    inside(cpg.typeDecl.fullNameExact("A.B.Foo").head.bindsOut.l) { case List(binding: Binding) =>
      binding.name shouldBe lambdaName
      binding.signature shouldBe signature
      inside(binding.refOut.l) { case List(method: Method) =>
        method.name shouldBe lambdaName
        method.fullName shouldBe lambdaFullName
        method.signature shouldBe signature
      }
    }
  }

  "be correct when calling a lambda" in {
    val cpg = code(
      """
        |auto x = [](int n) -> int
        |{
        |  return 32 + n;
        |};
        |
        |constexpr int foo1 = x(10);
        |constexpr int foo2 = [](int n) -> int
        |{
        |  return 32 + n;
        |}(10);
        |""".stripMargin,
      "test.cpp"
    )
    val signature       = "int(int)"
    val lambda1Name     = "<lambda>0"
    val lambda1FullName = s"$lambda1Name:$signature"
    val lambda2Name     = "<lambda>1"
    val lambda2FullName = s"$lambda2Name:$signature"

    cpg.local.nameExact("x").order.l shouldBe List(1)
    cpg.local.nameExact("foo1").order.l shouldBe List(3)
    cpg.local.nameExact("foo2").order.l shouldBe List(5)

    inside(cpg.assignment.l) { case List(assignment1, assignment2, assignment3) =>
      assignment1.order shouldBe 2
      assignment2.order shouldBe 4
      assignment3.order shouldBe 6
      inside(assignment1.astMinusRoot.isMethodRef.l) { case List(ref) =>
        ref.methodFullName shouldBe lambda1FullName
      }
    }

    inside(cpg.method.fullNameExact(lambda1FullName).isLambda.l) { case List(l1) =>
      l1.name shouldBe lambda1Name
      l1.code should startWith("[](int n) -> int")
      l1.signature shouldBe signature
    }

    inside(cpg.typeDecl(NamespaceTraversal.globalNamespaceName).head.bindsOut.l) {
      case List(b1: Binding, b2: Binding) =>
        b1.name shouldBe lambda1Name
        b1.signature shouldBe signature
        inside(b1.refOut.l) { case List(method: Method) =>
          method.name shouldBe lambda1Name
          method.fullName shouldBe lambda1FullName
          method.signature shouldBe signature
        }
        b2.name shouldBe lambda2Name
        b2.signature shouldBe signature
        inside(b2.refOut.l) { case List(method: Method) =>
          method.name shouldBe lambda2Name
          method.fullName shouldBe lambda2FullName
          method.signature shouldBe signature
        }
    }

    inside(cpg.call.nameExact("<operator>()").l) { case List(lambda1call, lambda2call) =>
      lambda1call.name shouldBe "<operator>()"
      lambda1call.methodFullName shouldBe "<operator>():int(int)"
      lambda1call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      inside(lambda1call.astChildren.l) { case List(id: Identifier, lit: Literal) =>
        id.code shouldBe "x"
        lit.code shouldBe "10"
      }
      inside(lambda1call.argument.l) { case List(lit: Literal) =>
        lit.code shouldBe "10"
      }
      inside(lambda1call.receiver.l) { case List(receiver: Identifier) =>
        receiver.code shouldBe "x"
      }

      lambda2call.name shouldBe "<operator>()"
      lambda2call.methodFullName shouldBe "<operator>():int(int)"
      lambda2call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      inside(lambda2call.astChildren.l) { case List(ref: MethodRef, lit: Literal) =>
        ref.methodFullName shouldBe lambda2FullName
        ref.code should startWith("[](int n) -> int")
        lit.code shouldBe "10"
      }

      inside(lambda2call.argument.l) { case List(lit: Literal) =>
        lit.code shouldBe "10"
      }
      inside(lambda2call.receiver.l) { case List(ref: MethodRef) =>
        ref.methodFullName shouldBe lambda2FullName
        ref.code should startWith("[](int n) -> int")
      }
    }
  }

}
