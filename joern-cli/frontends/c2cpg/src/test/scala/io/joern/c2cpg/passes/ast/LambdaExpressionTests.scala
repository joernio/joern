package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class LambdaExpressionTests extends AstC2CpgSuite(FileDefaults.CppExt) {

  "a simple lambda expression as argument" should {
    val cpg = code("""
        |class Foo {
        |  public:
        |    string getFromSupplier(string input, std::function<string(string)>& mapper) {
        |      return mapper.apply(input);
        |    }
        |
        |    void foo(string input, string fallback) {
        |      getFromSupplier(
        |        input,
        |        [fallback] (string lambdaInput) -> string { return lambdaInput.length() > 5 ? "Long" : fallback; }
        |      );
        |    }
        |}
        |""".stripMargin)

    "create the correct typedecl node for the lambda" in {
      cpg.typeDecl.name(".*lambda.*").name.l shouldBe List("<lambda>0")
      cpg.typeDecl.name(".*lambda.*").fullName.l shouldBe List("Test0.cpp:<global>.Foo.foo.<lambda>0:string(string)")
    }

    "create a method node for the lambda" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").isLambda.l match {
        case List(lambdaMethod) =>
          lambdaMethod.name shouldBe "<lambda>0"
          lambdaMethod.fullName shouldBe "Test0.cpp:<global>.Foo.foo.<lambda>0:string(string)"
          lambdaMethod.parameter.l match {
            case List(lambdaInput) =>
              lambdaInput.name shouldBe "lambdaInput"
              lambdaInput.typeFullName shouldBe "string"
            case result => fail(s"Expected single lambda parameter but got $result")
          }
          lambdaMethod.methodReturn.typeFullName shouldBe "string"
        case result => fail(s"Expected single lambda method but got $result")
      }
    }

    "create a binding for the lambda method" in {
      val List(typeDecl) = cpg.typeDecl.fullNameExact("Test0.cpp:<global>.Foo.foo.<lambda>0:string(string)").l
      val List(binding)  = typeDecl.bindsOut.l
      binding.name shouldBe Defines.OperatorCall
      binding.methodFullName shouldBe "Test0.cpp:<global>.Foo.foo.<lambda>0:string(string)"
      binding.signature shouldBe "string(string)"
      val List(methodReffed) = binding.refOut.l
      methodReffed.name shouldBe "<lambda>0"
      methodReffed.fullName shouldBe "Test0.cpp:<global>.Foo.foo.<lambda>0:string(string)"
      methodReffed.signature shouldBe "string(string)"
    }

    "create a method body for the lambda method" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").block.astChildren.l match {
        case List(fallBack: Local, returnNode: Return) =>
          returnNode.code shouldBe "return lambdaInput.length() > 5 ? \"Long\" : fallback;"
          returnNode.astChildren.l match {
            case List(expr: Call) =>
              expr.methodFullName shouldBe Operators.conditional
            case result => fail(s"Expected return conditional, but got $result")
          }
          fallBack.name shouldBe "fallback"
        case result => fail(s"Expected lambda body with single return but got $result")
      }
    }

    "create locals for captured identifiers in the lambda method" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").local.sortBy(_.name) match {
        case Seq(fallbackLocal: Local) =>
          fallbackLocal.name shouldBe "fallback"
          fallbackLocal.code shouldBe "fallback"
          fallbackLocal.typeFullName shouldBe "string"
        case result => fail(s"Expected single local for fallback but got $result")
      }
    }

    "create closure bindings for captured identifiers" in {
      cpg.all.collectAll[ClosureBinding].sortBy(_.closureOriginalName) match {
        case Seq(fallbackClosureBinding) =>
          val fallbackLocal = cpg.method.name(".*lambda.*").local.name("fallback").head
          fallbackClosureBinding.closureBindingId shouldBe fallbackLocal.closureBindingId

          fallbackClosureBinding._refOut.l match {
            case List(capturedParam: MethodParameterIn) =>
              capturedParam.name shouldBe "fallback"
              capturedParam.method.fullName shouldBe "Foo.foo:void(string,string)"
            case result => fail(s"Expected single capturedParam but got $result")
          }

          fallbackClosureBinding._captureIn.l match {
            case List(outMethod: MethodRef) =>
              outMethod.typeFullName shouldBe "Test0.cpp:<global>.Foo.foo.<lambda>0:string(string)"
              outMethod.methodFullName shouldBe "Test0.cpp:<global>.Foo.foo.<lambda>0:string(string)"
            case result => fail(s"Expected single METHOD_REF but got $result")
          }
        case result => fail(s"Expected 1 closure binding for captured variables but got $result")
      }
    }

    "create a typeDecl node inheriting from correct interface" in {
      cpg.typeDecl.name(".*lambda.*").l match {
        case List(lambdaDecl) =>
          lambdaDecl.name shouldBe "<lambda>0"
          lambdaDecl.fullName shouldBe "Test0.cpp:<global>.Foo.foo.<lambda>0:string(string)"
          lambdaDecl.inheritsFromTypeFullName should contain theSameElementsAs List("std.function")
        case result => fail(s"Expected a single typeDecl for the lambda but got $result")
      }
    }
  }

  "lambdas capturing this in method" should {
    val cpg = code("""
        |class Foo {
        |  public:
        |    int firstDirty;
        |    void foo() {
        |      bar(l, [this] { return this->firstDirty == nullptr; });
        |    }
        |}
        |""".stripMargin)

    "ref this correctly" in {
      val List(thisId) = cpg.identifier.nameExact("this").l
      val List(refsTo) = thisId.refsTo.collectAll[MethodParameterIn].l
      refsTo.typeFullName shouldBe "Foo*"
      refsTo.name shouldBe "this"
      refsTo.index shouldBe 0
      refsTo.order shouldBe 0
      refsTo.method.fullName shouldBe "Foo.foo:void()"
    }
  }

  "lambdas capturing this in global method" should {
    val cpg = code("""
        |class Foo {}
        |void Foo::foo() {
        |  bar(l, [this] { return this->firstDirty == nullptr; });
        |}
        |""".stripMargin)

    "ref this correctly" in {
      val List(thisId) = cpg.identifier.nameExact("this").l
      val List(refsTo) = thisId.refsTo.collectAll[MethodParameterIn].l
      refsTo.typeFullName shouldBe "Foo*"
      refsTo.name shouldBe "this"
      refsTo.index shouldBe 0
      refsTo.order shouldBe 0
      refsTo.method.fullName shouldBe "Foo.foo:void()"
    }
  }

  "lambda capturing local variable by value" should {
    val cpg = code("""
        |class Foo {
        |  public:
        |    void foo(Object arg) {
        |      string myValue = "abc";
        |      std::list<string> userPayload = {};
        |      auto userNamesList = userPayload.map([myValue] (string item) -> string {
        |        sink2(myValue);
        |        return item + myValue;
        |      });
        |      sink1(userNamesList);
        |      return;
        |    }
        |}
        |""".stripMargin)

    "be captured precisely" in {
      cpg.all.collectAll[ClosureBinding].l match {
        case myValue :: Nil =>
          myValue.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
          myValue.closureOriginalName.head shouldBe "myValue"
          myValue._localViaRefOut.get.name shouldBe "myValue"
          myValue._captureIn.collectFirst { case x: MethodRef =>
            x.methodFullName
          }.head shouldBe "Test0.cpp:<global>.Foo.foo.<lambda>0:string(string)"
        case result =>
          fail(s"Expected single closure binding to collect but got $result")
      }
    }

  }

  "lambda capturing local variable by reference" should {
    val cpg = code("""
        |class Foo {
        |  public:
        |    void foo(Object arg) {
        |      string myValue = "abc";
        |      std::list<string> userPayload = {};
        |      auto userNamesList = userPayload.map([&] (string item) -> string {
        |        sink2(myValue);
        |        return item + myValue;
        |      });
        |      sink1(userNamesList);
        |      return;
        |    }
        |}
        |""".stripMargin)

    "be captured precisely" in {
      cpg.all.collectAll[ClosureBinding].l match {
        case myValue :: Nil =>
          myValue.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
          myValue.closureOriginalName.head shouldBe "myValue"
          myValue._localViaRefOut.get.name shouldBe "myValue"
          myValue._captureIn.collectFirst { case x: MethodRef =>
            x.methodFullName
          }.head shouldBe "Test0.cpp:<global>.Foo.foo.<lambda>0:string(string)"
        case result =>
          fail(s"Expected single closure binding to collect but got $result")
      }
    }

  }

  "be correct for simple lambda expressions" in {
    val cpg = code("""
        |auto x = [] (int a, int b) -> int
        | { return a + b; };
        |auto y = [] (string a, string b) -> string
        | { return a + b; };
        |""".stripMargin)
    val lambda1FullName = "Test0.cpp:<global>.<lambda>0:int(int,int)"
    val lambda2FullName = "Test0.cpp:<global>.<lambda>1:string(string,string)"

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
    cpg.typeDecl(NamespaceTraversal.globalNamespaceName).head.bindsOut.size shouldBe 0
  }

  "be correct for simple lambda expression in class" in {
    val cpg = code("""
        |class Foo {
        | auto x = [] (int a, int b) -> int
        | {
        |   return a + b;
        | };
        |};
        |
        |""".stripMargin)
    val lambdaName     = "<lambda>0"
    val signature      = "int(int,int)"
    val lambdaFullName = s"Test0.cpp:<global>.Foo.$lambdaName:$signature"

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
  }

  "be correct for simple lambda expression in class under namespaces" in {
    val cpg = code("""
        |namespace A { class B {
        |class Foo {
        | auto x = [] (int a, int b) -> int
        | {
        |   return a + b;
        | };
        |};
        |};}
        |""".stripMargin)
    val lambdaName     = "<lambda>0"
    val signature      = "int(int,int)"
    val lambdaFullName = s"Test0.cpp:<global>.A.B.Foo.$lambdaName:$signature"

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

  }

  "be correct when calling a lambda" in {
    val cpg = code("""
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
        |""".stripMargin)
    val signature       = "int(int)"
    val lambda1Name     = "<lambda>0"
    val lambda1FullName = s"Test0.cpp:<global>.$lambda1Name:$signature"
    val lambda2Name     = "<lambda>1"
    val lambda2FullName = s"Test0.cpp:<global>.$lambda2Name:$signature"

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
        ref.code shouldBe lambda2FullName
        lit.code shouldBe "10"
      }

      inside(lambda2call.argument.l) { case List(lit: Literal) =>
        lit.code shouldBe "10"
      }
      inside(lambda2call.receiver.l) { case List(ref: MethodRef) =>
        ref.methodFullName shouldBe lambda2FullName
        ref.code shouldBe lambda2FullName
      }
    }
  }

}
