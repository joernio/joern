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

    "ref the lambda param correctly" in {
      val List(lambdaMethod) = cpg.typeDecl.name("Foo").method.name(".*lambda.*").isLambda.l
      val List(param)        = lambdaMethod.parameter.l
      val List(reffedParam)  = cpg.identifier.nameExact("lambdaInput").refsTo.collectAll[MethodParameterIn].l
      reffedParam shouldBe param
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
          cpg.identifier.nameExact("fallback").refsTo.l shouldBe List(fallbackLocal)
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

  "lambdas with different return type annotations" should {
    val cpg = code("""
        |void foo() {
        |  auto l1 = [] () -> int { return 1; };  // explicit type
        |  auto l2 = [] () { return 1; }; // inferred
        |  auto l3 = [] () -> unknown { return bar(); }; // broken or unknown
        |  auto l4 = [] () mutable -> int { return 1; };
        |  auto l5 = [] () mutable { return 1; };
        |}
        |""".stripMargin)

    "have the correct fullname" in {
      val List(l0, l1, l2, l3, l4) = cpg.method.name(".*lambda.*").sortBy(_.name).l
      l0.fullName shouldBe "Test0.cpp:<global>.foo.<lambda>0:int()"
      l1.fullName shouldBe "Test0.cpp:<global>.foo.<lambda>1:ANY()" // CDT is unable to infer the type here; needs to be fixed
      l2.fullName shouldBe "Test0.cpp:<global>.foo.<lambda>2:unknown()"
      l3.fullName shouldBe "Test0.cpp:<global>.foo.<lambda>3:int()"
      l4.fullName shouldBe "Test0.cpp:<global>.foo.<lambda>4:ANY()"
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
      val List(lambda) = cpg.method.name(".*lambda.*").l
      lambda.fullName shouldBe "Test0.cpp:<global>.Foo.foo.<lambda>0:ANY()"
      cpg.all.collectAll[ClosureBinding].l match {
        case List(thisClosureBinding) =>
          val thisLocal = cpg.method.name(".*lambda.*").local.nameExact("this").head
          thisClosureBinding.closureBindingId shouldBe thisLocal.closureBindingId

          cpg.identifier.nameExact("this").refsTo.l shouldBe List(thisLocal)

          thisClosureBinding._refOut.l match {
            case List(capturedThisParam: MethodParameterIn) =>
              capturedThisParam.name shouldBe "this"
              capturedThisParam.typeFullName shouldBe "Foo*"
              capturedThisParam.method.fullName shouldBe "Foo.foo:void()"
            case result => fail(s"Expected single capturedParam but got $result")
          }

          thisClosureBinding._captureIn.l match {
            case List(outMethod: MethodRef) =>
              outMethod.typeFullName shouldBe lambda.fullName
              outMethod.methodFullName shouldBe lambda.fullName
            case result => fail(s"Expected single METHOD_REF but got $result")
          }
        case result => fail(s"Expected 1 closure binding for captured variables but got $result")
      }
    }
  }

  "lambdas capturing with shadowing" should {
    val cpg = code("""
     |static void foo(int *x) {
     |  auto f = [=] { float *x = nullptr; };
     |}""".stripMargin)

    "ref the shadowed variable correctly" in {
      cpg.all.collectAll[ClosureBinding] shouldBe empty
      val List(lambda) = cpg.method.name(".*lambda.*").l
      val List(xLocal) = lambda.block.local.nameExact("x").l
      xLocal.typeFullName shouldBe "float*"
      xLocal.closureBindingId shouldBe None
      xLocal.closureBinding shouldBe empty
      cpg.identifier.nameExact("x").refsTo.l shouldBe List(xLocal)
    }
  }

  "lambdas capturing with shadowing in nested lambdas" should {
    val cpg = code("""
        |static void foo(int *x) {
        |  auto f = [=] {
        |    x = nullptr; // x is captured
        |    auto nested = [=] {
        |      float *x = nullptr; // this x here is shadowed
        |    };
        |  };
        |}""".stripMargin)

    "ref the shadowed variable correctly" in {
      val List(lambdaF, lambdaNested) = cpg.method.name(".*lambda.*").sortBy(_.lineNumber.get).l
      cpg.all.collectAll[ClosureBinding].l match {
        case List(xClosureBinding) =>
          val List(xLocalCaptured) = lambdaF.block.local.nameExact("x").l
          xClosureBinding.closureBindingId shouldBe xLocalCaptured.closureBindingId

          cpg.identifier.nameExact("x").lineNumber(4).refsTo.l shouldBe List(xLocalCaptured)

          xClosureBinding._refOut.l match {
            case List(capturedThisParam: MethodParameterIn) =>
              capturedThisParam.name shouldBe "x"
              capturedThisParam.typeFullName shouldBe "int*"
              capturedThisParam.method.fullName shouldBe "foo:void(int*)"
            case result => fail(s"Expected single capturedParam but got $result")
          }

          xClosureBinding._captureIn.l match {
            case List(outMethod: MethodRef) =>
              outMethod.typeFullName shouldBe lambdaF.fullName
              outMethod.methodFullName shouldBe lambdaF.fullName
            case result => fail(s"Expected single METHOD_REF but got $result")
          }
        case result => fail(s"Expected 1 closure binding for captured variables but got $result")
      }

      val List(xLocalNested) = lambdaNested.block.local.nameExact("x").l
      xLocalNested.typeFullName shouldBe "float*"
      xLocalNested.closureBindingId shouldBe None
      xLocalNested.closureBinding shouldBe empty
      cpg.identifier.nameExact("x").lineNumber(6).refsTo.l shouldBe List(xLocalNested)
    }
  }

  "lambdas capturing with shadowing in nested blocks" should {
    val cpg = code("""
        |static void foo(int *x) {
        |  auto f = [&] {
        |    *x = 0; // capture
        |    {
        |      float *x = nullptr; // first shadowing
        |      {
        |        double *x = nullptr; // second shadowing
        |      }
        |    }
        |  };
        |}
        |""".stripMargin)

    "ref the shadowed variable correctly" in {
      val List(x1, x2, x3) = cpg.method.name(".*lambda.*").ast.isLocal.sortBy(_.lineNumber.get).nameExact("x").l

      x1.typeFullName shouldBe "int*"
      x1.closureBindingId shouldBe Some("Test0.cpp:<lambda>0:x")
      cpg.identifier.nameExact("x").lineNumber(4).refsTo.l shouldBe List(x1)

      x2.typeFullName shouldBe "float*"
      x2.closureBindingId shouldBe None
      cpg.identifier.nameExact("x").lineNumber(6).refsTo.l shouldBe List(x2)

      x3.typeFullName shouldBe "double*"
      x3.closureBindingId shouldBe None
      cpg.identifier.nameExact("x").lineNumber(8).refsTo.l shouldBe List(x3)
    }
  }

  "lambdas capturing with shadowing in nested blocks 2" should {
    val cpg = code("""
        |static void foo(int *x) {
        |  auto f = [&] {
        |    *x = 0; // capture
        |    {
        |      float *x = nullptr; // shadowing
        |      {
        |        *x = 0;
        |      }
        |    }
        |  };
        |}
        |""".stripMargin)

    "ref the shadowed variable correctly" in {
      val List(x1, x2) = cpg.method.name(".*lambda.*").ast.isLocal.sortBy(_.lineNumber.get).nameExact("x").l

      x1.typeFullName shouldBe "int*"
      x1.closureBindingId shouldBe Some("Test0.cpp:<lambda>0:x")
      cpg.identifier.nameExact("x").lineNumber(4).refsTo.l shouldBe List(x1)

      x2.typeFullName shouldBe "float*"
      x2.closureBindingId shouldBe None
      cpg.identifier.nameExact("x").lineNumber(6).refsTo.l shouldBe List(x2)

      cpg.identifier.nameExact("x").lineNumber(8).refsTo.l shouldBe List(x2)
    }
  }

  "lambdas capturing with shadowing in nested blocks 3" should {
    val cpg = code("""
        |static void foo() {
        |  int *x;
        |  auto f = [&] {
        |    *x = 0; // capture
        |    {
        |      float *x = nullptr; // first shadowing
        |      {
        |        double *x = nullptr; // second shadowing
        |      }
        |    }
        |  };
        |}
        |""".stripMargin)

    "ref the shadowed variable correctly" in {
      val List(x1, x2, x3) = cpg.method.name(".*lambda.*").ast.isLocal.sortBy(_.lineNumber.get).nameExact("x").l

      x1.typeFullName shouldBe "int*"
      x1.closureBindingId shouldBe Some("Test0.cpp:<lambda>0:x")
      cpg.identifier.nameExact("x").lineNumber(5).refsTo.l shouldBe List(x1)

      x2.typeFullName shouldBe "float*"
      x2.closureBindingId shouldBe None
      cpg.identifier.nameExact("x").lineNumber(7).refsTo.l shouldBe List(x2)

      x3.typeFullName shouldBe "double*"
      x3.closureBindingId shouldBe None
      cpg.identifier.nameExact("x").lineNumber(9).refsTo.l shouldBe List(x3)
    }
  }

  "lambdas capturing with shadowing in nested blocks 4" should {
    val cpg = code("""
        |static void foo(int *x) {
        |  {
        |    float *x = nullptr; // first shadowing
        |    {
        |      double *x = nullptr; // second shadowing
        |      auto f = [&] {
        |        *x = 0.0L; // capture of double*
        |      };
        |    }
        |  }
        |}
        |""".stripMargin)

    "ref the shadowed variable correctly" in {
      val List(x1, x2, x3) = cpg.local.sortBy(_.lineNumber.get).nameExact("x").l

      x1.typeFullName shouldBe "float*"
      x1.closureBindingId shouldBe None
      cpg.identifier.nameExact("x").lineNumber(4).refsTo.l shouldBe List(x1)

      x2.typeFullName shouldBe "double*"
      x2.closureBindingId shouldBe None
      cpg.identifier.nameExact("x").lineNumber(6).refsTo.l shouldBe List(x2)

      x3.typeFullName shouldBe "double*"
      x3.closureBindingId shouldBe Some("Test0.cpp:<lambda>0:x")
      cpg.identifier.nameExact("x").lineNumber(8).refsTo.l shouldBe List(x3)
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
      val List(lambda) = cpg.method.name(".*lambda.*").l
      cpg.all.collectAll[ClosureBinding].l match {
        case List(thisClosureBinding) =>
          val thisLocal = cpg.method.name(".*lambda.*").local.nameExact("this").head
          thisClosureBinding.closureBindingId shouldBe thisLocal.closureBindingId

          cpg.identifier.nameExact("this").refsTo.l shouldBe List(thisLocal)

          thisClosureBinding._refOut.l match {
            case List(capturedThisParam: MethodParameterIn) =>
              capturedThisParam.name shouldBe "this"
              capturedThisParam.typeFullName shouldBe "Foo*"
              capturedThisParam.method.fullName shouldBe "Foo.foo:void()"
            case result => fail(s"Expected single capturedParam but got $result")
          }

          thisClosureBinding._captureIn.l match {
            case List(outMethod: MethodRef) =>
              outMethod.typeFullName shouldBe lambda.fullName
              outMethod.methodFullName shouldBe lambda.fullName
            case result => fail(s"Expected single METHOD_REF but got $result")
          }
        case result => fail(s"Expected 1 closure binding for captured variables but got $result")
      }
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
