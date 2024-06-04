package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import overflowdb.traversal.toNodeTraversal

class AstCreationPassTests extends AstC2CpgSuite {

  "Method AST layout" should {

    "be correct for method signature" in {
      val cpg = code("""
       |char *foo() {};
       |char *hello();
       |""".stripMargin)
      inside(cpg.method("foo").l) { case List(foo) =>
        foo.signature shouldBe "char* foo ()"
      }
      inside(cpg.method("hello").l) { case List(hello) =>
        hello.signature shouldBe "char* hello ()"

      }
    }

    "be correct for packed args" in {
      val cpg = code(
        """
       |void foo(int x, int*... args) {};
       |""".stripMargin,
        "test.cpp"
      )
      inside(cpg.method("foo").l) { case List(m) =>
        m.signature shouldBe "void foo (int,int*)"
        inside(m.parameter.l) { case List(x, args) =>
          x.name shouldBe "x"
          x.code shouldBe "int x"
          x.typeFullName shouldBe "int"
          x.isVariadic shouldBe false
          x.order shouldBe 1
          args.name shouldBe "args"
          args.code shouldBe "int*... args"
          args.typeFullName shouldBe "int*"
          args.isVariadic shouldBe true
          args.order shouldBe 2
        }
      }
    }

    "be correct for varargs" in {
      val cpg = code(
        """
       |void foo(int x, int args...) {};
       |""".stripMargin,
        "test.cpp"
      )
      inside(cpg.method("foo").l) { case List(m) =>
        inside(m.parameter.l) { case List(x, args) =>
          x.name shouldBe "x"
          x.code shouldBe "int x"
          x.typeFullName shouldBe "int"
          x.isVariadic shouldBe false
          x.order shouldBe 1
          args.name shouldBe "args"
          args.code shouldBe "int args..."
          args.typeFullName shouldBe "int"
          args.isVariadic shouldBe true
          args.order shouldBe 2
        }
      }
    }

    "be correct for knr function declarations" in {
      val cpg = code("""
        |int handler(x, y)
        | int *x;
        | int *y;
        | {};
        |""".stripMargin)
      inside(cpg.method("handler").l) { case List(m) =>
        inside(m.parameter.l) { case List(x, y) =>
          x.name shouldBe "x"
          x.code shouldBe "int *x;"
          x.typeFullName shouldBe "int*"
          x.order shouldBe 1
          y.name shouldBe "y"
          y.code shouldBe "int *y;"
          y.typeFullName shouldBe "int*"
          y.order shouldBe 2
        }
      }
    }

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
      val lambda1FullName = "<lambda>0"
      val lambda2FullName = "<lambda>1"

      cpg.local.name("x").order.l shouldBe List(1)
      cpg.local.name("y").order.l shouldBe List(3)

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
        l1.name shouldBe lambda1FullName
        l1.code should startWith("[] (int a, int b) -> int")
        l1.signature shouldBe s"int $lambda1FullName (int,int)"
        l1.body.code shouldBe "{ return a + b; }"
      }

      inside(cpg.method.fullNameExact(lambda2FullName).isLambda.l) { case List(l2) =>
        l2.name shouldBe lambda2FullName
        l2.code should startWith("[] (string a, string b) -> string")
        l2.signature shouldBe s"string $lambda2FullName (string,string)"
        l2.body.code shouldBe "{ return a + b; }"
      }

      inside(cpg.typeDecl(NamespaceTraversal.globalNamespaceName).head.bindsOut.l) {
        case List(bX: Binding, bY: Binding) =>
          bX.name shouldBe lambda1FullName
          bX.signature shouldBe s"int $lambda1FullName (int,int)"
          inside(bX.refOut.l) { case List(method: Method) =>
            method.name shouldBe lambda1FullName
            method.fullName shouldBe lambda1FullName
            method.signature shouldBe s"int $lambda1FullName (int,int)"
          }
          bY.name shouldBe lambda2FullName
          bY.signature shouldBe s"string $lambda2FullName (string,string)"
          inside(bY.refOut.l) { case List(method: Method) =>
            method.name shouldBe lambda2FullName
            method.fullName shouldBe lambda2FullName
            method.signature shouldBe s"string $lambda2FullName (string,string)"
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
      val lambdaFullName = s"Foo.$lambdaName"
      val signature      = s"int $lambdaFullName (int,int)"

      cpg.member.name("x").order.l shouldBe List(1)

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
      val lambdaFullName = s"A.B.Foo.$lambdaName"
      val signature      = s"int $lambdaFullName (int,int)"

      cpg.member.name("x").order.l shouldBe List(1)

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
      val lambda1Name = "<lambda>0"
      val signature1  = s"int $lambda1Name (int)"
      val lambda2Name = "<lambda>1"
      val signature2  = s"int $lambda2Name (int)"

      cpg.local.name("x").order.l shouldBe List(1)
      cpg.local.name("foo1").order.l shouldBe List(3)
      cpg.local.name("foo2").order.l shouldBe List(5)

      inside(cpg.assignment.l) { case List(assignment1, assignment2, assignment3) =>
        assignment1.order shouldBe 2
        assignment2.order shouldBe 4
        assignment3.order shouldBe 6
        inside(assignment1.astMinusRoot.isMethodRef.l) { case List(ref) =>
          ref.methodFullName shouldBe lambda1Name
        }
      }

      inside(cpg.method.fullNameExact(lambda1Name).isLambda.l) { case List(l1) =>
        l1.name shouldBe lambda1Name
        l1.code should startWith("[](int n) -> int")
        l1.signature shouldBe signature1
      }

      inside(cpg.typeDecl(NamespaceTraversal.globalNamespaceName).head.bindsOut.l) {
        case List(b1: Binding, b2: Binding) =>
          b1.name shouldBe lambda1Name
          b1.signature shouldBe signature1
          inside(b1.refOut.l) { case List(method: Method) =>
            method.name shouldBe lambda1Name
            method.fullName shouldBe lambda1Name
            method.signature shouldBe signature1
          }
          b2.name shouldBe lambda2Name
          b2.signature shouldBe signature2
          inside(b2.refOut.l) { case List(method: Method) =>
            method.name shouldBe lambda2Name
            method.fullName shouldBe lambda2Name
            method.signature shouldBe signature2
          }
      }

      inside(cpg.call("x").l) { case List(lambda1call) =>
        lambda1call.name shouldBe "x"
        lambda1call.methodFullName shouldBe "x"
        lambda1call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        inside(lambda1call.astChildren.l) { case List(lit: Literal) =>
          lit.code shouldBe "10"
        }
        inside(lambda1call.argument.l) { case List(lit: Literal) =>
          lit.code shouldBe "10"
        }
        lambda1call.receiver.l shouldBe empty
      }

      inside(cpg.call(lambda2Name).l) { case List(lambda2call) =>
        lambda2call.name shouldBe lambda2Name
        lambda2call.methodFullName shouldBe lambda2Name
        // TODO: lambda2call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
        inside(lambda2call.astChildren.l) { case List(ref: MethodRef, lit: Literal) =>
          ref.methodFullName shouldBe lambda2Name
          ref.code should startWith("[](int n) -> int")
          lit.code shouldBe "10"
        }

        inside(lambda2call.argument.l) { case List(ref: MethodRef, lit: Literal) =>
          ref.methodFullName shouldBe lambda2Name
          ref.code should startWith("[](int n) -> int")
          lit.code shouldBe "10"
        }
      }
    }

    "be correct for empty method" in {
      val cpg = code("void method(int x) { }")
      inside(cpg.method.name("method").astChildren.l) {
        case List(param: MethodParameterIn, _: Block, ret: MethodReturn) =>
          ret.typeFullName shouldBe "void"
          param.typeFullName shouldBe "int"
          param.name shouldBe "x"
      }
    }

    "be correct parameter in nodes as pointer" in {
      val cpg = code("""
        |void method(a_struct_type *a_struct) {
        |  void *x = NULL;
        |  a_struct->foo = x;
        |  free(x);
        |}
        |""".stripMargin)
      inside(cpg.method.name("method").parameter.l) { case List(param: MethodParameterIn) =>
        param.typeFullName shouldBe "a_struct_type*"
        param.name shouldBe "a_struct"
        param.code shouldBe "a_struct_type *a_struct"
      }
    }

    "be correct parameter in nodes as pointer with struct" in {
      val cpg = code("""
       |void method(struct date *date) {
       |  void *x = NULL;
       |  a_struct->foo = x;
       |  free(x);
       |}
       |""".stripMargin)
      inside(cpg.method.name("method").parameter.l) { case List(param: MethodParameterIn) =>
        param.code shouldBe "struct date *date"
        param.typeFullName shouldBe "date*"
        param.name shouldBe "date"
      }
    }

    "be correct parameter in nodes as array" in {
      val cpg = code("""
       |void method(int x[]) {
       |  void *x = NULL;
       |  a_struct->foo = x;
       |  free(x);
       |}
       |""".stripMargin)
      inside(cpg.method.name("method").parameter.l) { case List(param: MethodParameterIn) =>
        param.typeFullName shouldBe "int[]"
        param.name shouldBe "x"
      }
    }

    "be correct parameter in nodes as array ptr" in {
      val cpg = code("""
       |void method(int []) {
       |  void *x = NULL;
       |  a_struct->foo = x;
       |  free(x);
       |}
       |""".stripMargin)
      inside(cpg.method.name("method").parameter.l) { case List(param: MethodParameterIn) =>
        param.typeFullName shouldBe "int[]"
        param.name shouldBe ""
      }
    }

    "be correct parameter in nodes as struct array" in {
      val cpg = code("""
       |void method(a_struct_type a_struct[]) {
       |  void *x = NULL;
       |  a_struct->foo = x;
       |  free(x);
       |}
       |""".stripMargin)
      inside(cpg.method.name("method").parameter.l) { case List(param: MethodParameterIn) =>
        param.typeFullName shouldBe "a_struct_type[]"
        param.name shouldBe "a_struct"
      }
    }

    "be correct parameter in nodes as struct array with ptr" in {
      val cpg = code("""
      |void method(a_struct_type *a_struct[]) {
      |  void *x = NULL;
      |  a_struct->foo = x;
      |  free(x);
      |}
      |""".stripMargin)
      inside(cpg.method.name("method").parameter.l) { case List(param: MethodParameterIn) =>
        param.typeFullName shouldBe "a_struct_type[]*"
        param.name shouldBe "a_struct"
      }
    }

    "be correct for decl assignment" in {
      val cpg = code("""
        |void method() {
        |  int local = 1;
        |}
        |""".stripMargin)
      inside(cpg.method.name("method").block.astChildren.l) { case List(local: Local, call: Call) =>
        local.name shouldBe "local"
        local.typeFullName shouldBe "int"
        local.order shouldBe 1
        call.name shouldBe Operators.assignment
        call.order shouldBe 2
        inside(call.astChildren.l) { case List(identifier: Identifier, literal: Literal) =>
          identifier.name shouldBe "local"
          identifier.typeFullName shouldBe "int"
          identifier.order shouldBe 1
          identifier.argumentIndex shouldBe 1
          literal.code shouldBe "1"
          literal.typeFullName shouldBe "int"
          literal.order shouldBe 2
          literal.argumentIndex shouldBe 2
        }
      }
    }

    "be correct for decl assignment with typedecl" in {
      val cpg = code(
        """
       |void method() {
       |  int local = 1;
       |  constexpr bool is_std_array_v = decltype(local)::value;
       |}
       |""".stripMargin,
        "test.cpp"
      )
      inside(cpg.method.name("method").block.astChildren.l) { case List(_, call1: Call, _, call2: Call) =>
        call1.name shouldBe Operators.assignment
        inside(call2.astChildren.l) { case List(identifier: Identifier, call: Call) =>
          identifier.name shouldBe "is_std_array_v"
          identifier.typeFullName shouldBe "bool"
          identifier.order shouldBe 1
          identifier.argumentIndex shouldBe 1
          call.code shouldBe "decltype(local)::value"
          call.order shouldBe 2
          call.methodFullName shouldBe Operators.fieldAccess
          call.argument(2).code shouldBe "value"
          inside(call.argument(1)) { case fa: Call =>
            fa.code shouldBe "decltype(local)"
            fa.methodFullName shouldBe "<operator>.typeOf"
            fa.argument(1).code shouldBe "local"
          }
        }
      }
    }

    "be correct for decl assignment with identifier on the right" in {
      val cpg = code("""
          |void method(int x) {
          |  int local = x;
          |}""".stripMargin)
      cpg.local.name("local").order.l shouldBe List(1)
      inside(cpg.method("method").block.astChildren.assignment.source.l) { case List(identifier: Identifier) =>
        identifier.code shouldBe "x"
        identifier.typeFullName shouldBe "int"
        identifier.order shouldBe 2
        identifier.argumentIndex shouldBe 2
      }
    }

    "be correct for decl assignment of multiple locals" in {
      val cpg = code("""
          |void method(int x, int y) {
          |  int local = x, local2 = y;
          |}""".stripMargin)
      // Note that `cpg.method.local` does not work
      // because it depends on CONTAINS edges which
      // are created by a backend pass in semanticcpg
      // construction.
      inside(cpg.local.l.sortBy(_.order)) { case List(local1, local2) =>
        local1.name shouldBe "local"
        local1.typeFullName shouldBe "int"
        local1.order shouldBe 1
        local2.name shouldBe "local2"
        local2.typeFullName shouldBe "int"
        local2.order shouldBe 2
      }

      inside(cpg.assignment.l.sortBy(_.order)) { case List(a1, a2) =>
        a1.order shouldBe 3
        a2.order shouldBe 4
        List(a1.target.code, a1.source.code) shouldBe List("local", "x")
        List(a2.target.code, a2.source.code) shouldBe List("local2", "y")
      }
    }

    "be correct for nested expression" in {
      val cpg = code("""
        |void method() {
        |  int x;
        |  int y;
        |  int z;
        |
        |  x = y + z;
        |}
      """.stripMargin)
      val localX = cpg.local.order(1)
      localX.name.l shouldBe List("x")
      val localY = cpg.local.order(2)
      localY.name.l shouldBe List("y")
      val localZ = cpg.local.order(3)
      localZ.name.l shouldBe List("z")

      inside(cpg.method.name("method").ast.isCall.name(Operators.assignment).cast[OpNodes.Assignment].l) {
        case List(assignment) =>
          assignment.target.code shouldBe "x"
          assignment.source.start.isCall.name.l shouldBe List(Operators.addition)
          inside(assignment.source.astChildren.l) { case List(id1: Identifier, id2: Identifier) =>
            id1.order shouldBe 1
            id1.code shouldBe "y"
            id2.order shouldBe 2
            id2.code shouldBe "z"
          }
      }
    }

    "be correct for nested block" in {
      val cpg = code("""
        |void method() {
        |  int x;
        |  {
        |    int y;
        |  }
        |}
      """.stripMargin)
      inside(cpg.method.name("method").block.astChildren.l) { case List(local: Local, innerBlock: Block) =>
        local.name shouldBe "x"
        local.order shouldBe 1
        inside(innerBlock.astChildren.l) { case List(localInBlock: Local) =>
          localInBlock.name shouldBe "y"
          localInBlock.order shouldBe 1
        }
      }
    }

    "be correct for while-loop" in {
      val cpg = code("""
        |void method(int x) {
        |  while (x < 1) {
        |    x += 1;
        |  }
        |}
      """.stripMargin)
      inside(cpg.method.name("method").block.astChildren.isControlStructure.l) {
        case List(controlStruct: ControlStructure) =>
          controlStruct.code shouldBe "while (x < 1)"
          controlStruct.controlStructureType shouldBe ControlStructureTypes.WHILE
          inside(controlStruct.condition.l) { case List(cndNode) =>
            cndNode.code shouldBe "x < 1"
          }
          controlStruct.whenTrue.assignment.code.l shouldBe List("x += 1")
          controlStruct.lineNumber shouldBe Option(3)
          controlStruct.columnNumber shouldBe Option(3)
      }
    }

    "be correct for if" in {
      val cpg = code("""
        |void method(int x) {
        |  int y;
        |  if (x > 0) {
        |    y = 0;
        |  }
        |}
      """.stripMargin)
      inside(cpg.method.name("method").controlStructure.l) { case List(controlStruct: ControlStructure) =>
        controlStruct.code shouldBe "if (x > 0)"
        controlStruct.controlStructureType shouldBe ControlStructureTypes.IF
        inside(controlStruct.condition.l) { case List(cndNode) =>
          cndNode.code shouldBe "x > 0"

        }
        controlStruct.whenTrue.assignment.code.l shouldBe List("y = 0")
      }
    }

    "be correct for if-else" in {
      val cpg = code("""
        |void method(int x) {
        |  int y;
        |  if (x > 0) {
        |    y = 0;
        |  } else {
        |    y = 1;
        |  }
        |}
      """.stripMargin)
      inside(cpg.method.name("method").controlStructure.l) { case List(ifStmt, elseStmt) =>
        ifStmt.controlStructureType shouldBe ControlStructureTypes.IF
        ifStmt.code shouldBe "if (x > 0)"
        elseStmt.controlStructureType shouldBe ControlStructureTypes.ELSE
        elseStmt.code shouldBe "else"

        inside(ifStmt.condition.l) { case List(cndNode) =>
          cndNode.code shouldBe "x > 0"
        }

        ifStmt.whenTrue.assignment
          .map(x => (x.target.code, x.source.code))
          .headOption shouldBe Option(("y", "0"))
        ifStmt.whenFalse.assignment
          .map(x => (x.target.code, x.source.code))
          .headOption shouldBe Option(("y", "1"))
      }
    }

    "be correct for conditional expression in call" in {
      val cpg = code("""
         | void method() {
         |   int x = (true ? vlc_dccp_CreateFD : vlc_datagram_CreateFD)(fd);
         | }
      """.stripMargin)
      inside(cpg.method.name("method").ast.isCall.name(Operators.conditional).l) { case List(call) =>
        call.code shouldBe "true ? vlc_dccp_CreateFD : vlc_datagram_CreateFD"
      }
    }

    "be correct for conditional expression" in {
      val cpg = code("""
        | void method() {
        |   int x = (foo == 1) ? bar : 0;
        | }
      """.stripMargin)
      // Just like we cannot use `cpg.method.local`,
      // `cpg.method.call` will not work at this stage
      // either because there are no CONTAINS edges

      inside(cpg.method.name("method").ast.isCall.name(Operators.conditional).l) { case List(call) =>
        call.code shouldBe "(foo == 1) ? bar : 0"
        inside(call.argument.l) { case List(condition, trueBranch, falseBranch) =>
          condition.argumentIndex shouldBe 1
          condition.code shouldBe "foo == 1"
          trueBranch.argumentIndex shouldBe 2
          trueBranch.code shouldBe "bar"
          falseBranch.argumentIndex shouldBe 3
          falseBranch.code shouldBe "0"
        }
      }
    }

    "be correct for ranged for-loop" in {
      val cpg = code(
        """
       |void method() {
       |  for (int x : list) {
       |    int z = x;
       |  }
       |}""".stripMargin,
        "file.cpp"
      )
      inside(cpg.method.name("method").controlStructure.l) { case List(forStmt) =>
        forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
        inside(forStmt.astChildren.order(1).l) { case List(ident: Identifier) =>
          ident.code shouldBe "list"
        }
        inside(forStmt.astChildren.order(2).l) { case List(x: Local) =>
          x.name shouldBe "x"
          x.typeFullName shouldBe "int"
          x.code shouldBe "int x"
        }
        inside(forStmt.astChildren.order(3).l) { case List(block: Block) =>
          block.astChildren.isCall.code.l shouldBe List("z = x")
        }
      }
    }

    "be correct for ranged for-loop with structured binding" in {
      val cpg = code(
        """
        |void method() {
        |  int foo[2] = {1, 2};
        |  for(const auto& [a, b] : foo) {};
        |}
        |""".stripMargin,
        "test.cpp"
      )
      inside(cpg.method.name("method").controlStructure.l) { case List(forStmt) =>
        forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
        inside(forStmt.astChildren.order(1).l) { case List(ident) =>
          ident.code shouldBe "foo"
        }
        inside(forStmt.astChildren.order(2).astChildren.l) { case List(idA, idB) =>
          idA.code shouldBe "a"
          idB.code shouldBe "b"
        }
        inside(forStmt.astChildren.order(3).l) { case List(block) =>
          block.code shouldBe "<empty>"
          block.astChildren.l shouldBe empty
        }
      }
    }

    "be correct for for-loop with multiple initializations" in {
      val cpg = code("""
        |void method(int x, int y) {
        |  for ( x = 0, y = 0; x < 1; x += 1) {
        |    int z = 0;
        |  }
        |}
      """.stripMargin)
      inside(cpg.method.name("method").controlStructure.l) { case List(forStmt) =>
        forStmt.controlStructureType shouldBe ControlStructureTypes.FOR
        childContainsAssignments(forStmt, 1, List("x = 0", "y = 0"))

        inside(forStmt.astChildren.order(2).l) { case List(condition: Expression) =>
          condition.code shouldBe "x < 1"
        }

        forStmt.condition.l shouldBe forStmt.astChildren.order(2).l
        childContainsAssignments(forStmt, 3, List("x += 1"))
        childContainsAssignments(forStmt, 4, List("z = 0"))
      }
    }

    def childContainsAssignments(node: AstNode, i: Int, list: List[String]) = {
      inside(node.astChildren.order(i).l) { case List(child) =>
        child.assignment.code.l shouldBe list
      }
    }

    "be correct for unary expression '++'" in {
      val cpg = code("""
        |void method(int x) {
        |  ++x;
        |}
      """.stripMargin)
      cpg.method
        .name("method")
        .ast
        .isCall
        .name(Operators.preIncrement)
        .argument(1)
        .code
        .l shouldBe List("x")
    }

    "be correct for expression list" in {
      val cpg = code("""
        |void method(int x) {
        |  return (__sync_synchronize(), foo(x));
        |}
      """.stripMargin)
      val List(bracketedPrimaryCall) = cpg.call("<operator>.bracketedPrimary").l
      val List(expressionListCall)   = bracketedPrimaryCall.argument.isCall.l
      expressionListCall.name shouldBe "<operator>.expressionList"

      val List(arg1) = expressionListCall.argument(1).start.collectAll[Call].l
      arg1.code shouldBe "__sync_synchronize()"
      val List(arg2) = expressionListCall.argument(2).start.collectAll[Call].l
      arg2.code shouldBe "foo(x)"
    }

    "not create an expression list for comma operator" in {
      val cpg = code("""
        |int something(void);
        |void a() {
        |  int b;
        |  int c;
        |  for (; b = something(), b > c;) {}
        |}
      """.stripMargin)
      val List(forLoop)        = cpg.controlStructure.l
      val List(conditionBlock) = forLoop.condition.collectAll[Block].l
      conditionBlock.argumentIndex shouldBe 2
      val List(assignmentCall, greaterCall) = conditionBlock.astChildren.collectAll[Call].l
      assignmentCall.argumentIndex shouldBe 1
      assignmentCall.code shouldBe "b = something()"
      greaterCall.argumentIndex shouldBe 2
      greaterCall.code shouldBe "b > c"
    }

    "be correct for call expression" in {
      val cpg = code("""
        |void method(int x) {
        |  foo(x);
        |}
      """.stripMargin)
      cpg.method
        .name("method")
        .ast
        .isCall
        .name("foo")
        .argument(1)
        .code
        .l shouldBe List("x")
    }

    "be correct for call expression returning pointer" in {
      val cpg = code("""
        |int * foo(int arg);
        |int * method(int x) {
        |  foo(x);
        |}
      """.stripMargin)
      inside(cpg.method.name("method").ast.isCall.l) { case List(call: Call) =>
        call.code shouldBe "foo(x)"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        val rec = call.receiver.l
        rec.length shouldBe 0
        call.argument(1).code shouldBe "x"
      }
    }

    "be correct for field access" in {
      val cpg = code("""
        |void method(struct someUndefinedStruct x) {
        |  x.a;
        |}
      """.stripMargin)
      inside(cpg.method.name("method").ast.isCall.name(Operators.fieldAccess).l) { case List(call) =>
        val arg1 = call.argument(1)
        val arg2 = call.argument(2)
        arg1.isIdentifier shouldBe true
        arg1.argumentIndex shouldBe 1
        arg1.asInstanceOf[Identifier].name shouldBe "x"
        arg2.isFieldIdentifier shouldBe true
        arg2.argumentIndex shouldBe 2
        arg2.asInstanceOf[FieldIdentifier].code shouldBe "a"
        arg2.asInstanceOf[FieldIdentifier].canonicalName shouldBe "a"
      }
    }

    "be correct for indirect field access" in {
      val cpg = code("""
        |void method(struct someUndefinedStruct *x) {
        |  x->a;
        |}
      """.stripMargin)
      inside(cpg.method.name("method").ast.isCall.name(Operators.indirectFieldAccess).l) { case List(call) =>
        val arg1 = call.argument(1)
        val arg2 = call.argument(2)
        arg1.isIdentifier shouldBe true
        arg1.argumentIndex shouldBe 1
        arg1.asInstanceOf[Identifier].name shouldBe "x"
        arg2.isFieldIdentifier shouldBe true
        arg2.argumentIndex shouldBe 2
        arg2.asInstanceOf[FieldIdentifier].code shouldBe "a"
        arg2.asInstanceOf[FieldIdentifier].canonicalName shouldBe "a"
      }
    }

    "be correct for indirect field access in call" in {
      val cpg = code("""
          |void method(struct someUndefinedStruct *x) {
          |  return (x->a)(1, 2);
          |}
      """.stripMargin)
      inside(cpg.method.name("method").ast.isCall.name(Operators.indirectFieldAccess).l) { case List(call) =>
        val arg1 = call.argument(1)
        val arg2 = call.argument(2)
        arg1.isIdentifier shouldBe true
        arg1.argumentIndex shouldBe 1
        arg1.asInstanceOf[Identifier].name shouldBe "x"
        arg2.isFieldIdentifier shouldBe true
        arg2.argumentIndex shouldBe 2
        arg2.asInstanceOf[FieldIdentifier].code shouldBe "a"
        arg2.asInstanceOf[FieldIdentifier].canonicalName shouldBe "a"
      }
    }

    "be correct for indirection on call" in {
      val cpg = code("""
       |typedef long unsigned int (*hStrLenFunc)(const char *str);
       |int main() {
       |  hStrLenFunc strLenFunc = &strlen;
       |  return (*strLenFunc)("123");
       |}
      """.stripMargin)
      inside(cpg.method.name("main").ast.isCall.codeExact("(*strLenFunc)(\"123\")").l) { case List(call) =>
        call.name shouldBe "*strLenFunc"
        call.methodFullName shouldBe "*strLenFunc"
      }
    }

    "be correct for sizeof operator on identifier with brackets" in {
      val cpg = code("""
        |void method() {
        |  int a;
        |  sizeof(a);
        |}
      """.stripMargin)
      cpg.method
        .name("method")
        .ast
        .isCall
        .name(Operators.sizeOf)
        .argument(1)
        .isIdentifier
        .name("a")
        .argumentIndex(1)
        .size shouldBe 1
    }

    "be correct for sizeof operator on identifier without brackets" in {
      val cpg = code("""
        |void method() {
        |  int a;
        |  sizeof a ;
        |}
      """.stripMargin)
      cpg.method
        .name("method")
        .ast
        .isCall
        .name(Operators.sizeOf)
        .argument(1)
        .isIdentifier
        .name("a")
        .argumentIndex(1)
        .size shouldBe 1
    }

    "be correct for sizeof operator on type" in {
      val cpg = code(
        """
        |void method() {
        |  sizeof(int);
        |}""".stripMargin,
        "file.cpp"
      )
      cpg.method
        .name("method")
        .ast
        .isCall
        .name(Operators.sizeOf)
        .argument(1)
        .isIdentifier
        .name("int")
        .argumentIndex(1)
        .size shouldBe 1
    }
  }

  "Structural AST layout" should {

    "be correct for empty method" in {
      val cpg = code("""
       | void method() {
       | };
      """.stripMargin)
      cpg.method.name("method").size shouldBe 1
    }

    "be correct for empty named struct" in {
      val cpg = code("""
       | struct foo {
       | };
      """.stripMargin)
      cpg.typeDecl.name("foo").size shouldBe 1
    }

    "be correct for struct decl" in {
      val cpg = code("""
       | struct foo;
      """.stripMargin)
      cpg.typeDecl.name("foo").size shouldBe 1
    }

    "be correct for named struct with single field" in {
      val cpg = code("""
       | struct foo {
       |   int x;
       | };
      """.stripMargin)
      cpg.typeDecl
        .name("foo")
        .member
        .code("x")
        .name("x")
        .typeFullName("int")
        .size shouldBe 1
    }

    "be correct for named struct with multiple fields" in {
      val cpg = code("""
        | struct foo {
        |   int x;
        |   int y;
        |   int z;
        | };
      """.stripMargin)
      cpg.typeDecl.name("foo").member.code.toSetMutable shouldBe Set("x", "y", "z")
    }

    "be correct for named struct with nested struct" in {
      val cpg = code("""
        | struct foo {
        |   int x;
        |   struct bar {
        |     int y;
        |     struct foo2 {
        |       int z;
        |     };
        |   };
        | };
      """.stripMargin)
      inside(cpg.typeDecl.name("foo").l) { case List(fooStruct: TypeDecl) =>
        fooStruct.member.name("x").size shouldBe 1
        inside(fooStruct.astChildren.isTypeDecl.l) { case List(barStruct: TypeDecl) =>
          barStruct.member.name("y").size shouldBe 1
          inside(barStruct.astChildren.isTypeDecl.l) { case List(foo2Struct: TypeDecl) =>
            foo2Struct.member.name("z").size shouldBe 1
          }
        }
      }
    }

    "be correct for typedef struct" in {
      val cpg = code("""
        |typedef struct foo {
        |} abc;
      """.stripMargin)
      cpg.typeDecl.name("foo").aliasTypeFullName("abc").size shouldBe 1
    }

    "be correct for struct with local" in {
      val cpg = code("""
        |struct A {
        |  int x;
        |} a;
        |struct B b;
      """.stripMargin)
      inside(cpg.typeDecl("A").member.l) { case List(x) =>
        x.name shouldBe "x"
        x.typeFullName shouldBe "int"
      }
      cpg.typeDecl.name("B").size shouldBe 1
      inside(cpg.local.l) { case List(localA, localB) =>
        localA.name shouldBe "a"
        localA.typeFullName shouldBe "A"
        localA.code shouldBe "struct A a"
        localB.name shouldBe "b"
        localB.typeFullName shouldBe "B"
        localB.code shouldBe "struct B b"
      }
    }

    "be correct for global struct" in {
      val cpg = code("""
        |struct filesystem {
        |	void (*open)(int a);
        |};
        |
        |void my_open(int a) {
        |	int b;
        |	b = a;
        |	return;
        |}
        |
        |static const struct filesystem my_fs = {
        |	.open = &my_open,
        |};
        |
        |int main(int argc, char *argv[]) {
        |	static int i;
        |	static const struct filesystem my_other_fs = {
        |		 .open = &my_open,
        |	};
        |	struct filesystem real_fs;
        |	real_fs.open = &my_open;
        |	i = 0;
        |}
      """.stripMargin)
      val List(localMyOtherFs) = cpg.method("main").local.name("my_other_fs").l
      localMyOtherFs.order shouldBe 2
      localMyOtherFs.referencingIdentifiers.name.l shouldBe List("my_other_fs")
      val List(localMyFs) = cpg.local.name("my_fs").l
      localMyFs.order shouldBe 4
      localMyFs.referencingIdentifiers.name.l shouldBe List("my_fs")
      cpg.typeDecl.nameNot(NamespaceTraversal.globalNamespaceName).fullName.l.distinct shouldBe List("filesystem")
    }

    "be correct for typedef enum" in {
      val cpg = code("""
        |typedef enum foo {
        |} abc;
      """.stripMargin)
      cpg.typeDecl.name("foo").aliasTypeFullName("abc").size shouldBe 1
    }

    "be correct for classes with friends" in {
      val cpg = code(
        """
        |class Bar {};
        |class Foo {
        |  friend Bar;
        |};
      """.stripMargin,
        "test.cpp"
      )
      inside(cpg.typeDecl("Foo").astChildren.isTypeDecl.l) { case List(bar) =>
        bar.name shouldBe "Bar"
        bar.aliasTypeFullName shouldBe Option("Bar")
      }
    }

    "be correct for single inheritance" in {
      val cpg = code(
        """
        |class Base {public: int i;};
        |class Derived : public Base{
        |public:
        | char x;
        | int method(){return i;};
        |};
      """.stripMargin,
        "file.cpp"
      )
      cpg.typeDecl
        .name("Derived")
        .count(_.inheritsFromTypeFullName == List("Base")) shouldBe 1
    }

    "be correct for field access" in {
      val cpg = code(
        """
        |class Foo {
        |public:
        | char x;
        | int method(){return i;};
        |};
        |
        |Foo f;
        |int x = f.method();
      """.stripMargin,
        "file.cpp"
      )
      cpg.typeDecl
        .name("Foo")
        .l
        .size shouldBe 1

      inside(cpg.call.code("f.method()").l) { case List(call: Call) =>
        call.methodFullName shouldBe Operators.fieldAccess
        call.argument(1).code shouldBe "f"
        call.argument(2).code shouldBe "method"
      }
    }

    "be correct for type initializer expression" in {
      val cpg = code(
        """
        |int x = (int){ 1 };
      """.stripMargin,
        "file.cpp"
      )
      inside(cpg.call.name(Operators.cast).l) { case List(call: Call) =>
        call.argument(2).code shouldBe "{ 1 }"
        call.argument(1).code shouldBe "int"
      }
    }

    "be correct for static assert" in {
      val cpg = code(
        """
        |void foo(){
        | int a = 0;
        | static_assert ( a == 0 , "not 0!");
        |}
      """.stripMargin,
        "file.cpp"
      )
      inside(cpg.call.codeExact("static_assert ( a == 0 , \"not 0!\");").l) { case List(call: Call) =>
        call.name shouldBe "static_assert"
        call.argument(1).code shouldBe "a == 0"
        call.argument(2).code shouldBe "\"not 0!\""
      }
    }

    "be correct for try catch" in {
      val cpg = code(
        """
        |void bar();
        |int foo(){
        | try { bar(); } 
        | catch(Foo x) { return 0; };
        |}
      """.stripMargin,
        "file.cpp"
      )
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        val List(tryBlock) = t.astChildren.isBlock.l
        tryBlock.order shouldBe 1
        tryBlock.astChildren.isCall.order(1).code.l shouldBe List("bar()")
        val List(catchX) = t.astChildren.isControlStructure.isCatch.l
        catchX.order shouldBe 2
        catchX.ast.isReturn.code.l shouldBe List("return 0;")
        catchX.ast.isLocal.code.l shouldBe List("Foo x")
      }
    }

    "be correct for try with multiple catches" in {
      val cpg: Cpg = code(
        """
          |int main() {
          |  try {
          |    a;
          |  } catch (short x) {
          |    b;
          |  } catch (int y) {
          |    c;
          |  } catch (long z) {
          |    d;
          |  }
          |}
          |""".stripMargin,
        "file.cpp"
      )
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        val List(tryBlock) = t.astChildren.isBlock.l
        tryBlock.order shouldBe 1
        tryBlock.astChildren.isIdentifier.order(1).code.l shouldBe List("a")
        val List(catchX, catchY, catchZ) = t.astChildren.isControlStructure.isCatch.l
        catchX.order shouldBe 2
        catchX.ast.isIdentifier.code.l shouldBe List("b")
        catchX.ast.isLocal.code.l shouldBe List("short x")
        catchY.order shouldBe 3
        catchY.ast.isIdentifier.code.l shouldBe List("c")
        catchY.ast.isLocal.code.l shouldBe List("int y")
        catchZ.order shouldBe 4
        catchZ.ast.isIdentifier.code.l shouldBe List("d")
        catchZ.ast.isLocal.code.l shouldBe List("long z")
      }
    }

    "be correct for try with multiple catches and broken catch clause" in {
      val cpg: Cpg = code(
        """
          |int main() {
          |  try {}
          |  catch (int a) {}
          |  catch (...) {}
          |}
          |""".stripMargin,
        "file.cpp"
      )
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        val List(tryBlock) = t.astChildren.isBlock.l
        tryBlock.order shouldBe 1
        tryBlock.astChildren shouldBe empty
        val List(catchA, catchB) = t.astChildren.isControlStructure.isCatch.l
        catchA.order shouldBe 2
        catchA.ast.isBlock.astChildren shouldBe empty
        catchA.ast.isLocal.name.l shouldBe List("a")
        catchB.order shouldBe 3
        catchB.ast.isBlock.astChildren shouldBe empty
        catchB.ast.isLocal shouldBe empty
      }
    }

    "be correct for constructor initializer" in {
      val cpg = code(
        """
        |class Foo {
        |public:
        | Foo(int i){};
        |};
        |Foo f1(0);
      """.stripMargin,
        "file.cpp"
      )
      cpg.typeDecl
        .fullNameExact("Foo")
        .l
        .size shouldBe 1
      inside(cpg.call.codeExact("f1(0)").l) { case List(call: Call) =>
        call.name shouldBe "f1"
        call.argument(1).code shouldBe "0"
      }
    }

    "be correct for template class" in {
      val cpg = code(
        """
        | template<class T>
        | class Y
        | {
        |   void mf() { }
        | };
        | template class Y<char*>;
        | template void Y<double>::mf();
      """.stripMargin,
        "file.cpp"
      )
      cpg.typeDecl
        .name("Y")
        .l
        .size shouldBe 1
    }

    "be correct for template function" in {
      val cpg = code(
        """
        | template<typename T>
        | void f(T s)
        | { }
        |
        | template void f<double>(double); // instantiates f<double>(double)
        | template void f<>(char); // instantiates f<char>(char), template argument deduced
        | template void f(int); // instantiates f<int>(int), template argument deduced
      """.stripMargin,
        "file.cpp"
      )
      cpg.method
        .name("f")
        .l
        .size shouldBe 1
    }

    "be correct for constructor expression" in {
      val cpg = code(
        """
        |class Foo {
        |public:
        | Foo(int i) {  };
        |};
        |Foo x = Foo{0};
      """.stripMargin,
        "file.cpp"
      )
      cpg.typeDecl
        .fullNameExact("Foo")
        .l
        .size shouldBe 1
      inside(cpg.call.codeExact("Foo{0}").l) { case List(call: Call) =>
        call.name shouldBe "Foo"
        call.argument(1).code shouldBe "{0}"
      }
    }

    "be correct for method calls" in {
      val cpg = code("""
        |void foo(int x) {
        |  bar(x);
        |}
        |""".stripMargin)
      cpg.method
        .name("foo")
        .ast
        .isCall
        .name("bar")
        .argument
        .code("x")
        .size shouldBe 1
    }

    "be correct for method returns" in {
      val cpg = code("""
        |int d(int x) {
        |  return x * 2;
        |}
        |""".stripMargin)
      // TODO no step class defined for `Return` nodes
      cpg.method.name("d").ast.isReturn.astChildren.order(1).isCall.code.l shouldBe List("x * 2")
      cpg.method
        .name("d")
        .ast
        .isReturn
        .outE(EdgeTypes.ARGUMENT)
        .head
        .inNode()
        .get
        .asInstanceOf[CallDb]
        .code shouldBe "x * 2"
    }

    "be correct for binary method calls" in {
      val cpg = code("""
        |int d(int x) {
        |  return x * 2;
        |}
        |""".stripMargin)
      cpg.call.name(Operators.multiplication).code.l shouldBe List("x * 2")
    }

    "be correct for unary method calls" in {
      val cpg = code("""
        |bool invert(bool b) {
        |  return !b;
        |}
        |""".stripMargin)
      cpg.call.name(Operators.logicalNot).argument(1).code.l shouldBe List("b")
    }

    "be correct for unary expr" in {
      val cpg = code("""
        |int strnlen (const char *str, int max)
        |    {
        |      const char *end = memchr(str, 0, max);
        |      return end ? (int)(end - str) : max;
        |    }
        |""".stripMargin)
      inside(cpg.call.name(Operators.cast).astChildren.l) { case List(tpe: Unknown, call: Call) =>
        call.code shouldBe "end - str"
        call.argumentIndex shouldBe 2
        tpe.code shouldBe "int"
        tpe.argumentIndex shouldBe 1
      }
    }

    "be correct for post increment method calls" in {
      val cpg = code("""
        |int foo(int x) {
        |  int sub = x--;
        |  int pos = x++;
        |  return pos;
        |}
        |""".stripMargin)
      cpg.call.name(Operators.postIncrement).argument(1).code("x").size shouldBe 1
      cpg.call.name(Operators.postDecrement).argument(1).code("x").size shouldBe 1
    }

    "be correct for conditional expressions containing calls" in {
      val cpg = code("""
        |int abs(int x) {
        |  return x > 0 ? x : -x;
        |}
        |""".stripMargin)
      cpg.call.name(Operators.conditional).argument.code.l shouldBe List("x > 0", "x", "-x")
    }

    "be correct for sizeof expressions" in {
      val cpg = code("""
        |size_t int_size() {
        |  return sizeof(int);
        |}
        |""".stripMargin)
      inside(cpg.call.name(Operators.sizeOf).argument(1).l) { case List(i: Identifier) =>
        i.code shouldBe "int"
        i.name shouldBe "int"
      }
    }

    "be correct for label" in {
      val cpg = code("void foo() { label:; }")
      cpg.jumpTarget.code("label:;").size shouldBe 1
    }

    "be correct for array indexing" in {
      val cpg = code("""
        |int head(int x[]) {
        |  return x[0];
        |}
        |""".stripMargin)
      cpg.call.name(Operators.indirectIndexAccess).argument.code.l shouldBe List("x", "0")
    }

    "be correct for type casts" in {
      val cpg = code("""
        |int trunc(long x) {
        |  return (int) x;
        |}
        |""".stripMargin)
      cpg.call.name(Operators.cast).argument.code.l shouldBe List("int", "x")
    }

    "be correct for 'new' array" in {
      val cpg = code(
        """
        |int * alloc(int n) {
        |   int * arr = new int[n];
        |   return arr;
        |}
        |""".stripMargin,
        "file.cpp"
      )
      // TODO: "<operator>.new" is not part of Operators
      cpg.call.name("<operator>.new").code("new int\\[n\\]").argument.code("int").size shouldBe 1
    }

    "be correct for 'new' with explicit identifier" in {
      val cpg = code(
        """
        |void a() {
        |  char buf[80];
        |  new (buf) string("hi");
        |}
        |""".stripMargin,
        "file.cpp"
      )
      // TODO: "<operator>.new" is not part of Operators
      val List(newCall)         = cpg.call.name("<operator>.new").l
      val List(string, hi, buf) = newCall.argument.l
      string.argumentIndex shouldBe 1
      string.code shouldBe "string"
      hi.argumentIndex shouldBe 2
      hi.code shouldBe "\"hi\""
      buf.argumentIndex shouldBe 3
      buf.code shouldBe "buf"
    }

    // for: https://github.com/ShiftLeftSecurity/codepropertygraph/issues/1526
    "be correct for array size" in {
      val cpg = code("""
        |int main() {
        |  char buf[256];
        |  printf("%s", buf);
        |}
        |""".stripMargin)
      inside(cpg.local.l) { case List(buf: Local) =>
        buf.typeFullName shouldBe "char[256]"
        buf.name shouldBe "buf"
        buf.code shouldBe "char[256] buf"
      }
    }

    "be correct for array init" in {
      val cpg = code("""
        |int x[] = {0, 1, 2, 3};
        |""".stripMargin)
      inside(cpg.assignment.astChildren.l) { case List(ident: Identifier, call: Call) =>
        ident.typeFullName shouldBe "int[]"
        ident.order shouldBe 1
        call.code shouldBe "{0, 1, 2, 3}"
        call.order shouldBe 2
        call.name shouldBe Operators.arrayInitializer
        call.methodFullName shouldBe Operators.arrayInitializer
        val children = call.astChildren.l
        val args     = call.argument.l
        inside(children) { case List(literalA: Literal, literalB: Literal, literalC: Literal, literalD: Literal) =>
          literalA.order shouldBe 1
          literalA.code shouldBe "0"
          literalB.order shouldBe 2
          literalB.code shouldBe "1"
          literalC.order shouldBe 3
          literalC.code shouldBe "2"
          literalD.order shouldBe 4
          literalD.code shouldBe "3"
        }
        children shouldBe args
      }
    }

    "be correct for static array init" in {
      val cpg = code("""
        |static int x[] = {0, 1, 2, 3};
        |""".stripMargin)
      inside(cpg.assignment.astChildren.l) { case List(ident: Identifier, call: Call) =>
        ident.typeFullName shouldBe "int[]"
        ident.order shouldBe 1
        call.code shouldBe "{0, 1, 2, 3}"
        call.order shouldBe 2
        call.name shouldBe Operators.arrayInitializer
        call.methodFullName shouldBe Operators.arrayInitializer
        val children = call.astChildren.l
        val args     = call.argument.l
        inside(children) { case List(literalA: Literal, literalB: Literal, literalC: Literal, literalD: Literal) =>
          literalA.order shouldBe 1
          literalA.code shouldBe "0"
          literalB.order shouldBe 2
          literalB.code shouldBe "1"
          literalC.order shouldBe 3
          literalC.code shouldBe "2"
          literalD.order shouldBe 4
          literalD.code shouldBe "3"
        }
        children shouldBe args
      }
    }

    "be correct for const array init" in {
      val cpg = code("""
        |const int x[] = {0, 1, 2, 3};
        |""".stripMargin)
      inside(cpg.assignment.astChildren.l) { case List(ident: Identifier, call: Call) =>
        ident.typeFullName shouldBe "int[]"
        ident.order shouldBe 1
        call.code shouldBe "{0, 1, 2, 3}"
        call.order shouldBe 2
        call.name shouldBe Operators.arrayInitializer
        call.methodFullName shouldBe Operators.arrayInitializer
        val children = call.astChildren.l
        val args     = call.argument.l
        inside(children) { case List(literalA: Literal, literalB: Literal, literalC: Literal, literalD: Literal) =>
          literalA.order shouldBe 1
          literalA.code shouldBe "0"
          literalB.order shouldBe 2
          literalB.code shouldBe "1"
          literalC.order shouldBe 3
          literalC.code shouldBe "2"
          literalD.order shouldBe 4
          literalD.code shouldBe "3"
        }
        children shouldBe args
      }
    }

    "be correct for static const array init" in {
      val cpg = code("""
        |static const int x[] = {0, 1, 2, 3};
        |""".stripMargin)
      inside(cpg.assignment.astChildren.l) { case List(ident: Identifier, call: Call) =>
        ident.typeFullName shouldBe "int[]"
        ident.order shouldBe 1
        call.code shouldBe "{0, 1, 2, 3}"
        call.order shouldBe 2
        call.name shouldBe Operators.arrayInitializer
        call.methodFullName shouldBe Operators.arrayInitializer
        val children = call.astChildren.l
        val args     = call.argument.l
        inside(children) { case List(literalA: Literal, literalB: Literal, literalC: Literal, literalD: Literal) =>
          literalA.order shouldBe 1
          literalA.code shouldBe "0"
          literalB.order shouldBe 2
          literalB.code shouldBe "1"
          literalC.order shouldBe 3
          literalC.code shouldBe "2"
          literalD.order shouldBe 4
          literalD.code shouldBe "3"
        }
        children shouldBe args
      }
    }

    "be correct for array init with method refs" in {
      val cpg = code("""
        |static void methodA() { return; };
        |static int methodB() { return 0; };
        |static const struct foo bar = {
        | .a = methodA,
        | .b = methodB,
        |};""".stripMargin)
      val List(methodA, methodB) = cpg.method.nameNot("<global>").l
      inside(cpg.call.nameExact(Operators.arrayInitializer).assignment.l) { case List(callA: Call, callB: Call) =>
        val argsAIdent = callA.argument(1).asInstanceOf[Identifier]
        val argARef    = callA.argument(2).asInstanceOf[MethodRef]
        argsAIdent.order shouldBe 1
        argsAIdent.name shouldBe "a"
        argsAIdent.code shouldBe "a"
        argARef.order shouldBe 2
        argARef.methodFullName shouldBe methodA.fullName
        argARef.typeFullName shouldBe methodA.methodReturn.typeFullName
        val argsBIdent = callB.argument(1).asInstanceOf[Identifier]
        val argBRef    = callB.argument(2).asInstanceOf[MethodRef]
        argsBIdent.order shouldBe 1
        argsBIdent.code shouldBe "b"
        argsBIdent.name shouldBe "b"
        argBRef.order shouldBe 2
        argBRef.methodFullName shouldBe methodB.fullName
        argBRef.typeFullName shouldBe methodB.methodReturn.typeFullName
      }
    }

    "be correct for locals for array init" in {
      val cpg = code("""
        |bool x[2] = { TRUE, FALSE };
        |""".stripMargin)
      inside(cpg.local.l) { case List(x) =>
        x.name shouldBe "x"
        x.typeFullName shouldBe "bool[2]"
      }
    }

    "be correct for array init without actual assignment" in {
      val cpg = code(
        """
        |int foo{1};
        |int bar[]{0, 1, 2};
        |""".stripMargin,
        "test.cpp"
      )
      val List(localFoo, localBar) = cpg.local.l
      localFoo.name shouldBe "foo"
      localFoo.order shouldBe 1
      localBar.name shouldBe "bar"
      localBar.order shouldBe 3

      val List(assignment1, assignment2) = cpg.assignment.l
      assignment1.order shouldBe 2
      assignment1.code shouldBe "foo{1}"
      assignment1.name shouldBe Operators.assignment
      assignment1.methodFullName shouldBe Operators.assignment
      assignment2.order shouldBe 4
      assignment2.code shouldBe "bar[]{0, 1, 2}"
      assignment2.name shouldBe Operators.assignment
      assignment2.methodFullName shouldBe Operators.assignment

      inside(cpg.assignment.astChildren.l) {
        case List(identFoo: Identifier, identBar: Identifier, callFoo: Call, barCall: Call) =>
          identFoo.typeFullName shouldBe "int"
          identFoo.order shouldBe 1
          callFoo.code shouldBe "{1}"
          callFoo.order shouldBe 2
          callFoo.name shouldBe Operators.arrayInitializer
          callFoo.methodFullName shouldBe Operators.arrayInitializer
          val childrenFoo = callFoo.astChildren.l
          val argsFoo     = callFoo.argument.l
          inside(childrenFoo) { case List(literal: Literal) =>
            literal.order shouldBe 1
            literal.code shouldBe "1"
          }
          childrenFoo shouldBe argsFoo

          identBar.typeFullName shouldBe "int[3]"
          identBar.order shouldBe 1
          barCall.code shouldBe "{0, 1, 2}"
          barCall.order shouldBe 2
          barCall.name shouldBe Operators.arrayInitializer
          barCall.methodFullName shouldBe Operators.arrayInitializer
          val childrenBar = barCall.astChildren.l
          val argsBar     = barCall.argument.l
          inside(childrenBar) { case List(literalA: Literal, literalB: Literal, literalC: Literal) =>
            literalA.order shouldBe 1
            literalA.code shouldBe "0"
            literalB.order shouldBe 2
            literalB.code shouldBe "1"
            literalC.order shouldBe 3
            literalC.code shouldBe "2"
          }
          childrenBar shouldBe argsBar
      }
    }

    "be correct for 'new' object" in {
      val cpg = code(
        """
        |Foo* alloc(int n) {
        |   Foo* foo = new Foo(n, 42);
        |   return foo;
        |}
        |""".stripMargin,
        "file.cpp"
      )
      cpg.call.name("<operator>.new").codeExact("new Foo(n, 42)").argument.code("Foo").size shouldBe 1
    }

    "be correct for simple 'delete'" in {
      val cpg = code(
        """
        |int delete_number(int* n) {
        |  delete n;
        |}
        |""".stripMargin,
        "file.cpp"
      )
      cpg.call.name(Operators.delete).code("delete n").argument.code("n").size shouldBe 1
    }

    "be correct for array 'delete'" in {
      val cpg = code(
        """
        |void delete_number(int n[]) {
        |  delete[] n;
        |}
        |""".stripMargin,
        "file.cpp"
      )
      cpg.call.name(Operators.delete).codeExact("delete[] n").argument.code("n").size shouldBe 1
    }

    "be correct for const_cast" in {
      val cpg = code(
        """
        |void foo() {
        |  int y = const_cast<int>(n);
        |  return;
        |}
        |""".stripMargin,
        "file.cpp"
      )
      cpg.call.name(Operators.cast).codeExact("const_cast<int>(n)").argument.code.l shouldBe List("int", "n")
    }

    "be correct for static_cast" in {
      val cpg = code(
        """
        |void foo() {
        |  int y = static_cast<int>(n);
        |  return;
        |}
        |""".stripMargin,
        "file.cpp"
      )
      cpg.call.name(Operators.cast).codeExact("static_cast<int>(n)").argument.code.l shouldBe List("int", "n")
    }

    "be correct for dynamic_cast" in {
      val cpg = code(
        """
        |void foo() {
        |  int y = dynamic_cast<int>(n);
        |  return;
        |}
        |""".stripMargin,
        "file.cpp"
      )
      cpg.call.name(Operators.cast).codeExact("dynamic_cast<int>(n)").argument.code.l shouldBe List("int", "n")
    }

    "be correct for reinterpret_cast" in {
      val cpg = code(
        """
        |void foo() {
        |  int y = reinterpret_cast<int>(n);
        |  return;
        |}
        |""".stripMargin,
        "file.cpp"
      )
      cpg.call.name(Operators.cast).codeExact("reinterpret_cast<int>(n)").argument.code.l shouldBe List("int", "n")
    }

    "be correct for designated initializers in plain C" in {
      val cpg = code("""
        |void foo() {
        |  int a[3] = { [1] = 5, [2] = 10, [3 ... 9] = 15 };
        |};
      """.stripMargin)
      inside(cpg.assignment.head.astChildren.l) { case List(ident: Identifier, call: Call) =>
        ident.typeFullName shouldBe "int[3]"
        ident.order shouldBe 1
        call.code shouldBe "{ [1] = 5, [2] = 10, [3 ... 9] = 15 }"
        call.order shouldBe 2
        call.name shouldBe Operators.arrayInitializer
        call.methodFullName shouldBe Operators.arrayInitializer
        val children = call.astMinusRoot.isCall.name(Operators.assignment).l
        val args     = call.argument.astChildren.l
        inside(children) { case List(call1, call2, call3) =>
          call1.code shouldBe "[1] = 5"
          call1.name shouldBe Operators.assignment
          call1.astMinusRoot.code.l shouldBe List("1", "5")
          call1.argument.code.l shouldBe List("1", "5")
          call2.code shouldBe "[2] = 10"
          call2.name shouldBe Operators.assignment
          call2.astMinusRoot.code.l shouldBe List("2", "10")
          call2.argument.code.l shouldBe List("2", "10")
          call3.code shouldBe "[3 ... 9] = 15"
          call3.name shouldBe Operators.assignment
          val List(desCall) = call3.argument(1).start.collectAll[Call].l
          val List(value)   = call3.argument(2).start.collectAll[Literal].l
          value.code shouldBe "15"
          desCall.name shouldBe Operators.arrayInitializer
          desCall.code shouldBe "[3 ... 9]"
          desCall.argument.code.l shouldBe List("3", "9")
        }
        children shouldBe args
      }
    }

    "be correct for designated initializers in C++" in {
      val cpg = code(
        """
        |void foo() {
        |  int a[3] = { [1] = 5, [2] = 10, [3 ... 9] = 15 };
        |};
      """.stripMargin,
        "test.cpp"
      )
      inside(cpg.assignment.head.astChildren.l) { case List(ident: Identifier, call: Call) =>
        ident.typeFullName shouldBe "int[3]"
        ident.order shouldBe 1
        call.code shouldBe "{ [1] = 5, [2] = 10, [3 ... 9] = 15 }"
        call.order shouldBe 2
        call.name shouldBe Operators.arrayInitializer
        call.methodFullName shouldBe Operators.arrayInitializer
        val children = call.astMinusRoot.isCall.name(Operators.assignment).l
        val args     = call.argument.astChildren.l
        inside(children) { case List(call1, call2, call3) =>
          call1.code shouldBe "[1] = 5"
          call1.name shouldBe Operators.assignment
          call1.astMinusRoot.code.l shouldBe List("1", "5")
          call1.argument.code.l shouldBe List("1", "5")
          call2.code shouldBe "[2] = 10"
          call2.name shouldBe Operators.assignment
          call2.astMinusRoot.code.l shouldBe List("2", "10")
          call2.argument.code.l shouldBe List("2", "10")
          call3.code shouldBe "[3 ... 9] = 15"
          call3.name shouldBe Operators.assignment
          val List(desCall) = call3.argument(1).start.collectAll[Call].l
          val List(value)   = call3.argument(2).start.collectAll[Literal].l
          value.code shouldBe "15"
          desCall.name shouldBe Operators.arrayInitializer
          desCall.code shouldBe "[3 ... 9]"
          desCall.argument.code.l shouldBe List("3", "9")
        }
        children shouldBe args
      }
    }

    "be correct for struct designated initializers in plain C" in {
      val cpg = code("""
        |void foo() {
        |  struct foo b = { .a = 1, .b = 2 };
        |};
      """.stripMargin)
      inside(cpg.assignment.head.astChildren.l) { case List(ident: Identifier, call: Call) =>
        ident.typeFullName shouldBe "foo"
        ident.order shouldBe 1
        call.code shouldBe "{ .a = 1, .b = 2 }"
        call.order shouldBe 2
        call.name shouldBe Operators.arrayInitializer
        call.methodFullName shouldBe Operators.arrayInitializer
        val children = call.astMinusRoot.isCall.l
        val args     = call.argument.astChildren.l
        inside(children) { case List(call1, call2) =>
          call1.code shouldBe ".a = 1"
          call1.name shouldBe Operators.assignment
          call1.astMinusRoot.code.l shouldBe List("a", "1")
          call1.argument.code.l shouldBe List("a", "1")
          call2.code shouldBe ".b = 2"
          call2.name shouldBe Operators.assignment
          call2.astMinusRoot.code.l shouldBe List("b", "2")
          call2.argument.code.l shouldBe List("b", "2")
        }
        children shouldBe args
      }
    }

    "be correct for designated struct initializers in C++" in {
      val cpg = code(
        """
        |class Point3D {
        | public:
        |  int x;
        |  int y;
        |  int z;
        |};
        |
        |void foo() {
        |  Point3D point3D { .x = 1, .y = 2, .z = 3 };
        |};
      """.stripMargin,
        "test.cpp"
      )
      inside(cpg.call.code("point3D \\{ .x = 1, .y = 2, .z = 3 \\}").l) { case List(call: Call) =>
        call.name shouldBe "point3D"
        call.methodFullName shouldBe "point3D"
        inside(call.astChildren.l) { case List(initCall: Call) =>
          initCall.code shouldBe "{ .x = 1, .y = 2, .z = 3 }"
          initCall.name shouldBe Operators.arrayInitializer
          initCall.methodFullName shouldBe Operators.arrayInitializer
          val children = initCall.astMinusRoot.isCall.l
          val args     = initCall.argument.astChildren.l
          inside(children) { case List(call1, call2, call3) =>
            call1.code shouldBe ".x = 1"
            call1.name shouldBe Operators.assignment
            call1.astMinusRoot.code.l shouldBe List("x", "1")
            call1.argument.code.l shouldBe List("x", "1")
            call2.code shouldBe ".y = 2"
            call2.name shouldBe Operators.assignment
            call2.astMinusRoot.code.l shouldBe List("y", "2")
            call2.argument.code.l shouldBe List("y", "2")
            call3.code shouldBe ".z = 3"
            call3.name shouldBe Operators.assignment
            call3.astMinusRoot.code.l shouldBe List("z", "3")
            call3.argument.code.l shouldBe List("z", "3")
          }
          children shouldBe args
        }
      }
    }

    "be correct for call with pack expansion" in {
      val cpg = code(
        """
        |void foo(int x, int*... args) {
        |  foo(x, args...);
        |};
      """.stripMargin,
        "test.cpp"
      )
      inside(cpg.call.l) { case List(fooCall: Call) =>
        fooCall.code shouldBe "foo(x, args...)"
        inside(fooCall.argument.l) { case List(x, args) =>
          x.order shouldBe 1
          x.code shouldBe "x"
          args.order shouldBe 2
          args.code shouldBe "args"
        }
      }
    }

    "be correct for embedded ASM code" in {
      val cpg = code("""
        |asm(
        | "  push %ebp       \n"
        | "  movl %esp, %ebp \n"
        | "  push %ebx       \n"
        |);
      """.stripMargin)
      inside(cpg.method.ast.filter(_.label == NodeTypes.UNKNOWN).l) { case List(asm: Unknown) =>
        asm.code should startWith("asm(")
      }
    }

    "be correct for embedded ASM calls" in {
      val cpg = code("""
        |void foo() {
        |  asm("paddh %0, %1, %2\n\t"
        |	  : "=f" (x)
        |	  : "f" (y), "f" (z)
        |	);
        |}
      """.stripMargin)
      inside(cpg.method("foo").ast.filter(_.label == NodeTypes.UNKNOWN).l) { case List(asm: Unknown) =>
        asm.code should startWith("asm(")
      }
    }

    "be correct for compound statement expressions" in {
      val cpg = code("""
        |int x = ({int y = 1; y;}) + ({int z = 2; z;});
        |""".stripMargin)
      inside(cpg.call(Operators.addition).l) { case List(add) =>
        inside(add.argument.l) { case List(y, z) =>
          y.argumentIndex shouldBe 1
          y.order shouldBe 1
          inside(y.astChildren.l) { case List(_, c: Call, i: Identifier) =>
            c.code shouldBe "y = 1"
            i.code shouldBe "y"
          }
          z.argumentIndex shouldBe 2
          z.order shouldBe 2
          inside(z.astChildren.l) { case List(_, c: Call, i: Identifier) =>
            c.code shouldBe "z = 2"
            i.code shouldBe "z"
          }
        }
      }
    }

    "have correct line number for method content" in {
      val cpg = code("""
        |
        |
        |
        |
        | void method(int x) {
        |
        |   x = 1;
        | }
      """.stripMargin)
      cpg.method.name("method").lineNumber.l shouldBe List(6)
      cpg.method.name("method").block.assignment.lineNumber.l shouldBe List(8)
    }

    // for https://github.com/ShiftLeftSecurity/codepropertygraph/issues/1321
    "have correct line numbers example 1" in {
      val cpg = code("""
        |int main() {
        |int a = 0;
        |statementthatdoesnothing();
        |int b = 0;
        |int c = 0;
        |}
      """.stripMargin)
      inside(cpg.identifier.l) { case List(idA, idB, idC) =>
        idA.lineNumber shouldBe Option(3)
        idA.columnNumber shouldBe Option(5)
        idB.lineNumber shouldBe Option(5)
        idB.columnNumber shouldBe Option(5)
        idC.lineNumber shouldBe Option(6)
        idC.columnNumber shouldBe Option(5)
      }
    }

    // for https://github.com/ShiftLeftSecurity/codepropertygraph/issues/1321
    "have correct line/column numbers on all platforms" in {
      val windowsNewline = "\r\n"
      val windowsFixture: Cpg = code(
        s"void offset() {${windowsNewline}char * data = NULL;${windowsNewline}memset(data, 'A', 100-1); /* fill with 'A's */${windowsNewline}data = dataBuffer;$windowsNewline}"
      )
      val macNewline = "\r"
      val macFixture: Cpg = code(
        s"void offset() {${macNewline}char * data = NULL;${macNewline}memset(data, 'A', 100-1); /* fill with 'A's */${macNewline}data = dataBuffer;$macNewline}"
      )
      val linuxNewline = "\n"
      val linuxFixture: Cpg = code(
        s"void offset() {${linuxNewline}char * data = NULL;${linuxNewline}memset(data, 'A', 100-1); /* fill with 'A's */${linuxNewline}data = dataBuffer;$linuxNewline}"
      )

      val windowsLineNumbers = windowsFixture.identifier.lineNumber.l
      val macLineNumbers     = macFixture.identifier.lineNumber.l
      val linuxLineNumbers   = linuxFixture.identifier.lineNumber.l

      windowsLineNumbers should not be empty
      macLineNumbers should not be empty
      linuxLineNumbers should not be empty

      windowsLineNumbers shouldBe macLineNumbers
      windowsLineNumbers shouldBe linuxLineNumbers
      macLineNumbers shouldBe linuxLineNumbers

      val windowsColumnNumbers = windowsFixture.identifier.columnNumber.l
      val macColumnNumbers     = macFixture.identifier.columnNumber.l
      val linuxColumnNumbers   = linuxFixture.identifier.columnNumber.l

      windowsColumnNumbers should not be empty
      macColumnNumbers should not be empty
      linuxColumnNumbers should not be empty

      windowsColumnNumbers shouldBe macColumnNumbers
      windowsColumnNumbers shouldBe linuxColumnNumbers
      macColumnNumbers shouldBe linuxColumnNumbers

      windowsFixture.close()
      macFixture.close()
      linuxFixture.close()
    }

  }

  "AST with types" should {

    "be correct for function edge case" in {
      val cpg          = code("class Foo { char (*(*x())[5])() }", "test.cpp")
      val List(method) = cpg.method.nameNot("<global>").l
      method.name shouldBe "x"
      method.fullName shouldBe "Foo.x"
      method.code shouldBe "char (*(*x())[5])()"
      method.signature shouldBe "char Foo.x ()"
    }

    "be consistent with pointer types" in {
      val cpg = code("""
        |struct x { char * z; };
        |char *a(char *y) {
        |  char *x;
        |}
        |""".stripMargin)
      cpg.member.name("z").typeFullName.head shouldBe "char*"
      cpg.parameter.name("y").typeFullName.head shouldBe "char*"
      cpg.local.name("x").typeFullName.head shouldBe "char*"
      cpg.method.name("a").methodReturn.typeFullName.head shouldBe "char*"
    }

    "be consistent with array types" in {
      val cpg = code("""
        |struct x { char z[1]; };
        |void a(char y[1]) {
        |  char x[1];
        |}
        |""".stripMargin)
      cpg.member.name("z").typeFullName.head shouldBe "char[1]"
      cpg.parameter.name("y").typeFullName.head shouldBe "char[1]"
      cpg.local.name("x").typeFullName.head shouldBe "char[1]"
    }

    "be consistent with long number types" in {
      val cpg = code("""
        |#define BUFSIZE 0x111111111111111
        |void copy(char *string) {
        |	char buf[BUFSIZE];
        |	stpncpy(buf, string, BUFSIZE);
        |}
        |""".stripMargin)
      val List(bufLocal) = cpg.local.nameExact("buf").l
      bufLocal.typeFullName shouldBe "char[0x111111111111111]"
      bufLocal.code shouldBe "char[0x111111111111111] buf"
      cpg.literal.code.l shouldBe List("0x111111111111111")
    }
  }
}
