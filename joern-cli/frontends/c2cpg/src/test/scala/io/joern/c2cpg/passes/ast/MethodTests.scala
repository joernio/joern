package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class MethodTests extends C2CpgSuite {

  "MethodTest1" should {
    val cpg = code("""
      |  int main(int argc, char **argv) {
      | }""".stripMargin)

    "should contain exactly one method node with correct fields" in {
      inside(cpg.method.name("main").l) { case List(x) =>
        x.name shouldBe "main"
        x.fullName shouldBe "main"
        x.code should startWith("int main(int argc, char **argv) {")
        x.signature shouldBe "int(int,char**)"
        x.isExternal shouldBe false
        x.order shouldBe 1
        x.filename shouldBe "Test0.c"
        x.lineNumber shouldBe Option(2)
        x.lineNumberEnd shouldBe Option(3)
        x.columnNumber shouldBe Option(3)
        x.columnNumberEnd shouldBe Option(2)
      }
    }

    "should return correct number of lines" in {
      cpg.method.name("main").numberOfLines.l shouldBe List(2)
    }

    "should allow traversing to parameters" in {
      cpg.method.name("main").parameter.name.toSetMutable shouldBe Set("argc", "argv")
    }

    "should allow traversing to methodReturn" in {
      cpg.method.name("main").methodReturn.typeFullName.l shouldBe List("int")
    }

    "should allow traversing to file" in {
      cpg.method.name("main").file.name.l should not be empty
    }

  }

  "MethodTest2" should {
    val cpg = code("int foo(); int bar() { return woo(); }")

    "should identify method as stub" in {
      cpg.method.isStub.fullName.sorted.l shouldBe List("<includes>:<global>", "foo", "woo")
      cpg.method.isNotStub.fullName.sorted.l shouldBe List("Test0.c:<global>", "bar")
    }
  }

  "MethodTest3" should {
    val cpg = code("void doFoo() {}")

    "should generate a type decl for top level method definitions" in {
      inside(cpg.method.name("doFoo").l) { case List(x) =>
        x.name shouldBe "doFoo"
        x.fullName shouldBe "doFoo"
        x.astParentType shouldBe NodeTypes.TYPE_DECL
        x.astParentFullName should endWith(NamespaceTraversal.globalNamespaceName)
      }
      cpg.typeDecl.fullName.l should contain("doFoo")
    }
  }

  "MethodTest4" should {
    val cpg = code("void doFoo();")

    "should not generate a type decl for method declarations" in {
      inside(cpg.method.name("doFoo").l) { case List(x) =>
        x.name shouldBe "doFoo"
        x.fullName shouldBe "doFoo"
        x.astParentType shouldBe NodeTypes.TYPE_DECL
        x.astParentFullName should endWith(NamespaceTraversal.globalNamespaceName)
      }
      cpg.typeDecl.fullName.l should not contain "doFoo"
    }
  }

  "MethodTest5" should {
    val cpg = code("void foo(int &data) {};", "test.cpp")

    "should be correct for pointer dereference parameter" in {
      inside(cpg.method("foo").parameter.l) { case List(data) =>
        data.index shouldBe 1
        data.name shouldBe "data"
        data.code shouldBe "int &data"
        data.typeFullName shouldBe "int&"
        data.isVariadic shouldBe false
      }
    }
  }

  "MethodTest6" should {
    val cpg = code(
      """
      |void foo<A,
      |         B,
      |         C>() {};
      |""".stripMargin,
      "test.cpp"
    )

    "should be correct for methods with line breaks / whitespace" in {
      inside(cpg.method("foo").l) { case List(foo) =>
        foo.name shouldBe "foo"
        foo.fullName shouldBe "foo:void()"
        foo.signature shouldBe "void()"
      }
    }
  }

  "MethodTest7" should {
    val cpg = code("""
        |int foo(int x, int y) {
        |
        |}
        |""".stripMargin)

    "have correct METHOD node for method foo" in {
      val List(method) = cpg.method.nameExact("foo").l
      method.isExternal shouldBe false
      method.fullName shouldBe "foo"
      method.signature shouldBe "int(int,int)"
      method.lineNumber shouldBe Option(2)
      method.columnNumber shouldBe Option(1)
      method.lineNumberEnd shouldBe Option(4)
      method.columnNumberEnd shouldBe Option(1)
      method.code should startWith("int foo(int x, int y) {")
    }

    "have correct METHOD_PARAMETER_IN nodes for method foo" in {
      val List(param1, param2) = cpg.method.nameExact("foo").parameter.l
      param1.order shouldBe 1
      param1.code shouldBe "int x"
      param1.name shouldBe "x"
      param1.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      param1.lineNumber shouldBe Option(2)
      param1.columnNumber shouldBe Option(9)

      param2.order shouldBe 2
      param2.code shouldBe "int y"
      param2.name shouldBe "y"
      param2.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      param2.lineNumber shouldBe Option(2)
      param2.columnNumber shouldBe Option(16)
    }

    "have correct METHOD_RETURN node for method foo" in {
      val List(ret) = cpg.method.nameExact("foo").methodReturn.l
      ret.code shouldBe "RET"
      ret.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      ret.lineNumber shouldBe Option(2)
      ret.columnNumber shouldBe Option(1)
    }

  }

  "MethodTest8" should {
    val cpg = code("""
        |void method1() {
        |  int x;
        |  x = 1;
        |}
        |
        |void method2(int x) {
        |  x = 1;
        |}
        |
        |void method3(int x) {
        |  int y;
        |  {
        |    int x;
        |    int y;
        |
        |    x = 1;
        |    y = 1;
        |  }
        |
        |  x = 1;
        |  y = 1;
        |}
        |""".stripMargin)

    "be correct for local x in method1" in {
      val List(method)       = cpg.method.nameExact("method1").l
      val List(indentifierX) = method.block.ast.isIdentifier.l
      indentifierX.name shouldBe "x"

      val localX = indentifierX._localViaRefOut.get
      localX.name shouldBe "x"
    }

    "be correct for parameter x in method2" in {
      val List(method)       = cpg.method.nameExact("method2").l
      val List(indentifierX) = method.block.ast.isIdentifier.l
      indentifierX.name shouldBe "x"

      val parameterX = indentifierX._methodParameterInViaRefOut.get
      parameterX.name shouldBe "x"
    }

    "be correct for all identifiers x, y in method3" in {
      val List(method)           = cpg.method.nameExact("method3").l
      val List(outerIdentifierX) = method.block.astChildren.astChildren.isIdentifier.nameExact("x").l

      val parameterX = outerIdentifierX._methodParameterInViaRefOut.get
      parameterX.name shouldBe "x"

      val List(expectedParameterX) = method.parameter.l
      expectedParameterX.name shouldBe "x"
      parameterX shouldBe expectedParameterX

      val List(outerIdentifierY) = method.block.astChildren.astChildren.isIdentifier.nameExact("y").l

      val outerLocalY = outerIdentifierY._localViaRefOut.get
      outerLocalY.name shouldBe "y"

      val List(expectedOuterLocalY) = method.block.astChildren.isLocal.l
      expectedOuterLocalY.name shouldBe "y"
      outerLocalY shouldBe expectedOuterLocalY

      val List(nestedBlock) = method.block.astChildren.isBlock.l

      val List(nestedIdentifierX) = nestedBlock.ast.isIdentifier.nameExact("x").l
      nestedIdentifierX.name shouldBe "x"

      val nestedLocalX = nestedIdentifierX._localViaRefOut.get
      nestedLocalX.name shouldBe "x"

      val List(expectedNestedLocalX) = nestedBlock.ast.isLocal.nameExact("x").l
      nestedLocalX shouldBe expectedNestedLocalX

      val List(nestedIdentifierY) = nestedBlock.ast.isIdentifier.nameExact("y").l
      nestedIdentifierY.name shouldBe "y"

      val nestedLocalY = nestedIdentifierY._localViaRefOut.get
      nestedLocalY.name shouldBe "y"

      val List(expectedNestedLocalY) = nestedBlock.ast.isLocal.nameExact("y").l
      nestedLocalY shouldBe expectedNestedLocalY
    }
  }

  "MethodTest9" should {
    val cpg = code(
      """
        |int main(char **argv, int argc) {
        |  return abs(argc);
        |}
        |
        |int abs(int j);
        |int abs(int j);
        |int abs(int j);
        |""".stripMargin,
      "test.cpp"
    )

    "deduplicate method forward declarations correctly" in {
      cpg.method.fullNameExact("abs:int(int)").size shouldBe 1
      cpg.call.name("abs").callee(NoResolve).size shouldBe 1
    }

  }

  "Static modifier for methods" should {
    "be correct" in {
      val cpg = code(
        """
          |static void staticCMethodDecl();
          |static void staticCMethodDef() {}
          |""".stripMargin,
        "test.c"
      ).moreCode(
        """
          |class A {
          |  static void staticCPPMethodDecl();
          |	 static void staticCPPMethodDef() {}
          |};
          |""".stripMargin,
        "test.cpp"
      )
      val List(staticCMethodDecl)   = cpg.method.nameExact("staticCMethodDecl").isStatic.l
      val List(staticCMethodDef)    = cpg.method.nameExact("staticCMethodDef").isStatic.l
      val List(staticCPPMethodDecl) = cpg.method.nameExact("staticCPPMethodDecl").isStatic.l
      val List(staticCPPMethodDef)  = cpg.method.nameExact("staticCPPMethodDef").isStatic.l
      staticCMethodDecl.fullName shouldBe "staticCMethodDecl"
      staticCMethodDef.fullName shouldBe "staticCMethodDef"
      staticCPPMethodDecl.fullName shouldBe "A.staticCPPMethodDecl:void()"
      staticCPPMethodDef.fullName shouldBe "A.staticCPPMethodDef:void()"
    }
  }

  "Name for method parameter in parentheses" should {
    "be correct" in {
      val cpg = code("""
          |int foo(int * (a)) {
          |  int (x) = a;
          |  return 2 * *a;
          |}
          |""".stripMargin)
      val List(paramA) = cpg.method("foo").parameter.l
      paramA.code shouldBe "int * (a)"
      paramA.typeFullName shouldBe "int*"
      paramA.name shouldBe "a"
      cpg.identifier.nameExact("x").size shouldBe 1
      cpg.method("foo").local.nameExact("x").size shouldBe 1
    }
  }

  "Method name, signature and full name tests" should {
    "be correct for plain C method" in {
      val cpg = code(
        """
          |int method(int);
          |""".stripMargin,
        "test.c"
      )
      val List(method) = cpg.method.nameExact("method").l
      method.signature shouldBe "int(int)"
      method.fullName shouldBe "method"
    }

    "be correct for C++ method with long int types" in {
      val cpg = code(
        """
          |long methodA(long int a, long int b) { return a *  b; }
          |long int methodB(long a, long b) { return a *  b; }
          |int main() {
          |  methodA(1.9, 2.9);
          |  methodB(1.9, 2.9);
          |  return 0;
          |}
          |""".stripMargin,
        "test.cpp"
      )
      val List(methodA) = cpg.method.nameExact("methodA").l
      methodA.signature shouldBe "long(long,long)"
      methodA.fullName shouldBe "methodA:long(long,long)"
      val List(methodB) = cpg.method.nameExact("methodB").l
      methodB.signature shouldBe "long(long,long)"
      methodB.fullName shouldBe "methodB:long(long,long)"
    }

    "be correct for C++ method with long long int types" in {
      val cpg = code(
        """
          |long long methodA(long long int a, long long int b) { return a *  b; }
          |long long int methodB(long long a, long long b) { return a *  b; }
          |int main() {
          |  methodA(1.9, 2.9);
          |  methodB(1.9, 2.9);
          |  return 0;
          |}
          |""".stripMargin,
        "test.cpp"
      )
      val List(methodA) = cpg.method.nameExact("methodA").l
      methodA.signature shouldBe "longlong(longlong,longlong)"
      methodA.fullName shouldBe "methodA:longlong(longlong,longlong)"
      val List(methodB) = cpg.method.nameExact("methodB").l
      methodB.signature shouldBe "longlong(longlong,longlong)"
      methodB.fullName shouldBe "methodB:longlong(longlong,longlong)"
    }

    "be correct for C++ method with short int types" in {
      val cpg = code(
        """
          |short methodA(short int a, short int b) { return a *  b; }
          |short int methodB(short a, short b) { return a *  b; }
          |int main() {
          |  methodA(1, 2);
          |  methodB(1, 2);
          |  return 0;
          |}
          |""".stripMargin,
        "test.cpp"
      )
      val List(methodA) = cpg.method.nameExact("methodA").l
      methodA.signature shouldBe "short(short,short)"
      methodA.fullName shouldBe "methodA:short(short,short)"
      val List(methodB) = cpg.method.nameExact("methodB").l
      methodB.signature shouldBe "short(short,short)"
      methodB.fullName shouldBe "methodB:short(short,short)"
    }

    "be correct for C++ method in class" in {
      val cpg = code(
        """
          |#include <vector>
          |
          |class A {
          |  public:
          |    int foo1(void) { return 0; }
          |    std::vector<int> foo2(void) {
          |      std::vector<int> v;
          |      return v;
          |    }
          |}
          |
          |std::vector<int> foo3(void) {
          |  std::vector<int> v;
          |  return v;
          |}
          |""".stripMargin,
        "test.cpp"
      )
      cpg.method.filter(_.name.contains("foo")).methodReturn.typeFullName.l shouldBe List("int", "vector", "vector")
    }

    "be correct for C function pointer" in {
      val cpg = code(
        """
          |int (*foo)(int, int) = { 0 };
          |int (*bar[])(int, int) = { 0 };
          |""".stripMargin,
        "test.c"
      )
      val List(foo, bar) = cpg.local.l
      foo.name shouldBe "foo"
      foo.typeFullName shouldBe "int(*)(int,int)"
      bar.name shouldBe "bar"
      bar.typeFullName shouldBe "int(*[])(int,int)"
    }

    "be correct for plain method CPP" in {
      val cpg = code(
        """
          |namespace NNN {
          |  int method(int);
          |}
          |""".stripMargin,
        "test.cpp"
      )
      val List(method) = cpg.method.nameExact("method").l
      method.signature shouldBe "int(int)"
      method.fullName shouldBe "NNN.method:int(int)"
    }

    "be correct for plain extern C method" in {
      val cpg = code(
        """
          |namespace NNN {
          |  extern "C" {
          |    int method(int);
          |  }
          |}
          |""".stripMargin,
        "test.cpp"
      )
      val List(method) = cpg.method.nameExact("method").l
      method.signature shouldBe "int(int)"
      method.fullName shouldBe "method"
    }

    "be correct for class method" in {
      val cpg = code(
        """
          |namespace NNN {
          |  class CCC {
          |    int method(int);
          |  }
          |}
          |""".stripMargin,
        "test.cpp"
      )
      val List(method) = cpg.method.nameExact("method").l
      method.signature shouldBe "int(int)"
      method.fullName shouldBe "NNN.CCC.method:int(int)"
    }

    "be correct for class method with implicit member access" in {
      val cpg = code(
        """
          |class A {
          |  int var;
          |  void meth();
          |};
          |namespace Foo {
          |  void A::meth() {
          |    assert(this->var == var);
          |  }
          |}""".stripMargin,
        "test.cpp"
      )
      val List(implicitThisParam) = cpg.method.name("meth").parameter.l
      implicitThisParam.name shouldBe Defines.This
      implicitThisParam.typeFullName shouldBe "A*"
      val List(trueVarAccess) = cpg.call.name(Operators.equals).argument.argumentIndex(1).isCall.l
      trueVarAccess.code shouldBe "this->var"
      trueVarAccess.name shouldBe Operators.indirectFieldAccess
      val List(trueThisId, trueVarFieldIdent) = trueVarAccess.argument.l
      trueThisId.code shouldBe Defines.This
      trueThisId.isIdentifier shouldBe true
      trueThisId.asInstanceOf[Identifier].typeFullName shouldBe "A*"
      trueThisId._refOut.l shouldBe List(implicitThisParam)
      trueVarFieldIdent.code shouldBe "var"
      trueVarFieldIdent.isFieldIdentifier shouldBe true

      val List(varAccess) = cpg.call.name(Operators.equals).argument.argumentIndex(2).isCall.l
      varAccess.code shouldBe "this->var"
      varAccess.name shouldBe Operators.indirectFieldAccess
      val List(thisId, varFieldIdent) = varAccess.argument.l
      thisId.code shouldBe Defines.This
      thisId.isIdentifier shouldBe true
      thisId.asInstanceOf[Identifier].typeFullName shouldBe "A*"
      thisId._refOut.l shouldBe List(implicitThisParam)
      varFieldIdent.code shouldBe "var"
      varFieldIdent.isFieldIdentifier shouldBe true
    }

    "be correct for class method in nested class" in {
      val cpg = code(
        """class Outer {
          |  class Inner {
          |    void Method();
          |    int member;
          | };
          |};
          |void Outer::Inner::Method() {
          |  member;
          |}""".stripMargin,
        "test.cpp"
      )
      cpg.identifier.name("member").size shouldBe 0
      val List(memberCall) = cpg.call.codeExact("this->member").l
      memberCall.typeFullName shouldBe "int"
      memberCall.name shouldBe Operators.indirectFieldAccess
      memberCall.argument.isIdentifier.typeFullName.l shouldBe List("Outer.Inner*")
      memberCall.argument.isFieldIdentifier.code.l shouldBe List("member")
    }
  }
}
