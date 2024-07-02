package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language.*

/** These tests are based off of those found in javasrc2cpg but modified to fit to Jimple's 3-address code rule and flat
  * AST.
  */
class ConstructorInvocationTests extends JimpleCode2CpgFixture {

  lazy val cpg: Cpg = code("""
      |class Foo {
      |  int x;
      |
      |  public Foo(int x) {
      |    this.x = x;
      |  }
      |
      |  public int getValue() {
      |    return x;
      |  }
      |}
      |
      |class Bar extends Foo {
      |  public Bar(int x) {
      |    super(x);
      |  }
      |
      |  public Bar(int x, int y) {
      |    this(x + y);
      |  }
      |
      |  public static Bar id(Bar b) {
      |    return b;
      |  }
      |
      |  public static void test1() {
      |    Bar b = new Bar(4, 2);
      |  }
      |
      |  public static void test2(Bar b) {
      |    b = new Bar(4, 2);
      |  }
      |
      |  public static void test3(Bar[] bs) {
      |    bs[0] = new Bar(42);
      |  }
      |}
      |""".stripMargin).cpg

  "it should create correct method nodes for constructors" in {
    cpg.method.name(io.joern.x2cpg.Defines.ConstructorMethodName).where(_.fullName("^Foo.*")).l match {
      case List(cons: Method) =>
        cons.fullName shouldBe "Foo.<init>:void(int)"
        cons.signature shouldBe "void(int)"
        cons.code.trim.startsWith("public void <init>(int)") shouldBe true
        cons.parameter.size shouldBe 2
        val objParam = cons.parameter.index(0).head
        objParam.name shouldBe "this"
        objParam.typeFullName shouldBe "Foo"
        objParam.dynamicTypeHintFullName shouldBe Seq("Foo")
        val otherParam = cons.parameter.index(1).head
        otherParam.name shouldBe "x"
        otherParam.typeFullName shouldBe "int"
        otherParam.dynamicTypeHintFullName shouldBe Seq()

      case res =>
        fail(s"Expected single Foo constructor, but got $res")
    }

    cpg.method.name(io.joern.x2cpg.Defines.ConstructorMethodName).where(_.fullName("^Bar.*")).l match {
      case List(cons1: Method, cons2: Method) =>
        cons1.fullName shouldBe "Bar.<init>:void(int)"
        cons1.signature shouldBe "void(int)"
        cons1.code.trim.startsWith("public void <init>(int)") shouldBe true
        cons1.parameter.size shouldBe 2
        cons1.parameter.index(0).head.name shouldBe "this"
        cons1.parameter.index(1).head.name shouldBe "x"

        cons2.fullName shouldBe "Bar.<init>:void(int,int)"
        cons2.signature shouldBe "void(int,int)"
        cons2.code.trim.startsWith("public void <init>(int, int)") shouldBe true
        cons2.parameter.size shouldBe 3
        cons2.parameter.index(0).head.name shouldBe "this"
        cons2.parameter.index(1).head.name shouldBe "x"
        cons2.parameter.index(2).head.name shouldBe "y"

      case res =>
        fail(s"Expected 2 Bar constructors, but got $res")
    }
  }

  "it should create joint `alloc` and `init` calls for a constructor invocation in a vardecl" in {
    cpg.typeDecl.name("Bar").method.name("test1").l match {
      case List(method) =>
        val List(_: Local, _: Local, assign: Call, init: Call, _: Call, _: Return) =
          method.astChildren.isBlock.astChildren.l: @unchecked

        assign.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
        assign.name shouldBe Operators.assignment
        val alloc = assign.argument(2).asInstanceOf[Call]
        alloc.name shouldBe Operators.alloc
        alloc.code shouldBe "new Bar"
        alloc.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
        alloc.methodFullName shouldBe Operators.alloc
        alloc.typeFullName shouldBe "Bar"
        alloc.argument.size shouldBe 0

        init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        init.methodFullName shouldBe "Bar.<init>:void(int,int)"
        init.callOut.head.fullName shouldBe "Bar.<init>:void(int,int)"
        init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
        init.typeFullName shouldBe "void"
        init.signature shouldBe "void(int,int)"
        init.code shouldBe "$stack1.Bar(4, 2)"

        init.argument.size shouldBe 3
        val List(obj: Identifier, initArg1: Literal, initArg2: Literal) = init.argument.l: @unchecked
        obj.argumentIndex shouldBe 0
        obj.name shouldBe "$stack1"
        obj.typeFullName shouldBe "Bar"
        obj.code shouldBe "$stack1"
        initArg1.code shouldBe "4"
        initArg2.code shouldBe "2"

      case res => fail(s"Expected main method in Bar but found $res")
    }
  }

  "it should create joint `alloc` and `init` calls for a constructor invocation in an assignment" in {
    cpg.typeDecl.name("Bar").method.name("test2").l match {
      case List(method) =>
        val List(_: Local, _: Local, assign: Call, init: Call, _: Call, _: Return) =
          method.astChildren.isBlock.astChildren.l: @unchecked

        assign.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
        assign.name shouldBe Operators.assignment
        val alloc = assign.argument(2).asInstanceOf[Call]
        alloc.name shouldBe Operators.alloc
        alloc.code shouldBe "new Bar"
        alloc.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
        alloc.methodFullName shouldBe Operators.alloc
        alloc.typeFullName shouldBe "Bar"
        alloc.argument.size shouldBe 0

        init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        init.methodFullName shouldBe "Bar.<init>:void(int,int)"
        init.callOut.head.fullName shouldBe "Bar.<init>:void(int,int)"
        init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
        init.typeFullName shouldBe "void"
        init.signature shouldBe "void(int,int)"
        init.code shouldBe "$stack1.Bar(4, 2)"

        init.argument.size shouldBe 3
        val List(obj: Identifier, initArg1: Literal, initArg2: Literal) = init.argument.l: @unchecked
        obj.argumentIndex shouldBe 0
        obj.name shouldBe "$stack1"
        obj.typeFullName shouldBe "Bar"
        obj.code shouldBe "$stack1"
        initArg1.code shouldBe "4"
        initArg2.code shouldBe "2"

      case res => fail(s"Expected main method in Bar but found $res")
    }
  }

  "it should create `alloc` and `init` calls in a block for complex assignments" in {
    cpg.typeDecl.name("Bar").method.name("test3").l match {
      case List(method) =>
        val List(allocAssign: Call, init: Call, assign: Call, _: Call, indexAccess: Call) =
          method.call.l
        val List(arrayAccess: Call, temp: Identifier) = assign.argument.l: @unchecked
        temp.name shouldBe "$stack1"
        temp.typeFullName shouldBe "Bar"
        temp.argumentIndex shouldBe 2
        temp.code shouldBe "$stack1"

        val alloc = allocAssign.argument(2).asInstanceOf[Call]
        alloc.name shouldBe Operators.alloc
        alloc.methodFullName shouldBe Operators.alloc
        alloc.argumentIndex shouldBe 2
        alloc.code shouldBe "new Bar"
        alloc.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
        alloc.typeFullName shouldBe "Bar"
        alloc.argument.size shouldBe 0

        init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        init.methodFullName shouldBe "Bar.<init>:void(int)"
        init.callOut.head.fullName shouldBe "Bar.<init>:void(int)"
        init.signature shouldBe "void(int)"
        init.code shouldBe "$stack1.Bar(42)"
        init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString

        init.argument.size shouldBe 2
        val List(receiver: Identifier, initArg1: Literal) = init.argument.l: @unchecked
        receiver.argumentIndex shouldBe 0
        receiver.name shouldBe "$stack1"
        receiver.typeFullName shouldBe "Bar"

        initArg1.code shouldBe "42"

      case res => fail(s"Expected method test3 in Bar but found $res")
    }
  }

  "it should create only `init` call for direct invocation using `this`" in {
    cpg.typeDecl.name("Bar").method.fullNameExact("Bar.<init>:void(int,int)").l match {
      case List(method) =>
        val List(assignAddition: Call, init: Call, addition: Call) = method.call.l

        init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        init.methodFullName shouldBe "Bar.<init>:void(int)"
        init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
        init.typeFullName shouldBe "void"
        init.signature shouldBe "void(int)"

        val List(temp: Identifier, add: Call) = assignAddition.argument.l: @unchecked
        temp.name shouldBe "$stack3"
        temp.argumentIndex shouldBe 1
        temp.typeFullName shouldBe "int"

        add.code shouldBe "x + y"

        val List(obj: Identifier, additionResultPointer: Identifier) = init.argument.l: @unchecked
        obj.name shouldBe "this"
        obj.argumentIndex shouldBe 0
        obj.typeFullName shouldBe "Bar"

        additionResultPointer.code shouldBe "$stack3"

      case res => fail(s"Expected Bar constructor but found $res")
    }
  }

  "it should create only `init` call for direct invocation using `super`" in {
    cpg.typeDecl.name("Bar").method.fullNameExact("Bar.<init>:void(int)").l match {
      case List(method) =>
        val List(alloc: Call) = method.call.l
        alloc.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        alloc.methodFullName shouldBe "Foo.<init>:void(int)"
        alloc.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
        alloc.typeFullName shouldBe "void"
        alloc.signature shouldBe "void(int)"
        alloc.argument(1).code shouldBe "x"

      case res => fail(s"Expected Bar constructor but found $res")
    }
  }

}
