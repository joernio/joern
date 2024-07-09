package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Block, Call, Identifier, Literal, Local, Method}
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language.*

import scala.util.Try

class NewConstructorInvocationTests extends JavaSrcCode2CpgFixture {
  "inner class constructor invocations" when {
    "the receiver is an object creation expression" should {
      val cpg = code("""
        |class Foo {
        |  class Bar {}
        |
        |  public static void test() {
        |    Bar b = new Foo().new Bar();
        |  }
        |}
        |""".stripMargin)

      "have the correct block lowering" in {
        inside(cpg.call.methodFullName(".*Bar.*init.*").l) { case List(barInit) =>
          barInit.methodFullName shouldBe "Foo$Bar.<init>:void()"

          inside(barInit.argument.l) { case List(barReceiver: Identifier, outerClass: Block) =>
            barReceiver.name shouldBe "b"
            barReceiver.typeFullName shouldBe "Foo$Bar"

            inside(outerClass.astChildren.l) {
              case List(tmpLocal: Local, tmpAssign: Call, tmpInit: Call, tmpRet: Identifier) =>
                tmpLocal.name shouldBe "$obj0"
                tmpLocal.typeFullName shouldBe "Foo"

                tmpAssign.code shouldBe "$obj0 = new Foo()"
                tmpAssign.typeFullName shouldBe "Foo"
                tmpAssign.methodFullName shouldBe Operators.assignment

                tmpInit.code shouldBe "new Foo()"
                inside(tmpInit.argument.l) { case List(tmpReceiver: Identifier) =>
                  tmpReceiver.name shouldBe "$obj0"
                  tmpReceiver.typeFullName shouldBe "Foo"
                }
            }
          }
        }
      }
    }

    "the receiver is a variable" should {
      val cpg = code("""
        |class Foo {
        |  class Bar {}
        |
        |  public static void test(Foo f) {
        |    Bar b = f.new Bar();
        |  }
        |}
        |""".stripMargin)

      "have the correct structure" in {
        inside(cpg.call.methodFullName(".*Bar.*init.*").l) { case List(barInit) =>
          barInit.methodFullName shouldBe "Foo$Bar.<init>:void()"

          inside(barInit.argument.l) { case List(barReceiver: Identifier, outerClass: Identifier) =>
            barReceiver.name shouldBe "b"
            barReceiver.typeFullName shouldBe "Foo$Bar"

            outerClass.name shouldBe "f"
            outerClass.typeFullName shouldBe "Foo"
            outerClass.refsTo.l shouldBe cpg.method.name("test").parameter.name("f").l
          }
        }
      }
    }

    "the receiver is a call" should {
      val cpg = code("""
        |class Foo {
        |  class Bar {}
        |
        |  public static Foo foo() {
        |    return new Foo();
        |  }
        |
        |  public static void test() {
        |    Bar b = foo().new Bar();
        |  }
        |}
        |""".stripMargin)

      "have the correct structure" in {
        inside(cpg.call.methodFullName(".*Bar.*init.*").l) { case List(barInit) =>
          barInit.methodFullName shouldBe "Foo$Bar.<init>:void()"

          inside(barInit.argument.l) { case List(barReceiver: Identifier, outerClass: Call) =>
            barReceiver.name shouldBe "b"
            barReceiver.typeFullName shouldBe "Foo$Bar"

            outerClass.name shouldBe "foo"
            outerClass.typeFullName shouldBe "Foo"
            outerClass.methodFullName shouldBe "Foo.foo:Foo()"
          }
        }
      }
    }
  }

  "constructor init method call" should {
    val cpg = code("""
      |class Foo {
      |  Foo(long aaa) {
      |  }
      |  static void method() {
      |    Foo foo = new Foo(1);
      |  }
      |}
      |""".stripMargin)

    "have correct methodFullName and signature" in {
      val initCall = cpg.call.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).head
      initCall.signature shouldBe "void(long)"
      initCall.methodFullName shouldBe s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(long)"
    }
  }

  "a simple single argument constructor" should {
    val fooCpg = code("""
      |class Foo {
      |  int x;
      |
      |  public Foo(int x) {
      |    this.x = x;
      |  }
      |}
      |""".stripMargin)
    "create the correct Ast for the constructor" in {
      fooCpg.typeDecl.name("Foo").method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l match {
        case List(cons: Method) =>
          cons.fullName shouldBe s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)"
          cons.signature shouldBe "void(int)"
          cons.code shouldBe "public Foo(int x)"
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
    }
  }

  "an object creation expression as an argument to an unresolved call should still be created correctly" in {
    val cpg = code("""
        |class Foo {
        |  public Foo(int x) {}
        |
        |  void foo() {
        |    sink(new Foo(42));
        |  }
        |}
        |""".stripMargin)

    inside(cpg.call.name("sink").argument.l) { case List(_: Identifier, initBlock: Block) =>
      inside(initBlock.ast.collectAll[Call].nameExact("<init>").l) { case List(initCall) =>
        inside(initCall.argument.l) { case List(_: Identifier, givenArg: Literal) =>
          givenArg.code shouldBe "42"
        }
      }
    }
  }
}

class ConstructorInvocationTests extends JavaSrcCode2CpgFixture {

  val cpg = code("""
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
      |  public static void bar() {
      |    id(new Bar(42));
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
      |""".stripMargin)

  "it should create correct method nodes for constructors" in {

    cpg.typeDecl.name("Bar").method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l match {
      case List(cons1: Method, cons2: Method) =>
        cons1.fullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)"
        cons1.signature shouldBe "void(int)"
        cons1.code shouldBe "public Bar(int x)"
        cons1.parameter.size shouldBe 2
        cons1.parameter.index(0).head.name shouldBe "this"
        cons1.parameter.index(1).head.name shouldBe "x"

        cons2.fullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int,int)"
        cons2.signature shouldBe "void(int,int)"
        cons2.code shouldBe "public Bar(int x, int y)"
        cons2.parameter.size shouldBe 3
        cons2.parameter.index(0).head.name shouldBe "this"
        cons2.parameter.index(1).head.name shouldBe "x"
        cons2.parameter.index(2).head.name shouldBe "y"

      case res =>
        fail(s"Expected 2 Bar constructors, but got $res")
    }
  }

  "it should not leave any orphan identifiers" in {
    cpg.identifier.filter(node => Try(node.astParent).isFailure).l shouldBe Nil
  }

  "it should not have any nodes with multiple parents" in {
    cpg.all.collectAll[AstNode].filter(node => Try(node._astIn.size).toOption.exists(_ >= 2)).l shouldBe Nil
  }

  "it should create joint `alloc` and `init` calls for a constructor invocation in a vardecl" in {
    cpg.typeDecl.name("Bar").method.name("test1").l match {
      case List(method) =>
        val List(_: Local, assign: Call, init: Call) = method.astChildren.isBlock.astChildren.l: @unchecked

        assign.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()
        assign.name shouldBe Operators.assignment
        val alloc = assign.argument(2).asInstanceOf[Call]
        alloc.name shouldBe "<operator>.alloc"
        alloc.code shouldBe "new Bar(4, 2)"
        alloc.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()
        alloc.methodFullName shouldBe "<operator>.alloc"
        alloc.typeFullName shouldBe "Bar"
        alloc.argument.size shouldBe 0

        init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        init.methodFullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int,int)"
        init._methodViaCallOut.head.fullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int,int)"
        init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()
        init.typeFullName shouldBe "void"
        init.signature shouldBe "void(int,int)"
        init.code shouldBe "new Bar(4, 2)"

        init.argument.size shouldBe 3
        val List(obj: Identifier, initArg1: Literal, initArg2: Literal) = init.argument.l: @unchecked
        obj.order shouldBe 1
        obj.argumentIndex shouldBe 0
        obj.name shouldBe "b"
        obj.typeFullName shouldBe "Bar"
        obj.code shouldBe "b"
        initArg1.code shouldBe "4"
        initArg1.order shouldBe 2
        initArg1.argumentIndex shouldBe 1
        initArg2.code shouldBe "2"
        initArg2.order shouldBe 3
        initArg2.argumentIndex shouldBe 2

      case res => fail(s"Expected main method in Bar but found $res")
    }
  }

  "it should create joint `alloc` and `init` calls for a constructor invocation in an assignment" in {
    cpg.typeDecl.name("Bar").method.name("test2").l match {
      case List(method) =>
        val List(assign: Call, init: Call) = method.astChildren.isBlock.astChildren.l: @unchecked

        assign.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()
        assign.name shouldBe Operators.assignment
        val alloc = assign.argument(2).asInstanceOf[Call]
        alloc.name shouldBe "<operator>.alloc"
        alloc.code shouldBe "new Bar(4, 2)"
        alloc.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()
        alloc.methodFullName shouldBe "<operator>.alloc"
        alloc.typeFullName shouldBe "Bar"
        alloc.argument.size shouldBe 0

        init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        init.methodFullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int,int)"
        init._methodViaCallOut.head.fullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int,int)"
        init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()
        init.typeFullName shouldBe "void"
        init.signature shouldBe "void(int,int)"
        init.code shouldBe "new Bar(4, 2)"

        init.argument.size shouldBe 3
        val List(obj: Identifier, initArg1: Literal, initArg2: Literal) = init.argument.l: @unchecked
        obj.order shouldBe 1
        obj.argumentIndex shouldBe 0
        obj.name shouldBe "b"
        obj.typeFullName shouldBe "Bar"
        obj.code shouldBe "b"
        initArg1.code shouldBe "4"
        initArg1.order shouldBe 2
        initArg1.argumentIndex shouldBe 1
        initArg2.code shouldBe "2"
        initArg2.order shouldBe 3
        initArg2.argumentIndex shouldBe 2

      case res => fail(s"Expected main method in Bar but found $res")
    }
  }

  "it should create `alloc` and `init` calls in a block for constructor invocations not in assignments" in {
    cpg.typeDecl.name("Bar").method.name("bar").l match {
      case List(method) =>
        val idCall                                                        = method.call.name("id").head
        val consBlock                                                     = idCall.argument(1).asInstanceOf[Block]
        val List(local: Local, assign: Call, init: Call, ret: Identifier) = consBlock.astChildren.l: @unchecked

        local.name shouldBe "$obj0"
        local.typeFullName shouldBe "Bar"
        local.code shouldBe "$obj0"

        val List(temp: Identifier, alloc: Call) = assign.argument.l: @unchecked
        temp.name shouldBe "$obj0"
        temp.typeFullName shouldBe "Bar"
        temp.order shouldBe 1
        temp.argumentIndex shouldBe 1
        temp.code shouldBe "$obj0"

        alloc.name shouldBe "<operator>.alloc"
        alloc.methodFullName shouldBe "<operator>.alloc"
        alloc.order shouldBe 2
        alloc.argumentIndex shouldBe 2
        alloc.code shouldBe "new Bar(42)"
        alloc.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()
        alloc.typeFullName shouldBe "Bar"
        alloc.argument.size shouldBe 0

        init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        init.methodFullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)"
        init._methodViaCallOut.head.fullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)"
        init.signature shouldBe "void(int)"
        init.code shouldBe "new Bar(42)"
        init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()

        init.argument.size shouldBe 2
        val List(obj: Identifier, initArg1: Literal) = init.argument.l: @unchecked
        obj.order shouldBe 1
        obj.argumentIndex shouldBe 0
        obj.name shouldBe "$obj0"
        obj.typeFullName shouldBe "Bar"

        initArg1.code shouldBe "42"
        initArg1.order shouldBe 2
        initArg1.argumentIndex shouldBe 1

      case res => fail(s"Expected method bar in Bar but found $res")
    }
  }

  "it should create `alloc` and `init` calls in a block for complex assignments" in {
    cpg.typeDecl.name("Bar").method.name("test3").l match {
      case List(method) =>
        val assignCall = method.call.nameExact("<operator>.assignment").head
        val consBlock  = assignCall.argument(2).asInstanceOf[Block]
        val List(local: Local, assign: Call, init: Call, ret: Identifier) = consBlock.astChildren.l: @unchecked

        local.name shouldBe "$obj1"
        local.typeFullName shouldBe "Bar"
        local.code shouldBe "$obj1"

        val List(temp: Identifier, alloc: Call) = assign.argument.l: @unchecked
        temp.name shouldBe "$obj1"
        temp.typeFullName shouldBe "Bar"
        temp.order shouldBe 1
        temp.argumentIndex shouldBe 1
        temp.code shouldBe "$obj1"

        alloc.name shouldBe "<operator>.alloc"
        alloc.methodFullName shouldBe "<operator>.alloc"
        alloc.order shouldBe 2
        alloc.argumentIndex shouldBe 2
        alloc.code shouldBe "new Bar(42)"
        alloc.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()
        alloc.typeFullName shouldBe "Bar"
        alloc.argument.size shouldBe 0

        init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        init.methodFullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)"
        init._methodViaCallOut.head.fullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)"
        init.signature shouldBe "void(int)"
        init.code shouldBe "new Bar(42)"
        init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()

        init.argument.size shouldBe 2
        val List(obj: Identifier, initArg1: Literal) = init.argument.l: @unchecked
        obj.order shouldBe 1
        obj.argumentIndex shouldBe 0
        obj.name shouldBe "$obj1"
        obj.typeFullName shouldBe "Bar"

        initArg1.code shouldBe "42"
        initArg1.order shouldBe 2
        initArg1.argumentIndex shouldBe 1

      case res => fail(s"Expected method test3 in Bar but found $res")
    }
  }

  "it should create only `init` call for direct invocation using `this`" in {
    cpg.typeDecl
      .name("Bar")
      .method
      .fullNameExact(s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int,int)")
      .l match {
      case List(method) =>
        val List(init: Call) = method.astChildren.isBlock.astChildren.l: @unchecked
        init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        init.methodFullName shouldBe s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)"
        init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()
        init.typeFullName shouldBe "void"
        init.signature shouldBe "void(int)"

        val List(obj: Identifier, initArg1: Call) = init.argument.l: @unchecked
        obj.name shouldBe "this"
        obj.order shouldBe 1
        obj.argumentIndex shouldBe 0
        obj.typeFullName shouldBe "Bar"

        initArg1.code shouldBe "x + y"
        initArg1.order shouldBe 2
        initArg1.argumentIndex shouldBe 1

      case res => fail(s"Expected Bar constructor but found $res")
    }
  }

  "it should create only `init` call for direct invocation using `super`" in {
    cpg.typeDecl
      .name("Bar")
      .method
      .fullNameExact(s"Bar.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)")
      .l match {
      case List(method) =>
        val List(init: Call) = method.astChildren.isBlock.astChildren.l: @unchecked
        init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        init.methodFullName shouldBe s"Foo.${io.joern.x2cpg.Defines.ConstructorMethodName}:void(int)"
        init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.name()
        init.typeFullName shouldBe "void"
        init.signature shouldBe "void(int)"

        val List(obj: Identifier, initArg: Identifier) = init.argument.l: @unchecked
        obj.name shouldBe "this"
        obj.typeFullName shouldBe "Foo"
        obj.argumentIndex shouldBe 0
        obj.order shouldBe 1
        obj.code shouldBe "this"

        initArg.code shouldBe "x"
        initArg.order shouldBe 2
        initArg.argumentIndex shouldBe 1

      case res => fail(s"Expected Bar constructor but found $res")
    }
  }
}
