package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{
  Binding,
  Call,
  FieldIdentifier,
  Identifier,
  Method,
  MethodParameterIn
}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

class TypeDeclTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with class declaration using unresolved types which are available in imports" should {
    val cpg = code("""
      |package no.such.pkg
      |
      |import android.content.BroadcastReceiver
      |import android.content.Context
      |import android.content.Intent
      |
      |class CustomReceiver : BroadcastReceiver() {
      |    override fun onReceive(context: Context?, intent: Intent?) {}
      |}
      | """.stripMargin)

    "should contain a TYPE_DECL node with the correct TYPE_FULL_NAME property set" in {
      cpg.typeDecl.name("CustomReceiver").inheritsFromTypeFullName.l shouldBe List("android.content.BroadcastReceiver")
    }

    "should contain a TYPE_DECL node with a METHOD node with the correct TYPE_FULL_NAME properties set" in {
      cpg.typeDecl.name("CustomReceiver").method.nameExact("onReceive").parameter.typeFullName.l shouldBe
        List("no.such.pkg.CustomReceiver", "android.content.Context", "android.content.Intent")
    }

    "should contain a TYPE node for the superclass" in {
      cpg.typ.typeDeclFullNameExact("android.content.BroadcastReceiver").size shouldBe 1
    }
  }

  "CPG for code with class declaration with two init blocks" should {
    val cpg = code("""
       |package no.such.pkg
       |class YourNewMostFavoriteNewsletter {
       |    init { println("            𝖘𝖚𝖇𝖘𝖈𝖗𝖎𝖇𝖊 𝖓𝖔𝖜         ") }
       |    init { println("   https://grugq.substack.com/   ") }
       |}
       | """.stripMargin)

    "should contain CALL nodes for the calls inside the init blocks" in {
      cpg.call.code("println.*").size shouldBe 2
    }
  }

  "CPG for code with class declaration and secondary constructor" should {
    val cpg = code("""
       |package no.such.pkg
       |
       |class AClass() {
       |    init { println("inside first init block") }
       |    init { println("inside second init block") }
       |    constructor(p: Int) : this() {
       |        println("secondary ctor called with parameter $x")
       |    }
       |}
       | """.stripMargin)

    "should contain METHOD for the secondary ctor with a call to the primary ctor as the first child of its BLOCK" in {
      val List(secondaryCtor: Method) =
        cpg.method.name("<init>").where(_.parameter.nameExact("p")).l
      val List(firstCallOfSecondaryCtor: Call) =
        secondaryCtor.block.astChildren.collectAll[Call].take(1).l
      firstCallOfSecondaryCtor.methodFullName shouldBe "no.such.pkg.AClass.<init>:void()"
    }
  }

  "CPG for code with class declaration with one member" should {
    val cpg = code("""
        |package mypkg
        |class AClass(var x: String)
        | """.stripMargin)

    "should contain a METHOD node for the constructor of the class" in {
      val List(ctor: Method) = cpg.method.name(".init.*").l
      ctor.fullName shouldBe "mypkg.AClass.<init>:void(java.lang.String)"
      ctor.parameter.size shouldBe 2

      val List(firstCtorParam: MethodParameterIn, secondCtorParam: MethodParameterIn) = ctor.parameter.l
      firstCtorParam.name shouldBe "this"
      firstCtorParam.typeFullName shouldBe "mypkg.AClass"
      secondCtorParam.name shouldBe "x"
      secondCtorParam.typeFullName shouldBe "java.lang.String"

      val List(memberSetCall: Call) = ctor.block.expressionDown.l: @unchecked
      memberSetCall.methodFullName shouldBe Operators.assignment

      val List(memberSetCallLhs: Call, memberSetCallRhs: Identifier) = memberSetCall.argument.l: @unchecked
      memberSetCallLhs.code shouldBe "this.x"
      memberSetCallLhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      memberSetCallRhs.code shouldBe "x"
      memberSetCallRhs.typeFullName shouldBe "java.lang.String"

      val List(_this: Identifier, x: FieldIdentifier) = memberSetCallLhs.argument.l: @unchecked
      _this.code shouldBe "this"
      _this.typeFullName shouldBe "mypkg.AClass"
      _this.dynamicTypeHintFullName shouldBe Seq("mypkg.AClass")
      _this.refsTo.size shouldBe 1
      x.code shouldBe "x"
      x.canonicalName shouldBe "x"
    }
  }

  "CPG for code with data class declaration with one member" should {
    val cpg = code("""
        |package mypkg
        |data class AClass(var x: String)
        | """.stripMargin)

    "should contain a METHOD node for the constructor of the class" in {
      val List(ctor: Method) = cpg.method.name(".init.*").l
      ctor.fullName shouldBe "mypkg.AClass.<init>:void(java.lang.String)"
      ctor.parameter.size shouldBe 2

      val List(firstCtorParam: MethodParameterIn, secondCtorParam: MethodParameterIn) = ctor.parameter.l
      firstCtorParam.name shouldBe "this"
      firstCtorParam.typeFullName shouldBe "mypkg.AClass"
      secondCtorParam.name shouldBe "x"
      secondCtorParam.typeFullName shouldBe "java.lang.String"

      val List(memberSetCall: Call) = ctor.block.expressionDown.l: @unchecked
      memberSetCall.methodFullName shouldBe Operators.assignment

      val List(memberSetCallLhs: Call, memberSetCallRhs: Identifier) = memberSetCall.argument.l: @unchecked
      memberSetCallLhs.code shouldBe "this.x"
      memberSetCallLhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      memberSetCallRhs.code shouldBe "x"
      memberSetCallRhs.typeFullName shouldBe "java.lang.String"

      val List(_this: Identifier, x: FieldIdentifier) = memberSetCallLhs.argument.l: @unchecked
      _this.code shouldBe "this"
      _this.typeFullName shouldBe "mypkg.AClass"
      _this.dynamicTypeHintFullName shouldBe Seq("mypkg.AClass")
      _this.refsTo.size shouldBe 1
      x.code shouldBe "x"
      x.canonicalName shouldBe "x"
    }

    "should contain a METHOD node for `component1`" in {
      val List(componentN: Method) = cpg.method.name(".*component.*").l
      componentN.fullName shouldBe "mypkg.AClass.component1:java.lang.String()"
      componentN.parameter.size shouldBe 1

      val List(p: MethodParameterIn) = componentN.parameter.l
      p.name shouldBe "this"
      p.typeFullName shouldBe "mypkg.AClass"

      val List(returnCall: Call) = componentN.block.expressionDown.isReturn.astChildren.l: @unchecked
      returnCall.methodFullName shouldBe Operators.fieldAccess
      returnCall.code shouldBe "this.x"
      returnCall.typeFullName shouldBe "java.lang.String"

      val List(_this: Identifier, x: FieldIdentifier) = returnCall.argument.l: @unchecked
      _this.code shouldBe "this"
      _this.typeFullName shouldBe "mypkg.AClass"
      _this.dynamicTypeHintFullName shouldBe Seq("mypkg.AClass")
      _this.refsTo.size shouldBe 1
      x.code shouldBe "x"
      x.canonicalName shouldBe "x"
    }
  }

  "CPG for class declaration with single method" should {
    val cpg = code("""
        |package mypkg
        |class AClass {
        |   fun printThis() {
        |     println(this)
        |   }
        |}
        | """.stripMargin)

    "should contain a TYPE_DECL node for its method with an implicit _this_ parameter" in {
      val List(m) = cpg.typeDecl.name("AClass").method.name("printThis").l
      m.parameter.size shouldBe 1
    }

    "should contain an identifier for the usage of _this_ inside the call with one reference" in {
      val List(i) = cpg.identifier.name("this").l
      i.refsTo.size shouldBe 1
    }
  }

  "CPG for simple class" should {
    val cpg = code("""
        |package mypkg
        |
        |import java.lang.Object
        |
        |class Foo: Object {
        |  val z: Int = 1
        |
        |  fun add1(x: Int): Int {
        |    return x + 1
        |  }
        |}
        | """.stripMargin)

    "should contain a TYPE_DECL node for `Foo` with correct props set" in {
      val List(x) = cpg.typeDecl.isExternal(false).name("Foo").l
      x.name shouldBe "Foo"
      x.code shouldBe "Foo"
      x.fullName shouldBe "mypkg.Foo"
      x.inheritsFromTypeFullName shouldBe List("java.lang.Object")
      x.isExternal shouldBe false
      x.lineNumber shouldBe Some(6)
      x.columnNumber shouldBe Some(6)
    }

    "should contain a TYPE_DECL node for `Foo` with two METHOD nodes" in {
      cpg.typeDecl
        .isExternal(false)
        .name("Foo")
        .method
        .fullName
        .toSet shouldBe Set("mypkg.Foo.<init>:void()", "mypkg.Foo.add1:int(int)")
    }

    "should contain a TYPE_DECL node for `Foo` with a correct member node" in {
      val List(m) = cpg.typeDecl.isExternal(false).name("Foo").member.l
      m.name shouldBe "z"
      m.typeFullName shouldBe "int"
    }

    "should contain TYPE_DECL node for the external type `Int`" in {
      val List(x) = cpg.typeDecl.fullNameExact("int").l
      x.name shouldBe "int"
      x.isExternal shouldBe true
      x.inheritsFromTypeFullName shouldBe List()
      x.aliasTypeFullName shouldBe None
      x.filename shouldBe FileTraversal.UNKNOWN
    }
  }

  "CPG for code with user-defined class which has no specific superclasses" should {
    val cpg = code("""
        |package main
        |
        |class AClass
        |
        |fun main() {
        |    val aClass = AClass()
        |    println(aClass.toString())
        |}
        | """.stripMargin)

    "should contain TYPE_DECL node with a value of `java.lang.Object` in its INHERITS_FROM_TYPE_FULL_NAME prop" in {
      val List(x) = cpg.typeDecl.nameExact("AClass").l
      x.inheritsFromTypeFullName shouldBe List("java.lang.Object")
    }
  }

  "class with multiple initializers" ignore {
    val cpg = code("""
        |package baz
        |
        |import kotlin.io.println
        |
        |open class Foo(x: Int)
        |
        |class Bar(x: Int) : Foo(x) {
        |   val method: Int = 1 + 1
        |
        |   init {
        |     println("initBlock1")
        |   }
        |
        |   init {
        |     println("initBlock2")
        |   }
        |}
        | """.stripMargin)

    /*
    "should contain calls from both initializer blocks" in {
      cpg.call.codeExact("println(\"initBlock1\")").size shouldBe 1
      cpg.call.codeExact("println(\"initBlock2\")").size shouldBe 1
    }
     */
  }

  "CPG for code with simple class declaration and usage" should {
    val cpg = code("""
        |package mypkg
        |
        |class Foo {
        |  fun add1(x: Int): Int {
        |    return x + 1
        |  }
        |}
        |
        |fun main(argc: Int): Int {
        |  val x = Foo()
        |  val y = x.add1(argc)
        |  return y
        |}
        |""".stripMargin)

    "should contain a BINDING node for X with the correct props set" in {
      val List(b) = cpg.typeDecl.methodBinding.nameExact("add1").l
      b.name shouldBe "add1"
      b.methodFullName shouldBe "mypkg.Foo.add1:int(int)"
      b.signature shouldBe "int(int)"
    }
  }

  "CPG for code with usage of setter of simple user-defined class" should {
    val cpg = code("""
      |package mypkg
      |
      |class Simple {
      |    var message = "HELLO"
      |}
      |
      |fun action(msg: String): String {
      |    val simple = Simple()
      |    //println("before: " + simple.message)
      |    simple.message = msg
      |    //println("after: " + simple.message)
      |    println(simple.message)
      |    return simple.message
      |}
      |
      |fun main() {
      |    action("HELLO, WORLD")
      |}
      | """.stripMargin)

    "should contain a CALL node for the field access inside the assignment with the correct properties set" in {
      val List(c) = cpg.call.methodFullName(Operators.assignment).argument(1).isCall.code("simple.*").l
      c.code shouldBe "simple.message"
      c.methodFullName shouldBe Operators.fieldAccess
    }
  }

  "CPG for code with class defined inside user-defined function" should {
    val cpg = code("""
       |package mypkg
       |
       |fun doSomething(x: String): String {
       |    class AClass(val m: String)
       |    val aClass = AClass(x)
       |    return aClass.m
       |}
       |
       |fun main() {
       |    println(doSomething("AMESSAGE"))
       |}
       | """.stripMargin)

    "should contain a TYPE_DECL node for the class with the correct props set" in {
      val List(td) = cpg.typeDecl.nameExact("AClass").l
      td.isExternal shouldBe false
      td.fullName shouldBe "mypkg.doSomething.AClass"
    }
  }

  "CPG for code with nested class definition" should {
    val cpg = code("""package no.such.pkg
        |import another.made.up.pkg.SomeClass
        |class AClass { class AnotherClass : SomeClass() }
        |""".stripMargin)

    "should contain a TYPE_DECL node for the nested class" in {
      val List(td) = cpg.typeDecl.nameExact("AnotherClass").l
      td.inheritsFromTypeFullName shouldBe List("another.made.up.pkg.SomeClass")
    }
  }

  "CPG for code with class with call to fn member initializer" should {
    val cpg = code("""
        |package mypkg
        |fun addB(a: String): String {
        |    return a + "b"
        |}
        |class MyClass(val x: String) {
        |    var m: String = addB(x)
        |    fun printM() = println(this.m)
        |}
        |""".stripMargin)

    "should contain CALL nodes for the member initializer" in {
      val List(lhs: Call, rhs: Call) = cpg.call.code("this.*addB.*").argument.l: @unchecked
      lhs.code shouldBe "this.m"
      rhs.code shouldBe "addB(x)"
      rhs.methodFullName shouldBe "mypkg.addB:java.lang.String(java.lang.String)"
    }
  }

  "CPG for code with a simple interface" should {
    val cpg = code("""
        |package mypkg
        |interface AnInterface {
        |    fun doSomething(p: String)
        |}
        |""".stripMargin)

    "should contain a TYPE_DECL node with the correct properties set" in {
      val List(td) = cpg.typeDecl.nameExact("AnInterface").l
      td.fullName shouldBe "mypkg.AnInterface"
      td.inheritsFromTypeFullName shouldBe Seq("java.lang.Object")
      cpg.typeDecl.isAbstract.head shouldBe td
      td.method.isAbstract.fullName.l shouldBe List("mypkg.AnInterface.doSomething:void(java.lang.String)")
    }
  }

  "CPG for code with a simple functional interface" should {
    val cpg = code("""
        |package mypkg
        |fun interface AFunInterface {
        |    fun doSomething(p: String)
        |}
        |""".stripMargin)

    "should contain a TYPE_DECL node with the correct properties set" in {
      val List(td) = cpg.typeDecl.nameExact("AFunInterface").l
      td.fullName shouldBe "mypkg.AFunInterface"
      td.inheritsFromTypeFullName shouldBe Seq("java.lang.Object")
      cpg.typeDecl.isAbstract.head shouldBe td
      td.method.isAbstract.fullName.l shouldBe List("mypkg.AFunInterface.doSomething:void(java.lang.String)")
    }
  }

  "CPG for code with secondary ctor calling super" should {
    val cpg = code("""
      |package mypkg
      |open class PClass(val x: Int) {}
      |class QClass : PClass {
      |    constructor(x: Int, y: Int) : super(x)
      |}
      |""".stripMargin)

    "should contain a METHOD node for the secondary ctor with a call to the primary ctor as the first child of its BLOCK" in {
      val List(secondaryCtor: Method) =
        cpg.method.name("<init>").where(_.parameter.nameExact("y")).l
      val List(firstCallOfSecondaryCtor: Call) =
        secondaryCtor.block.astChildren.collectAll[Call].take(1).l
      firstCallOfSecondaryCtor.methodFullName shouldBe "mypkg.QClass.<init>:void()"
    }
  }
}
