package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ConstructorTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for a class declaration with an implicit constructor" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class Foo {
        |}
        |""".stripMargin)

    "should contain a TYPE_DECL node" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").size shouldBe 1
    }

    "should contain a METHOD node for the constructor with the correct fullname set" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.fullName.l shouldBe
        List("mypkg.Foo.<init>:void()")
    }

    "should contain a METHOD node for the constructor with the correct number of parameters" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.parameter.size shouldBe 0
    }
  }

  "CPG for a class declaration with an implicit constructor with parameters" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class Foo(bar: String) {
        |}
        |""".stripMargin)

    "should contain a TYPE_DECL node" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").size shouldBe 1
    }

    "should contain a METHOD node for the constructor with the correct fullname set" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.fullName.l shouldBe List("mypkg.Foo.<init>:void(java.lang.String)")
    }

    "should contain a METHOD node for the constructor with the correct number of parameters" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.parameter.size shouldBe 1
    }
  }

  "CPG for a class declaration with an explicit constructor with parameters" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class Foo constructor(bar: String) {
        |}
        |""".stripMargin)

    "should contain a TYPE_DECL node" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").size shouldBe 1
    }

    "should contain a METHOD node for the constructor with the correct fullname set" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.fullName.l shouldBe List("mypkg.Foo.<init>:void(java.lang.String)")
    }

    "should contain a METHOD node for the constructor with the correct number of parameters" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.parameter.size shouldBe 1
    }
  }

  "CPG for a class declaration with secondary constructor" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class Foo(foo: String) {
        |    var bar: Int = 0
        |    constructor(foo:String, bar: Int): this(foo) {
        |        this.bar = bar
        |    }
        |}
        |""".stripMargin)

    "should contain a TYPE_DECL node" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").size shouldBe 1
    }

    "should contain METHOD nodes for the primary and secondary constructors with the correct fullnames set" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.fullName.l shouldBe List(
        "mypkg.Foo.<init>:void(java.lang.String)",
        "mypkg.Foo.<init>:void(java.lang.String,java.lang.Integer)"
      )
    }

    "should contain a METHOD node for the primary constructor with the correct number of parameters" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.take(1).parameter.size shouldBe 1
    }

    "should contain a METHOD node for the secondary constructor with the correct number of parameters" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.drop(1).take(1).parameter.size shouldBe 2
    }

    "should contain a METHOD node for the primary constructor with properties set correctly" in {
      val List(td) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.take(1).l
      td.lineNumber shouldBe Some(3)
      td.columnNumber shouldBe Some(9)
      td.methodReturn.code shouldBe "RET"
      td.methodReturn.lineNumber shouldBe Some(3)
      td.methodReturn.columnNumber shouldBe Some(9)
    }

    "should contain a METHOD node for the secondary constructor with properties set correctly" in {
      val List(td) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.drop(1).take(1).l
      td.fullName shouldBe "mypkg.Foo.<init>:void(java.lang.String,java.lang.Integer)"
      td.lineNumber shouldBe Some(5)
      td.columnNumber shouldBe Some(4)
      td.methodReturn.code shouldBe "RET"
      td.methodReturn.lineNumber shouldBe Some(5)
      td.methodReturn.columnNumber shouldBe Some(4)
    }
  }
}
