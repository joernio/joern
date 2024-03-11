package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.rubysrc2cpg.RubySrc2Cpg
import io.joern.rubysrc2cpg.Config
import scala.util.{Success, Failure}
import org.scalatest.Inspectors

class ImportTests extends RubyCode2CpgFixture with Inspectors {

  "`require 'test'` is a CALL node with an IMPORT node pointing to it" in {
    val cpg = code("""
                     |require 'test'
                     |""".stripMargin)
    val List(importNode) = cpg.imports.l
    importNode.importedEntity shouldBe Some("test")
    importNode.importedAs shouldBe Some("test")
    val List(call) = importNode.call.l
    call.callee.name.l shouldBe List("require")
    call.argument.code.l shouldBe List("'test'")
  }

  "`begin require 'test' rescue LoadError end` has a CALL node with an IMPORT node pointing to it" in {
    val cpg = code("""
                     |begin 
                     |  require 'test'
                     |rescue LoadError
                     |end
                     |""".stripMargin)
    val List(importNode) = cpg.imports.l
    importNode.importedEntity shouldBe Some("test")
    importNode.importedAs shouldBe Some("test")
    val List(call) = importNode.call.l
    call.callee.name.l shouldBe List("require")
    call.argument.code.l shouldBe List("'test'")
  }

  "Ambiguous class resolves to required method" in {
    forAll(List("t2", "t3")) { path =>
      val cpg = code(
        s"""
      | require '${path}'
      | Test.new
      |""".stripMargin,
        "t1.rb"
      ).moreCode(
        """
      | class Test
      | end
      |""".stripMargin,
        "t2.rb"
      ).moreCode(
        """
      | class Test
      | end
      |""".stripMargin,
        "t3.rb"
      )

      val List(newCall) =
        cpg.method.name(":program").filename("t1.rb").ast.isCall.methodFullName(".*:<init>").methodFullName.l
      newCall should startWith(s"${path}.rb:")
    }
  }

  "Ambiguous methods resolves to included method" in {
    forAll(List("A", "B")) { moduleName =>
      val cpg = code(s"""
      | module A
      |   def foo
      |   end
      | end
      |
      | module B
      |   def foo
      |   end
      | end
      |
      | module C
      |   include ${moduleName}
      |   def bar
      |     foo()
      |   end
      | end
      |""".stripMargin)

      val List(methodName) =
        cpg.method.name("bar").ast.isCall.methodFullName(".*::program\\.(A|B):foo").methodFullName.l
      methodName should endWith(s"${moduleName}:foo")
    }
  }
}
