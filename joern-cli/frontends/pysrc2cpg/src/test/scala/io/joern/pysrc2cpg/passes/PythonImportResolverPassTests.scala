package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.importresolver.*

import java.io.File

class PythonImportResolverPassTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "__init__.py package imports" should {

    lazy val cpg = code(
      """
        |def greet(name):
        |    return "Hello " + name
        |
        |class Greeter:
        |    pass
        |""".stripMargin,
      Seq("mypkg", "__init__.py").mkString(File.separator)
    ).moreCode(
      """
        |from mypkg import greet, Greeter
        |
        |result = greet("world")
        |g = Greeter()
        |""".stripMargin,
      "app.py"
    )

    "resolve function import from __init__.py" in {
      val resolvedImports = cpg.file(".*app.py").ast.isCall
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .collect { case r: ResolvedMethod => r }
        .l
      resolvedImports.map(_.fullName) should contain(
        Seq("mypkg", "__init__.py:<module>.greet").mkString(File.separator)
      )
    }

    "resolve type import from __init__.py" in {
      val resolvedImports = cpg.file(".*app.py").ast.isCall
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .collect { case r: ResolvedTypeDecl => r }
        .l
      resolvedImports.map(_.fullName) should contain(
        Seq("mypkg", "__init__.py:<module>.Greeter").mkString(File.separator)
      )
    }
  }

  "star import expansion" should {

    lazy val cpg = code(
      """
        |def helper():
        |    return 42
        |
        |class Widget:
        |    pass
        |
        |x = 10
        |""".stripMargin,
      "utils.py"
    ).moreCode(
      """
        |from utils import *
        |
        |result = helper()
        |w = Widget()
        |""".stripMargin,
      "main.py"
    )

    "expand star import to resolve individual members" in {
      val resolvedImports = cpg.file(".*main.py").ast.isCall
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .l

      // Should have resolved imports for helper (method), Widget (type+constructor), and x (member)
      val resolvedMethods = resolvedImports.collect { case r: ResolvedMethod => r }
      val resolvedTypes   = resolvedImports.collect { case r: ResolvedTypeDecl => r }
      val resolvedMembers = resolvedImports.collect { case r: ResolvedMember => r }

      resolvedMethods.map(_.fullName) should contain("utils.py:<module>.helper")
      resolvedTypes.map(_.fullName) should contain("utils.py:<module>.Widget")
      resolvedMembers.map(_.memberName) should contain("x")
    }
  }

  "fallback for unresolvable external module" should {

    lazy val cpg = code(
      """
        |from unknown_external_lib import SomeClass
        |
        |obj = SomeClass()
        |""".stripMargin,
      "consumer.py"
    )

    "create pseudo imports for external modules" in {
      val resolvedImports = cpg.file(".*consumer.py").ast.isCall
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .l
      // For an unresolved import with a capitalized name, we expect UnknownMethod and UnknownTypeDecl
      val unknownMethods = resolvedImports.collect { case u: UnknownMethod => u }
      val unknownTypes   = resolvedImports.collect { case u: UnknownTypeDecl => u }

      unknownMethods should not be empty
      unknownTypes should not be empty
      unknownTypes.head.fullName shouldBe "unknown_external_lib.py:<module>.SomeClass"
    }
  }

}
