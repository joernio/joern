package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.importresolver.*

import java.io.File

class PythonImportResolverPassTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "importing from a package with __init__.py" should {
    lazy val cpg = code(
      """
        |def helper():
        |    return 42
        |
        |class Config:
        |    DEBUG = True
        |""".stripMargin,
      Seq("mypkg", "__init__.py").mkString(File.separator)
    ).moreCode(
      """
        |from mypkg import helper, Config
        |
        |x = helper()
        |c = Config()
        |""".stripMargin,
      "app.py"
    )

    "resolve the function from __init__.py" in {
      val resolvedMethods = cpg.call
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .collect { case r: ResolvedMethod => r }
        .l
      resolvedMethods.map(_.fullName) should contain(
        Seq("mypkg", "__init__.py:<module>.helper").mkString(File.separator)
      )
    }

    "resolve the type from __init__.py" in {
      val resolvedTypes = cpg.call
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .collect { case r: ResolvedTypeDecl => r }
        .l
      resolvedTypes.map(_.fullName) should contain(
        Seq("mypkg", "__init__.py:<module>.Config").mkString(File.separator)
      )
    }
  }

  "star import from a local module" should {
    lazy val cpg = code(
      """
        |def greet(name):
        |    return f"Hello, {name}"
        |
        |def farewell(name):
        |    return f"Goodbye, {name}"
        |
        |class Greeter:
        |    pass
        |
        |GREETING_CONST = "hi"
        |""".stripMargin,
      "utils.py"
    ).moreCode(
      """
        |from utils import *
        |
        |x = greet("world")
        |""".stripMargin,
      "app.py"
    )

    "expand to all top-level functions from the module" in {
      val resolvedMethods = cpg.file(".*app.py").ast.isCall
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .collect { case r: ResolvedMethod => r }
        .l
      resolvedMethods.map(_.fullName) should contain("utils.py:<module>.greet")
      resolvedMethods.map(_.fullName) should contain("utils.py:<module>.farewell")
    }

    "expand to all top-level classes from the module" in {
      val resolvedTypes = cpg.file(".*app.py").ast.isCall
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .collect { case r: ResolvedTypeDecl => r }
        .l
      resolvedTypes.map(_.fullName) should contain("utils.py:<module>.Greeter")
    }

    "expand to module-level variables from the module" in {
      val resolvedMembers = cpg.file(".*app.py").ast.isCall
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .collect { case r: ResolvedMember => r }
        .l
      resolvedMembers.map(_.memberName) should contain("GREETING_CONST")
    }
  }

  "star import from a package __init__.py" should {
    lazy val cpg = code(
      """
        |def pkg_func():
        |    return 1
        |
        |class PkgClass:
        |    pass
        |""".stripMargin,
      Seq("mypkg", "__init__.py").mkString(File.separator)
    ).moreCode(
      """
        |from mypkg import *
        |
        |x = pkg_func()
        |""".stripMargin,
      "app.py"
    )

    "expand star import to functions from __init__.py" in {
      val resolvedMethods = cpg.file(".*app.py").ast.isCall
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .collect { case r: ResolvedMethod => r }
        .l
      resolvedMethods.map(_.fullName) should contain(
        Seq("mypkg", "__init__.py:<module>.pkg_func").mkString(File.separator)
      )
    }

    "expand star import to classes from __init__.py" in {
      val resolvedTypes = cpg.file(".*app.py").ast.isCall
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .collect { case r: ResolvedTypeDecl => r }
        .l
      resolvedTypes.map(_.fullName) should contain(
        Seq("mypkg", "__init__.py:<module>.PkgClass").mkString(File.separator)
      )
    }
  }

  "star import from an external module" should {
    lazy val cpg = code(
      """
        |from unknown_external import *
        |""".stripMargin,
      "app.py"
    )

    "fall back to pseudo import when module is not in source tree" in {
      val imports = cpg.file(".*app.py").ast.isCall
        .where(_.referencedImports)
        .tag
        ._toEvaluatedImport
        .l
      imports should not be empty
    }
  }

}
