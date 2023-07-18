package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.Defines as XDefines
import io.shiftleft.codepropertygraph.generated.Operators
class ModuleTests extends RubyCode2CpgFixture {

  "Simple module checks" should {
    val cpg = code("""
        |module MyNamespace
        | MY_CONSTANT = 20
        |end
        |""".stripMargin)
    "Check namespace basic block structure" in {
      cpg.namespaceBlock
        .nameNot(NamespaceTraversal.globalNamespaceName)
        .filenameNot(FileTraversal.UNKNOWN)
        .l
        .size shouldBe 1
      val List(x) = cpg.namespaceBlock
        .nameNot(NamespaceTraversal.globalNamespaceName)
        .filenameNot(FileTraversal.UNKNOWN)
        .l
      x.name shouldBe "MyNamespace"
      x.fullName shouldBe "Test0.rb::program.MyNamespace"
    }

    "Respective dummy Method in place" in {
      cpg.method(XDefines.StaticInitMethodName).l.size shouldBe 1
      val List(x) = cpg.method(XDefines.StaticInitMethodName).l
      x.fullName shouldBe s"Test0.rb::program.MyNamespace.${XDefines.StaticInitMethodName}"
    }

    "Respective dummy TypeDecl in place" in {
      cpg.typeDecl("MyNamespace").l.size shouldBe 1
      val List(x) = cpg.typeDecl("MyNamespace").l
      x.fullName shouldBe s"Test0.rb::program.MyNamespace"
    }
  }

  "Hierarchical module checks" should {
    val cpg = code("""
        |module MyNamespaceParent
        | module MyNamespaceChild
        |  SOME_CONSTATN = 10
        | end
        |end
        |""".stripMargin)
    "Check namespace basic block structure" in {
      cpg.namespaceBlock
        .nameNot(NamespaceTraversal.globalNamespaceName)
        .filenameNot(FileTraversal.UNKNOWN)
        .l
        .size shouldBe 2
      val List(x, x1) = cpg.namespaceBlock
        .nameNot(NamespaceTraversal.globalNamespaceName)
        .filenameNot(FileTraversal.UNKNOWN)
        .l
      x.name shouldBe "MyNamespaceParent"
      x.fullName shouldBe s"Test0.rb::program.MyNamespaceParent"

      x1.name shouldBe "MyNamespaceChild"
      x1.fullName shouldBe s"Test0.rb::program.MyNamespaceParent.MyNamespaceChild"
    }

    "Respective dummy Method in place" in {
      cpg.method(XDefines.StaticInitMethodName).l.size shouldBe 2
      cpg.method(XDefines.StaticInitMethodName).fullName.l shouldBe List(
        s"Test0.rb::program.MyNamespaceParent.${XDefines.StaticInitMethodName}",
        s"Test0.rb::program.MyNamespaceParent.MyNamespaceChild.${XDefines.StaticInitMethodName}"
      )
    }

    "Respective dummy TypeDecl in place" in {
      cpg.typeDecl("MyNamespaceChild").l.size shouldBe 1
      val List(x) = cpg.typeDecl("MyNamespaceChild").l
      x.fullName shouldBe s"Test0.rb::program.MyNamespaceParent.MyNamespaceChild"
    }
  }

  "Module Internal structure checks with member variable" should {
    val cpg = code("""
            |module MyNamespace
            |  @@plays = 0
            |  class MyClass
            |    def method1
            |      puts "Method 1"
            |    end
            |  end
            |end
            |""".stripMargin)
    "Class structure in plcae" in {
      cpg.typeDecl("MyClass").l.size shouldBe 1
      val List(x) = cpg.typeDecl("MyClass").l
      x.fullName shouldBe s"Test0.rb::program.MyNamespace.MyClass"
    }

    "Class Method structure in place" in {
      cpg.method("method1").l.size shouldBe 1
      val List(x) = cpg.method("method1").l
      x.fullName shouldBe s"Test0.rb::program.MyNamespace.MyClass.method1"
    }

    "member variables structure in place" in {
      val List(classInit) = cpg.method(XDefines.StaticInitMethodName).l
      classInit.fullName shouldBe s"Test0.rb::program.MyNamespace.${XDefines.StaticInitMethodName}"
      val List(playsDef) = classInit.call.nameExact(Operators.fieldAccess).fieldAccess.l
      playsDef.fieldIdentifier.canonicalName.headOption shouldBe Option("plays")

      val List(myclassTd) = cpg.typeDecl("MyNamespace").l
      val List(plays)     = myclassTd.member.l
      plays.name shouldBe "plays"
      cpg.fieldAccess.fieldIdentifier.canonicalName.l shouldBe List("plays")
    }

    "Module Constant strucutre in place" ignore {
      cpg.identifier("MY_CONSTANT").l.size shouldBe 1
      val List(x)      = cpg.typeDecl("MyClass").l
      val List(member) = x.member.l
      member.name shouldBe "MY_CONSTANT"
    }
  }

  "Module internal structure checks with Constant defined in moudle" ignore {
    val cpg = code("""
        |module MyNamespace
        |  MY_CONSTANT = 100
        |end
        |""".stripMargin)
    "member variables structure in place" in {
      val List(classInit) = cpg.method(XDefines.StaticInitMethodName).l
      classInit.fullName shouldBe s"Test0.rb::program.MyNamespace.${XDefines.StaticInitMethodName}"
      val List(myconstant) = classInit.call.nameExact(Operators.fieldAccess).fieldAccess.l
      myconstant.fieldIdentifier.canonicalName.headOption shouldBe Option("MY_CONSTANT")

      val List(myclassTd)  = cpg.typeDecl("MyNamespace").l
      val List(myConstant) = myclassTd.member.l
      myConstant.name shouldBe "MY_CONSTANT"
      cpg.fieldAccess.fieldIdentifier.canonicalName.l shouldBe List("MY_CONSTANT")
    }
  }

}
