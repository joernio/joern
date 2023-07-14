package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class ModuleTests extends RubyCode2CpgFixture {

  "Simple module checks" should {
    val cpg = code("""
        |module MyNamespace
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
      x.fullName shouldBe s"Test0.rb::program.MyNamespace"
    }

    "Respective dummy Method in place" in {
      cpg.method("MyNamespace").l.size shouldBe 1
      val List(x) = cpg.method("MyNamespace").l
      x.fullName shouldBe s"Test0.rb::program.MyNamespace"
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
      cpg.method("MyNamespaceChild").l.size shouldBe 1
      val List(x) = cpg.method("MyNamespaceChild").l
      x.fullName shouldBe s"Test0.rb::program.MyNamespaceParent.MyNamespaceChild"
    }

    "Respective dummy TypeDecl in place" in {
      cpg.typeDecl("MyNamespaceChild").l.size shouldBe 1
      val List(x) = cpg.typeDecl("MyNamespaceChild").l
      x.fullName shouldBe s"Test0.rb::program.MyNamespaceParent.MyNamespaceChild"
    }
  }

  //  "Module namespace block strcuture checks" should {
  //    val cpg = code("""
  //        |module MyNamespace
  //        |  class MyClass
  //        |    def method1
  //        |      puts "Method 1"
  //        |    end
  //        |  end
  //        |
  //        |  MY_CONSTANT = 42
  //        |
  //        |  def self.outerMethodOne
  //        |    puts "outer method one"
  //        |  end
  //        |
  //        |  def self.outerMethodTwo
  //        |    puts "outer method two"
  //        |  end
  //        |end
  //        |
  //        |# Accessing classes and constants within the namespace
  //        |obj1 = MyNamespace::MyClass.new
  //        |obj1.method1
  //        |
  //        |puts MyNamespace::MY_CONSTANT
  //        |
  //        |MyNamespace.outerMethodOne
  //        |MyNamespace::outerMethodTwo
  //        |""".stripMargin)
  //
  //    "Check namespace basic block structure" in {
  //      cpg.namespaceBlock
  //        .nameNot(NamespaceTraversal.globalNamespaceName)
  //        .filenameNot(FileTraversal.UNKNOWN)
  //        .l
  //        .size shouldBe 1
  //    }
  //  }
}
