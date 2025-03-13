package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Block
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.semanticcpg.language.*

class ForEachLoopTests extends AstC2CpgSuite(fileSuffix = FileDefaults.CppExt) {

  "foreach loops over native array initialization expressions" should {
    val cpg = code("""
       |class Foo {
       |  public:
       |    static void sink(const char* s) {}
       |
       |    static void foo() {
       |        const char* items[] = {"a", "b", "c"};
       |        for (const char* item : items) {
       |            sink(item);
       |        }
       |    }
       |}""".stripMargin)

    "be correct" in {
      val List(method)      = cpg.method.nameExact("foo").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      cpg.controlStructure.isWhile.code.l shouldBe List("for (const char* item:items)")
      checkForInOrOf(loopBlock, "char*")
    }
  }

  "foreach loops over native arrays" should {
    val cpg = code("""
     |class Foo {
     |  public:
     |    static void sink(const char* s) {}
     |
     |    static void foo(const char* items[]) {
     |      for (const char* item : items) {
     |        sink(item);
     |      }
     |    }
     |}""".stripMargin)

    "be correct" in {
      val List(method)      = cpg.method.nameExact("foo").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      cpg.controlStructure.isWhile.code.l shouldBe List("for (const char* item:items)")
      checkForInOrOf(loopBlock, "char*")
    }
  }

  "foreach loops over collections" should {
    val cpg = code("""
        |class Foo {
        |  public:
        |    static void sink(const std::string& s) {}
        |
        |    static void foo(const std::vector<std::string>& items) {
        |      for (const std::string& item : items) {
        |        sink(item);
        |      }
        |    }
        |}""".stripMargin)

    "be correct" in {
      val List(method)      = cpg.method.nameExact("foo").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      cpg.controlStructure.isWhile.code.l shouldBe List("for (const std::string& item:items)")
      checkForInOrOf(loopBlock, "std.string&")
    }
  }

  private def checkForInOrOf(node: Block, expectedItemType: String): Unit = {
    val List(localIterator) = node.astChildren.isLocal.nameExact("<iterator>0").l
    localIterator.code shouldBe "<iterator>0"
    localIterator.typeFullName shouldBe Defines.Iterator

    val List(localI) = node.astChildren.isLocal.nameExact("item").l
    localI.code shouldBe "item"
    localI.typeFullName shouldBe expectedItemType

    val List(iteratorAssignment) =
      node.astChildren.isCall.codeExact("<iterator>0 = <operator>.iterator(items)").l
    iteratorAssignment.name shouldBe Operators.assignment

    val List(iteratorAssignmentLhs) = iteratorAssignment.astChildren.isIdentifier.l
    iteratorAssignmentLhs.name shouldBe "<iterator>0"
    iteratorAssignmentLhs.order shouldBe 1
    iteratorAssignmentLhs.argumentIndex shouldBe 1
    iteratorAssignmentLhs.typeFullName shouldBe Defines.Iterator

    val List(iteratorAssignmentRhs) = iteratorAssignment.astChildren.isCall.l
    iteratorAssignmentRhs.code shouldBe "<operator>.iterator(items)"
    iteratorAssignmentRhs.order shouldBe 2
    iteratorAssignmentRhs.argumentIndex shouldBe 2
    iteratorAssignmentRhs.name shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.methodFullName shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val objectKeysCallArg = iteratorAssignmentRhs.argument(1).asInstanceOf[Identifier]
    objectKeysCallArg.name shouldBe "items"
    objectKeysCallArg.order shouldBe 1

    val List(varI) = node.astChildren.isIdentifier.nameExact("item").l
    varI.code shouldBe "item"
    varI.typeFullName shouldBe expectedItemType

    val List(loop) = node.astChildren.isControlStructure.l
    loop.controlStructureType shouldBe ControlStructureTypes.WHILE

    val List(loopTestCall) = loop.astChildren.isCall.codeExact("<iterator>0.hasNext()").l
    loopTestCall.name shouldBe "hasNext"
    loopTestCall.methodFullName shouldBe s"${Defines.Iterator}.hasNext:bool()"
    loopTestCall.order shouldBe 1
    loopTestCall.receiver.isIdentifier.typeFullName.l shouldBe List(Defines.Iterator)

    val List(whileLoopBlock) = loop.astChildren.isBlock.l
    whileLoopBlock.order shouldBe 2

    val List(loopVarAssignmentCall) = whileLoopBlock.astChildren.isCall.codeExact("item = <iterator>0.next()").l
    loopVarAssignmentCall.name shouldBe Operators.assignment
    loopVarAssignmentCall.order shouldBe 1

    val List(loopNextCall) = loopVarAssignmentCall.argument.isCall.codeExact("<iterator>0.next()").l
    loopNextCall.name shouldBe "next"
    loopNextCall.methodFullName shouldBe s"${Defines.Iterator}.next"
    loopNextCall.receiver.isIdentifier.typeFullName.l shouldBe List(Defines.Iterator)

    val List(sinkCall) = whileLoopBlock.astChildren.isBlock.astChildren.isCall.codeExact("sink(item)").l
    sinkCall.name shouldBe "sink"
  }

}
