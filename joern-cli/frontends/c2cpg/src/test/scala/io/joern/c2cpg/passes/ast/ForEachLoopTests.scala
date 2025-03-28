package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.Block
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
      checkForEachForLoop(loopBlock, "char*", "char*[]")
    }
  }

  "foreach loops over native arrays" should {
    val cpg = code("""
     |class Foo {
     |  public:
     |    static void sink(const char* s) {}
     |
     |    static void foo(const char* items) {
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
      checkForEachForLoop(loopBlock, "char*", "char*")
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
      checkForEachForLoop(loopBlock, "std.string&", "std.vector&")
    }
  }

  private def checkForEachForLoop(node: Block, expectedItemType: String, expectedCollectionType: String): Unit = {
    val List(localIterator) = node.astChildren.isLocal.nameExact("<iterator>0").l
    localIterator.code shouldBe "<iterator>0"
    localIterator.typeFullName shouldBe Defines.Iterator

    val List(localI) = node.astChildren.isLocal.nameExact("item").l
    localI.code shouldBe "item"
    localI.typeFullName shouldBe expectedItemType

    val List(iteratorAssignment) =
      node.astChildren.isCall.codeExact("<iterator>0 = items.iterator()").l
    iteratorAssignment.name shouldBe Operators.assignment

    val List(iteratorAssignmentLhs) = iteratorAssignment.astChildren.isIdentifier.l
    iteratorAssignmentLhs.name shouldBe "<iterator>0"
    iteratorAssignmentLhs.order shouldBe 1
    iteratorAssignmentLhs.argumentIndex shouldBe 1
    iteratorAssignmentLhs.typeFullName shouldBe Defines.Iterator

    val List(iteratorAssignmentRhs) = iteratorAssignment.astChildren.isCall.l
    iteratorAssignmentRhs.code shouldBe "items.iterator()"
    iteratorAssignmentRhs.order shouldBe 2
    iteratorAssignmentRhs.argumentIndex shouldBe 2
    iteratorAssignmentRhs.name shouldBe "iterator"
    iteratorAssignmentRhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}.iterator:std.iterator()"
    iteratorAssignmentRhs.signature shouldBe "std.iterator()"
    iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    val List(iteratorCallReceiver) = iteratorAssignmentRhs.receiver.isIdentifier.l
    iteratorCallReceiver.name shouldBe "items"
    iteratorCallReceiver.argumentIndex shouldBe 0
    iteratorAssignmentRhs.argument(0) shouldBe iteratorCallReceiver
    iteratorCallReceiver.typeFullName shouldBe expectedCollectionType

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
    loopNextCall.methodFullName shouldBe s"${Defines.Iterator}.next:ANY()"
    loopNextCall.typeFullName shouldBe "ANY"
    loopNextCall.receiver.isIdentifier.typeFullName.l shouldBe List(Defines.Iterator)

    val List(sinkCall) = whileLoopBlock.astChildren.isBlock.astChildren.isCall.codeExact("sink(item)").l
    sinkCall.name shouldBe "sink"
  }

}
