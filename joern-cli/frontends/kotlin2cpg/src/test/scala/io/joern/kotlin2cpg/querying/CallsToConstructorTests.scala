package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Literal, Local}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class CallsToConstructorTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with call to constructor of Java stdlib object inside declaration" should {
    val cpg = code("""
        |package mypkg
        |import java.io.File
        |fun main() {
        |    val f = File("/tmp/myfile.txt")
        |    f.writeText("Hello, world!")
        |}
        |""".stripMargin)

    "should contain a correct lowered representation" in {
      val List(l: Local, allocAssignment: Call, init: Call, _: Call) =
        cpg.method.nameExact("main").block.astChildren.l: @unchecked
      l.name shouldBe "f"
      l.typeFullName shouldBe "java.io.File"

      allocAssignment.signature shouldBe ""
      allocAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(assignmentLhs: Identifier, assignmentRhs: Call) = allocAssignment.argument.l: @unchecked
      assignmentLhs.argumentIndex shouldBe 1
      assignmentLhs.code shouldBe "f"
      assignmentLhs.name shouldBe "f"
      assignmentLhs.lineNumber shouldBe Some(5)
      assignmentLhs.columnNumber shouldBe Some(8)
      l.referencingIdentifiers.id.l.contains(assignmentLhs.id) shouldBe true
      assignmentRhs.typeFullName shouldBe "java.io.File"
      assignmentRhs.code shouldBe Operators.alloc
      assignmentRhs.methodFullName shouldBe Operators.alloc
      assignmentRhs.argumentIndex shouldBe 2

      init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
      init.code shouldBe "File(\"/tmp/myfile.txt\")"
      init.methodFullName shouldBe "java.io.File.<init>:void(java.lang.String)"
      init.signature shouldBe "void(java.lang.String)"
      init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      init.typeFullName shouldBe "void"

      val List(firstInitArg: Identifier, secondInitArg: Literal) = init.argument.l: @unchecked
      firstInitArg.name shouldBe "f"
      firstInitArg.refsTo.size shouldBe 1
      secondInitArg.code shouldBe "\"/tmp/myfile.txt\""
    }
  }

  "CPG for code with call to constructor of Java stdlib object inside QE" should {
    val cpg = code("""
        |package mypkg
        |
        |import java.io.File
        |
        |fun main() {
        |     File("/tmp/myfile.txt").writeText("Hello, world!")
        |}
        |""".stripMargin)

    "should contain a correct lowered representation" in {
      val List(qeCall) = cpg.call.methodFullName(".*writeText.*").l

      val List(callLhs: Block, callRhs: Literal) = qeCall.argument.l: @unchecked
      callRhs.argumentIndex shouldBe 1

      val loweredBlock = callLhs
      loweredBlock.typeFullName shouldBe "java.io.File"
      loweredBlock.code shouldBe ""
      loweredBlock.argumentIndex shouldBe 0

      val List(firstBlockChild: Local) = loweredBlock.astChildren.take(1).l: @unchecked
      firstBlockChild.name shouldBe "tmp_1"
      firstBlockChild.typeFullName shouldBe "java.io.File"

      val List(secondBlockChild: Call) = loweredBlock.astChildren.slice(1, 2).l: @unchecked
      val allocAssignment              = secondBlockChild
      val List(allocAssignmentLhs: Identifier, allocAssignmentRhs: Call) = allocAssignment.argument.l: @unchecked
      allocAssignmentLhs.code shouldBe "tmp_1"
      allocAssignmentLhs.typeFullName shouldBe "java.io.File"
      firstBlockChild.referencingIdentifiers.id.l.contains(allocAssignmentLhs.id) shouldBe true

      allocAssignmentRhs.code shouldBe "alloc"
      allocAssignmentRhs.signature shouldBe ""
      allocAssignmentRhs.methodFullName shouldBe "<operator>.alloc"
      allocAssignmentRhs.typeFullName shouldBe "java.io.File"
      allocAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      allocAssignmentRhs.argumentIndex shouldBe 2

      val List(thirdBlockChild: Call) = loweredBlock.astChildren.slice(2, 3).l: @unchecked
      val initCall                    = thirdBlockChild
      initCall.code shouldBe "File(\"/tmp/myfile.txt\")"
      initCall.signature shouldBe "void(java.lang.String)"
      initCall.methodFullName shouldBe "java.io.File.<init>:void(java.lang.String)"
      initCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(initCallLhs: Identifier, initCallRhs: Literal) = initCall.argument.l: @unchecked
      initCallLhs.code shouldBe "tmp_1"
      initCallLhs.typeFullName shouldBe "java.io.File"
      initCallLhs.argumentIndex shouldBe 0
      firstBlockChild.referencingIdentifiers.id.l.contains(initCallLhs.id) shouldBe true

      initCallRhs.code shouldBe "\"/tmp/myfile.txt\""
      initCallRhs.typeFullName shouldBe "java.lang.String"
      initCallRhs.argumentIndex shouldBe 1

      val List(fourthBlockChild: Identifier) = loweredBlock.astChildren.slice(3, 4).l: @unchecked
      fourthBlockChild.code shouldBe "tmp_1"
      fourthBlockChild.typeFullName shouldBe "java.io.File"
      firstBlockChild.referencingIdentifiers.id.l.contains(fourthBlockChild.id) shouldBe true
    }
  }

  "CPG for code with call to simple constructor of user-defined class" should {
    val cpg = code("""
        |package mypkg
        |
        |class AClass(val x: String) {
        |    fun printX() {
        |        println(x)
        |    }
        |}
        |
        |fun main() {
        |    val a = AClass("AMESSAGE")
        |    a.printX()
        |}
        |""".stripMargin)

    "should contain a correct lowered representation" in {
      val List(l: Local, allocAssignment: Call, init: Call, _: Call) =
        cpg.method.nameExact("main").block.astChildren.l: @unchecked
      l.name shouldBe "a"
      l.typeFullName shouldBe "mypkg.AClass"

      allocAssignment.signature shouldBe ""
      allocAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(assignmentLhs: Identifier, assignmentRhs: Call) = allocAssignment.argument.l: @unchecked
      assignmentLhs.argumentIndex shouldBe 1
      assignmentLhs.code shouldBe "a"
      assignmentLhs.name shouldBe "a"
      assignmentLhs.lineNumber shouldBe Some(11)
      assignmentLhs.columnNumber shouldBe Some(8)
      l.referencingIdentifiers.id.l.contains(assignmentLhs.id) shouldBe true
      assignmentRhs.typeFullName shouldBe "mypkg.AClass"
      assignmentRhs.code shouldBe Operators.alloc
      assignmentRhs.methodFullName shouldBe Operators.alloc
      assignmentRhs.argumentIndex shouldBe 2

      init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
      init.code shouldBe "AClass(\"AMESSAGE\")"
      init.methodFullName shouldBe "mypkg.AClass.<init>:void(java.lang.String)"
      init.signature shouldBe "void(java.lang.String)"
      init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      init.typeFullName shouldBe "void"

      val List(firstInitArg: Identifier, secondInitArg: Literal) = init.argument.l: @unchecked
      firstInitArg.name shouldBe "a"
      firstInitArg.refsTo.size shouldBe 1
      secondInitArg.code shouldBe "\"AMESSAGE\""
    }
  }

  "CPG for code with fieldAccess call on ctor" should {
    lazy val cpg = code("""
      |package mypkg
      |fun sink(x: String) = println(x)
      |class AClass(val x: String)
      |fun f1(p: String) {
      |    sink(AClass(p).x)
      |}
      |""".stripMargin)

    "contain a CALL node with the correct METHOD_FULL_NAME set" in {
      val List(c: Call) = cpg.call.codeExact("AClass(p).x").l
      c.methodFullName shouldBe "<operator>.fieldAccess"
      c.signature shouldBe ""
    }
  }

  "CPG for code with fn call on ctor" should {
    lazy val cpg = code("""
      |package mypkg
      |fun sink(x: String) = println(x)
      |class AClass { fun appendX(to: String): String { return to + "X" } }
      |fun f1(p: String) { sink(AClass().appendX(p)) }
      |fun main() { f1("XXXX") }
      |""".stripMargin)

    "contain a CALL node with the correct METHOD_FULL_NAME set" in {
      val List(c: Call) = cpg.call.codeExact("AClass().appendX(p)").l
      c.methodFullName shouldBe "mypkg.AClass.appendX:java.lang.String(java.lang.String)"
      c.signature shouldBe "java.lang.String(java.lang.String)"
    }
  }

  "CPG for code with ctor call from java stdlib inside val assignment" should {
    lazy val cpg = code("""|fun main() {
         |    val a = java.lang.Error("err")
         |    println(a.message)
         |}
         |""".stripMargin)

    "contain a correctly lowered representation" in {
      val List(l: Local, allocAssignment: Call, init: Call, _: Call) =
        cpg.method.nameExact("main").block.astChildren.l: @unchecked
      l.name shouldBe "a"
      l.typeFullName shouldBe "java.lang.Error"

      allocAssignment.signature shouldBe ""
      allocAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(assignmentLhs: Identifier, assignmentRhs: Call) = allocAssignment.argument.l: @unchecked
      assignmentLhs.argumentIndex shouldBe 1
      assignmentLhs.code shouldBe "a"
      assignmentLhs.name shouldBe "a"
      l.referencingIdentifiers.id.l.contains(assignmentLhs.id) shouldBe true
      assignmentRhs.typeFullName shouldBe "java.lang.Error"
      assignmentRhs.code shouldBe Operators.alloc
      assignmentRhs.methodFullName shouldBe Operators.alloc
      assignmentRhs.argumentIndex shouldBe 2

      init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
      init.code shouldBe "Error(\"err\")"
      init.methodFullName shouldBe "java.lang.Error.<init>:void(java.lang.String)"
      init.signature shouldBe "void(java.lang.String)"
      init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      init.typeFullName shouldBe "void"

      val List(firstInitArg: Identifier, secondInitArg: Literal) = init.argument.l: @unchecked
      firstInitArg.name shouldBe "a"
      firstInitArg.refsTo.size shouldBe 1
      secondInitArg.code shouldBe "\"err\""
    }
  }

  "CPG for code with ctor call as argument of another call" should {
    lazy val cpg = code("""|fun main() {
         |    val l = listOf(java.lang.Error("err"))
         |    println(l)
         |}
         |""".stripMargin)

    "contain a correctly lowered representation" in {
      val List(c: Call) = cpg.call.code("listOf.*").l
      c.methodFullName shouldBe "kotlin.collections.listOf:java.util.List(java.lang.Object)"

      val List(b: Block) = c.argument.l: @unchecked
      val List(l: Local, allocAssignment: Call, init: Call, returningIdentifier: Identifier) =
        b.astChildren.l: @unchecked
      l.name shouldBe "tmp"
      l.typeFullName shouldBe "java.lang.Error"

      allocAssignment.code shouldBe "tmp = <operator>.alloc"
      allocAssignment.signature shouldBe ""
      allocAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(assignmentLhs: Identifier, assignmentRhs: Call) = allocAssignment.argument.l: @unchecked
      assignmentLhs.argumentIndex shouldBe 1
      assignmentLhs.code shouldBe "tmp"
      assignmentLhs.name shouldBe "tmp"
      l.referencingIdentifiers.id.l.contains(assignmentLhs.id) shouldBe true
      assignmentRhs.typeFullName shouldBe "java.lang.Error"
      assignmentRhs.code shouldBe Operators.alloc
      assignmentRhs.methodFullName shouldBe Operators.alloc
      assignmentRhs.argumentIndex shouldBe 2

      init.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
      init.code shouldBe "Error(\"err\")"
      init.methodFullName shouldBe "java.lang.Error.<init>:void(java.lang.String)"
      init.signature shouldBe "void(java.lang.String)"
      init.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      init.typeFullName shouldBe "void"

      val List(firstInitArg: Identifier, secondInitArg: Literal) = init.argument.l: @unchecked
      firstInitArg.name shouldBe "tmp"
      firstInitArg.refsTo.size shouldBe 1
      l.referencingIdentifiers.id.l.contains(firstInitArg.id) shouldBe true
      secondInitArg.code shouldBe "\"err\""

      returningIdentifier.name shouldBe "tmp"
      returningIdentifier.typeFullName shouldBe "java.lang.Error"
      l.referencingIdentifiers.id.l.contains(returningIdentifier.id) shouldBe true
    }
  }

}
