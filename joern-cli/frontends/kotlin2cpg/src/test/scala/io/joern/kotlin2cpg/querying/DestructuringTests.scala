package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal, Local}
import io.shiftleft.semanticcpg.language.*

class DestructuringTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with destructuring declaration and a variable as RHS" should {
    val cpg = code("""
        |package main
        |
        |data class AClass(val a: String, val b: Int)
        |fun main() {
        |    val aClass = AClass("AMESSAGE", 41414141)
        |    val (myA, myB) = aClass
        |    println(myA)
        |    println(myB)
        |// prints
        |//```
        |//AMESSAGE
        |//41414141
        |//```
        |}
        |""".stripMargin)

    "should contain a correctly-lowered representation" in {
      val List(firstDestructLocal) = cpg.local.nameExact("myA").l
      firstDestructLocal.code shouldBe "myA"
      firstDestructLocal.typeFullName shouldBe "java.lang.String"

      val List(secondDestructLocal) = cpg.local.nameExact("myB").l
      secondDestructLocal.code shouldBe "myB"
      secondDestructLocal.typeFullName shouldBe "int"

      val List(firstDestructAssignment) = cpg.call.code("myA.*=.*component.*").l
      firstDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      firstDestructAssignment.methodFullName shouldBe Operators.assignment

      val firstDestructLHSIdentifier = firstDestructAssignment.argument(1).asInstanceOf[Identifier]
      firstDestructLHSIdentifier.name shouldBe "myA"
      firstDestructLHSIdentifier.typeFullName shouldBe "java.lang.String"

      val firstDestructRHSCall = firstDestructAssignment.argument(2).asInstanceOf[Call]
      firstDestructRHSCall.code shouldBe "aClass.component1()"
      firstDestructRHSCall.name shouldBe "component1"
      firstDestructRHSCall.methodFullName shouldBe "main.AClass.component1:java.lang.String()"
      firstDestructRHSCall.signature shouldBe "java.lang.String()"
      firstDestructRHSCall.typeFullName shouldBe "java.lang.String"

      val List(firstDestructRHSCallArgument: Identifier) =
        firstDestructAssignment.argument(2).asInstanceOf[Call].argument.l: @unchecked
      firstDestructRHSCallArgument.argumentIndex shouldBe 0
      firstDestructRHSCallArgument.code shouldBe "aClass"
      firstDestructRHSCallArgument.name shouldBe "aClass"
      firstDestructRHSCallArgument.typeFullName shouldBe "main.AClass"
      firstDestructRHSCallArgument.refsTo.size shouldBe 1

      val List(secondDestructAssignment) = cpg.call.code("myB.*=.*component.*").l
      secondDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      secondDestructAssignment.methodFullName shouldBe Operators.assignment

      val secondDestructLHSIdentifier = secondDestructAssignment.argument(1).asInstanceOf[Identifier]
      secondDestructLHSIdentifier.name shouldBe "myB"
      secondDestructLHSIdentifier.typeFullName shouldBe "int"

      val secondDestructRHSCall = secondDestructAssignment.argument(2).asInstanceOf[Call]
      secondDestructRHSCall.code shouldBe "aClass.component2()"
      secondDestructRHSCall.methodFullName shouldBe "main.AClass.component2:int()"
      secondDestructRHSCall.signature shouldBe "int()"
      secondDestructRHSCall.typeFullName shouldBe "int"

      val List(secondDestructRHSCallArgument: Identifier) =
        secondDestructAssignment.argument(2).asInstanceOf[Call].argument.l: @unchecked
      secondDestructRHSCallArgument.argumentIndex shouldBe 0
      secondDestructRHSCallArgument.code shouldBe "aClass"
      secondDestructRHSCallArgument.typeFullName shouldBe "main.AClass"
      secondDestructRHSCallArgument.refsTo.size shouldBe 1
    }
  }

  "CPG for code with destructuring declaration and a variable as RHS, plus one `_`" should {
    val cpg = code("""
        |package main
        |
        |data class AClass(val a: String, val b: Int)
        |fun main() {
        |    val aClass = AClass("AMESSAGE", 41414141)
        |    val (myA, _) = aClass
        |    println(myA)
        |// prints
        |//```
        |//AMESSAGE
        |//```
        |}
        |""".stripMargin)

    "should not contain a CALL node for `component2`" in {
      cpg.call.methodFullName(".*component2.*").size shouldBe 0
    }

    "should not contain a LOCAL node for `_`" in {
      cpg.local.codeNot(".*tmp.*").code(".*_.*").size shouldBe 0
    }
  }

  /*
   _______ example lowering _________
  | -> val (one, two) = Person("a", "b")
  | -> LOCAL one
  | -> LOCAL two
  | -> LOCAL tmp
  | -> tmp = alloc
  | -> tmp.<init>
  | -> CALL one = tmp.component1()
  | -> CALL two = tmp.component2()
  |__________________________________
   */
  "CPG for code with destructuring expression with a ctor-invocation RHS" should {
    val cpg = code("""
        |package main
        |
        |data class AClass(val a: String, val b: Int)
        |fun main() {
        |    val aMessage = "AMESSAGE"
        |    val (myA, myB) = AClass(aMessage, 41414141)
        |    println(myA)
        |    println(myB)
        |// prints
        |//```
        |//AMESSAGE
        |//41414141
        |//```
        |}
        |""".stripMargin)

    "should contain a correctly-lowered representation" in {
      val List(firstDestructLocal) = cpg.local.nameExact("myA").l
      firstDestructLocal.code shouldBe "myA"
      firstDestructLocal.typeFullName shouldBe "java.lang.String"

      val List(secondDestructLocal) = cpg.local.nameExact("myB").l
      secondDestructLocal.code shouldBe "myB"
      secondDestructLocal.typeFullName shouldBe "int"

      val List(tmpLocal) = cpg.local.name("tmp_.*").l
      tmpLocal.code shouldBe "tmp_1"
      tmpLocal.typeFullName shouldBe "main.AClass"

      val List(tmpAssignmentToAlloc) = cpg.call.code("tmp.*=.*alloc.*").l
      tmpAssignmentToAlloc.methodFullName shouldBe Operators.assignment
      tmpAssignmentToAlloc.typeFullName shouldBe "ANY"
      tmpAssignmentToAlloc.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(allocAssignmentLhs: Identifier, allocAssignmentRhs: Call) = tmpAssignmentToAlloc.argument.l: @unchecked
      allocAssignmentLhs.argumentIndex shouldBe 1
      allocAssignmentLhs.code shouldBe "tmp_1"
      allocAssignmentLhs.typeFullName shouldBe "main.AClass"
      tmpLocal.referencingIdentifiers.id.l.contains(allocAssignmentLhs.id) shouldBe true

      allocAssignmentRhs.code shouldBe "alloc"
      allocAssignmentRhs.signature shouldBe ""
      allocAssignmentRhs.methodFullName shouldBe "<operator>.alloc"
      allocAssignmentRhs.typeFullName shouldBe "main.AClass"
      allocAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      allocAssignmentRhs.argumentIndex shouldBe 2

      val List(tmpInitCall) = cpg.call.code(".*init.*").l
      tmpInitCall.code shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
      tmpInitCall.methodFullName shouldBe "main.AClass.<init>:void(java.lang.String,int)"
      tmpInitCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      tmpInitCall.argument.code.l shouldBe List("tmp_1", "aMessage", "41414141")

      val List(initCallFirstArg: Identifier, initCallSecondArg: Identifier, initCallThirdArg: Literal) =
        tmpInitCall.argument.l: @unchecked
      initCallFirstArg.code shouldBe "tmp_1"
      initCallFirstArg.typeFullName shouldBe "main.AClass"
      initCallFirstArg.argumentIndex shouldBe 0
      initCallFirstArg.order shouldBe 1
      tmpLocal.referencingIdentifiers.id.l.contains(initCallFirstArg.id) shouldBe true

      initCallSecondArg.code shouldBe "aMessage"
      initCallSecondArg.typeFullName shouldBe "java.lang.String"
      initCallSecondArg.argumentIndex shouldBe 1
      initCallSecondArg.order shouldBe 2

      initCallThirdArg.code shouldBe "41414141"
      initCallThirdArg.typeFullName shouldBe "int"
      initCallThirdArg.argumentIndex shouldBe 2
      initCallThirdArg.order shouldBe 3

      val List(firstDestructAssignment) = cpg.call.code("myA.*=.*component.*").l
      firstDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      firstDestructAssignment.methodFullName shouldBe Operators.assignment

      val firstDestructLHSIdentifier = firstDestructAssignment.argument(1).asInstanceOf[Identifier]
      firstDestructLHSIdentifier.name shouldBe "myA"
      firstDestructLHSIdentifier.typeFullName shouldBe "java.lang.String"

      val firstDestructRHSCall = firstDestructAssignment.argument(2).asInstanceOf[Call]
      firstDestructRHSCall.code should startWith("tmp_")
      firstDestructRHSCall.code should endWith("component1()")
      firstDestructRHSCall.name shouldBe "component1"
      firstDestructRHSCall.methodFullName shouldBe "main.AClass.component1:java.lang.String()"
      firstDestructRHSCall.signature shouldBe "java.lang.String()"
      firstDestructRHSCall.typeFullName shouldBe "java.lang.String"

      val List(firstDestructRHSCallArgument: Identifier) =
        firstDestructAssignment.argument(2).asInstanceOf[Call].argument.l: @unchecked
      firstDestructRHSCallArgument.argumentIndex shouldBe 0
      firstDestructRHSCallArgument.code should startWith("tmp_")
      firstDestructRHSCallArgument.typeFullName shouldBe "main.AClass"
      firstDestructRHSCallArgument.refsTo.size shouldBe 1

      val List(secondDestructAssignment) = cpg.call.code("myB.*=.*component.*").l
      secondDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      secondDestructAssignment.methodFullName shouldBe Operators.assignment

      val secondDestructLHSIdentifier = secondDestructAssignment.argument(1).asInstanceOf[Identifier]
      secondDestructLHSIdentifier.name shouldBe "myB"
      secondDestructLHSIdentifier.typeFullName shouldBe "int"

      val secondDestructRHSCall = secondDestructAssignment.argument(2).asInstanceOf[Call]
      secondDestructRHSCall.code should startWith("tmp_")
      secondDestructRHSCall.code should endWith("component2()")
      secondDestructRHSCall.name shouldBe "component2"
      secondDestructRHSCall.methodFullName shouldBe "main.AClass.component2:int()"
      secondDestructRHSCall.signature shouldBe "int()"
      secondDestructRHSCall.typeFullName shouldBe "int"

      val List(secondDestructRHSCallArgument: Identifier) =
        secondDestructAssignment.argument(2).asInstanceOf[Call].argument.l: @unchecked
      secondDestructRHSCallArgument.argumentIndex shouldBe 0
      secondDestructRHSCallArgument.code should startWith("tmp_")
      secondDestructRHSCallArgument.typeFullName shouldBe "main.AClass"
      secondDestructRHSCallArgument.refsTo.size shouldBe 1
    }
  }

  "CPG for code with destructuring expression with a ctor-invocation RHS and an `_`" should {
    val cpg = code("""
        |package main
        |
        |data class AClass(val a: String, val b: Int)
        |fun main() {
        |    val aMessage = "AMESSAGE"
        |    val (myA, _) = AClass(aMessage, 41414141)
        |    println(myA)
        |// prints
        |//```
        |//AMESSAGE
        |//```
        |}
        |""".stripMargin)

    "should not contain a CALL node for `component2`" in {
      cpg.call.methodFullName(".*component2.*").size shouldBe 0
    }

    "should not contain a LOCAL node for `_`" in {
      cpg.local.codeNot(".*tmp.*").code(".*_.*").size shouldBe 0
    }
  }

  "CPG for code with destructuring expression with a non-ctor-call RHS" should {
    val cpg = code("""
        |package mypkg
        |
        |data class AClass(val a: String, val b: Int)
        |
        |fun makeA(x: String): AClass {
        |    val a = AClass(x, 41414141)
        |    return a
        |}
        |
        |fun main() {
        |    val aMessage = "AMESSAGE"
        |    val (myA, myB) = makeA(aMessage)
        |    println(myA)
        |    println(myB)
        |// prints
        |//```
        |//AMESSAGE
        |//41414141
        |//```
        |}
        |""".stripMargin)

    "should contain a correctly-lowered representation" in {
      val List(firstDestructLocal) = cpg.local.nameExact("myA").l
      firstDestructLocal.code shouldBe "myA"
      firstDestructLocal.typeFullName shouldBe "java.lang.String"

      val List(secondDestructLocal) = cpg.local.nameExact("myB").l
      secondDestructLocal.code shouldBe "myB"
      secondDestructLocal.typeFullName shouldBe "int"

      val List(tmpLocal) = cpg.local.name("tmp_.*").where(_.method.fullName(".*main.*")).l
      tmpLocal.code shouldBe "tmp_1"
      tmpLocal.typeFullName shouldBe "mypkg.AClass"

      val List(tmpAssignmentToRhsCall) = cpg.call.code("tmp.*=.*makeA.*").l
      tmpAssignmentToRhsCall.methodFullName shouldBe Operators.assignment
      tmpAssignmentToRhsCall.typeFullName shouldBe "ANY"
      tmpAssignmentToRhsCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(allocAssignmentLhs: Identifier, rhsCall: Call) = tmpAssignmentToRhsCall.argument.l: @unchecked
      allocAssignmentLhs.argumentIndex shouldBe 1
      allocAssignmentLhs.code shouldBe "tmp_1"
      allocAssignmentLhs.typeFullName shouldBe "mypkg.AClass"
      tmpLocal.referencingIdentifiers.id.l.contains(allocAssignmentLhs.id) shouldBe true

      rhsCall.code shouldBe "makeA(aMessage)"
      rhsCall.signature shouldBe "mypkg.AClass(java.lang.String)"
      rhsCall.methodFullName shouldBe "mypkg.makeA:mypkg.AClass(java.lang.String)"
      rhsCall.typeFullName shouldBe "mypkg.AClass"
      rhsCall.name shouldBe "makeA"
      rhsCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      rhsCall.argumentIndex shouldBe 2

      rhsCall.argument.size shouldBe 1
      val List(rhsCallFirstArg: Identifier) = rhsCall.argument.l: @unchecked
      rhsCallFirstArg.argumentIndex shouldBe 1
      rhsCallFirstArg.code shouldBe "aMessage"
      rhsCallFirstArg.typeFullName shouldBe "java.lang.String"

      val List(firstDestructAssignment) = cpg.call.code("myA.*=.*component.*").l
      firstDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      firstDestructAssignment.methodFullName shouldBe Operators.assignment

      val firstDestructLHSIdentifier = firstDestructAssignment.argument(1).asInstanceOf[Identifier]
      firstDestructLHSIdentifier.name shouldBe "myA"
      firstDestructLHSIdentifier.typeFullName shouldBe "java.lang.String"

      val firstDestructRHSCall = firstDestructAssignment.argument(2).asInstanceOf[Call]
      firstDestructRHSCall.code should startWith("tmp_")
      firstDestructRHSCall.code should endWith("component1()")
      firstDestructRHSCall.name shouldBe "component1"
      firstDestructRHSCall.methodFullName shouldBe "mypkg.AClass.component1:java.lang.String()"
      firstDestructRHSCall.signature shouldBe "java.lang.String()"
      firstDestructRHSCall.typeFullName shouldBe "java.lang.String"

      val List(firstDestructRHSCallArgument: Identifier) =
        firstDestructAssignment.argument(2).asInstanceOf[Call].argument.l: @unchecked
      firstDestructRHSCallArgument.argumentIndex shouldBe 0
      firstDestructRHSCallArgument.code should startWith("tmp_")
      firstDestructRHSCallArgument.typeFullName shouldBe "mypkg.AClass"
      firstDestructRHSCallArgument.refsTo.size shouldBe 1

      val List(secondDestructAssignment) = cpg.call.code("myB.*=.*component.*").l
      secondDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      secondDestructAssignment.methodFullName shouldBe Operators.assignment

      val secondDestructLHSIdentifier = secondDestructAssignment.argument(1).asInstanceOf[Identifier]
      secondDestructLHSIdentifier.name shouldBe "myB"
      secondDestructLHSIdentifier.typeFullName shouldBe "int"

      val secondDestructRHSCall = secondDestructAssignment.argument(2).asInstanceOf[Call]
      secondDestructRHSCall.code should startWith("tmp_")
      secondDestructRHSCall.code should endWith("component2()")
      secondDestructRHSCall.name shouldBe "component2"
      secondDestructRHSCall.methodFullName shouldBe "mypkg.AClass.component2:int()"
      secondDestructRHSCall.signature shouldBe "int()"
      secondDestructRHSCall.typeFullName shouldBe "int"

      val List(secondDestructRHSCallArgument: Identifier) =
        secondDestructAssignment.argument(2).asInstanceOf[Call].argument.l: @unchecked
      secondDestructRHSCallArgument.argumentIndex shouldBe 0
      secondDestructRHSCallArgument.code should startWith("tmp_")
      secondDestructRHSCallArgument.typeFullName shouldBe "mypkg.AClass"
      secondDestructRHSCallArgument.refsTo.size shouldBe 1
    }
  }

  "CPG for code with destructuring expression with a non-ctor-call RHS and an `_`" should {
    val cpg = code("""
        |package mypkg
        |
        |data class AClass(val a: String, val b: Int)
        |
        |fun makeA(x: String): AClass {
        |    val a = AClass(x, 41414141)
        |    return a
        |}
        |
        |fun main() {
        |    val aMessage = "AMESSAGE"
        |    val (myA, _) = makeA(aMessage)
        |    println(myA)
        |// prints
        |//```
        |//AMESSAGE
        |//```
        |}
        |""".stripMargin)

    "should not contain a CALL node for `component2`" in {
      cpg.call.methodFullName(".*component2.*").size shouldBe 0
    }

    "should not contain a LOCAL node for `_`" in {
      cpg.local.codeNot(".*tmp.*").code(".*_.*").size shouldBe 0
    }
  }

  /*
   _______ example lowering _________
  | -> val (one, two) = makeA("AMESSAGE")
  | -> LOCAL one
  | -> LOCAL two
  | -> LOCAL tmp
  | -> tmp = makeA("AMESSAGE")
  | -> CALL one = tmp.component1()
  | -> CALL two = tmp.component2()
  |__________________________________
   */
  "CPG for code with destructuring expression with a DQE call RHS" should {
    val cpg = code("""
        |package mypkg
        |
        |data class AClass(val a: String, val b: Int)
        |
        |class AFactory {
        |    fun makeA(x: String): AClass {
        |        val a = AClass(x, 41414141)
        |        return a
        |    }
        |}
        |
        |fun main() {
        |    val aFactory = AFactory()
        |    val aMessage = "AMESSAGE"
        |    val (myA, myB) = aFactory.makeA(aMessage)
        |    println(myA)
        |    println(myB)
        |//prints:
        |//```
        |//AMESSAGE
        |//41414141
        |//```
        |}
        |
        |""".stripMargin)

    "should contain a correctly-lowered representation" in {
      val List(firstDestructLocal) = cpg.local.nameExact("myA").l
      firstDestructLocal.code shouldBe "myA"
      firstDestructLocal.typeFullName shouldBe "java.lang.String"

      val List(secondDestructLocal) = cpg.local.nameExact("myB").l
      secondDestructLocal.code shouldBe "myB"
      secondDestructLocal.typeFullName shouldBe "int"

      val List(tmpLocal1: Local) =
        cpg.local
          .name("tmp_.*")
          .where(_.method.fullName(".*main.*"))
          .l
      tmpLocal1.code shouldBe "tmp_1"
      tmpLocal1.typeFullName shouldBe "mypkg.AClass"

      val List(tmpAssignmentToRhsCall) = cpg.call.code("tmp.*=.*makeA.*").l
      tmpAssignmentToRhsCall.methodFullName shouldBe Operators.assignment
      tmpAssignmentToRhsCall.typeFullName shouldBe "ANY"
      tmpAssignmentToRhsCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(allocAssignmentLhs: Identifier, rhsCall: Call) = tmpAssignmentToRhsCall.argument.l: @unchecked
      allocAssignmentLhs.argumentIndex shouldBe 1
      allocAssignmentLhs.code shouldBe "tmp_1"
      allocAssignmentLhs.typeFullName shouldBe "mypkg.AClass"
      tmpLocal1.referencingIdentifiers.id.l.contains(allocAssignmentLhs.id) shouldBe true

      rhsCall.code shouldBe "aFactory.makeA(aMessage)"
      rhsCall.signature shouldBe "mypkg.AClass(java.lang.String)"
      rhsCall.methodFullName shouldBe "mypkg.AFactory.makeA:mypkg.AClass(java.lang.String)"
      rhsCall.typeFullName shouldBe "mypkg.AClass"
      rhsCall.name shouldBe "makeA"
      rhsCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      rhsCall.argumentIndex shouldBe 2
      rhsCall.argument.size shouldBe 2

      val List(rhsCallFirstArg: Identifier, rhsCallSecondArg: Identifier) = rhsCall.argument.l: @unchecked
      rhsCallFirstArg.argumentIndex shouldBe 0
      rhsCallFirstArg.code shouldBe "aFactory"
      rhsCallFirstArg.typeFullName shouldBe "mypkg.AFactory"
      rhsCallSecondArg.argumentIndex shouldBe 1
      rhsCallSecondArg.code shouldBe "aMessage"
      rhsCallSecondArg.typeFullName shouldBe "java.lang.String"

      val List(firstDestructAssignment) = cpg.call.code("myA.*=.*component.*").l
      firstDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      firstDestructAssignment.methodFullName shouldBe Operators.assignment

      val firstDestructLHSIdentifier = firstDestructAssignment.argument(1).asInstanceOf[Identifier]
      firstDestructLHSIdentifier.name shouldBe "myA"
      firstDestructLHSIdentifier.typeFullName shouldBe "java.lang.String"

      val firstDestructRHSCall = firstDestructAssignment.argument(2).asInstanceOf[Call]
      firstDestructRHSCall.code should startWith("tmp_")
      firstDestructRHSCall.code should endWith("component1()")
      firstDestructRHSCall.name shouldBe "component1"
      firstDestructRHSCall.methodFullName shouldBe "mypkg.AClass.component1:java.lang.String()"
      firstDestructRHSCall.signature shouldBe "java.lang.String()"
      firstDestructRHSCall.typeFullName shouldBe "java.lang.String"

      val List(firstDestructRHSCallArgument: Identifier) =
        firstDestructAssignment.argument(2).asInstanceOf[Call].argument.l: @unchecked
      firstDestructRHSCallArgument.argumentIndex shouldBe 0
      firstDestructRHSCallArgument.code should startWith("tmp_")
      firstDestructRHSCallArgument.typeFullName shouldBe "mypkg.AClass"
      firstDestructRHSCallArgument.refsTo.size shouldBe 1

      val List(secondDestructAssignment) = cpg.call.code("myB.*=.*component.*").l
      secondDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      secondDestructAssignment.methodFullName shouldBe Operators.assignment

      val secondDestructLHSIdentifier = secondDestructAssignment.argument(1).asInstanceOf[Identifier]
      secondDestructLHSIdentifier.name shouldBe "myB"
      secondDestructLHSIdentifier.typeFullName shouldBe "int"

      val secondDestructRHSCall = secondDestructAssignment.argument(2).asInstanceOf[Call]
      secondDestructRHSCall.code should startWith("tmp_")
      secondDestructRHSCall.code should endWith("component2()")
      secondDestructRHSCall.name shouldBe "component2"
      secondDestructRHSCall.methodFullName shouldBe "mypkg.AClass.component2:int()"
      secondDestructRHSCall.signature shouldBe "int()"
      secondDestructRHSCall.typeFullName shouldBe "int"

      val List(secondDestructRHSCallArgument: Identifier) =
        secondDestructAssignment.argument(2).asInstanceOf[Call].argument.l: @unchecked
      secondDestructRHSCallArgument.argumentIndex shouldBe 0
      secondDestructRHSCallArgument.code should startWith("tmp_")
      secondDestructRHSCallArgument.typeFullName shouldBe "mypkg.AClass"
      secondDestructRHSCallArgument.refsTo.size shouldBe 1
    }
  }

  "CPG for code with destructuring expression with a DQE call RHS and an `_`" should {
    val cpg = code("""
        |package mypkg
        |
        |data class AClass(val a: String, val b: Int)
        |
        |class AFactory {
        |    fun makeA(x: String): AClass {
        |        val a = AClass(x, 41414141)
        |        return a
        |    }
        |}
        |
        |fun main() {
        |    val aFactory = AFactory()
        |    val aMessage = "AMESSAGE"
        |    val (myA, _) = aFactory.makeA(aMessage)
        |    println(myA)
        |//prints:
        |//```
        |//AMESSAGE
        |//```
        |}
        |
        |""".stripMargin)

    "should not contain a CALL node for `component2`" in {
      cpg.call.methodFullName(".*component2.*").size shouldBe 0
    }

    "should not contain a LOCAL node for `_`" in {
      cpg.local.codeNot(".*tmp.*").code(".*_.*").size shouldBe 0
    }
  }

  "CPG for code with destructuring expression with an array access call RHS" should {
    val cpg = code("""
        |package mypkg
        |fun f1() {
        |    data class Entry(val p: String, val q: String)
        |    val l = listOf(Entry("one", "two"), Entry("three", "four"))
        |    val (p, q) = l[1]
        |    println(p)
        |    println(q)
        |}
        |""".stripMargin)

    "should contain an `indexAccess` CALL node for the RHS" in {
      val List(c: Call) = cpg.call.codeExact("l[1]").l
      c.methodFullName shouldBe Operators.indexAccess
    }
  }

  "CPG for code with destructuring expression with a postfix expression RHS" should {
    val cpg = code("""
        |package mypkg
        |fun x98() {
        |    data class Entry(val p: String, val q: String)
        |    val mrr = mapOf("one" to Entry("p1", "q1"), "two" to Entry("p2", "q2"))
        |    val (p, q) = mrr.get("one")!!
        |    println(p)
        |    println(q)
        |}
        |""".stripMargin)

    "should contain an `notNullAssert` CALL node for the RHS" in {
      val List(c: Call) = cpg.call.codeExact("mrr.get(\"one\")!!").l
      c.methodFullName shouldBe Operators.notNullAssert
    }
  }

  "CPG for code with destructuring expression with a _when_ expression RHS" should {
    val cpg = code("""
        |package mypkg
        |fun f1() {
        |    data class Entry(val p: String, val q: String)
        |    val (p, q) =
        |        when(Random(1).nextInt()) {
        |            1 -> Entry("p1", "q1")
        |            else -> Entry("pelse", "qelse")
        |        }
        |}
        |""".stripMargin)

    "should contain an `when`-operator CALL node for the RHS" in {
      val List(c: Call) = cpg.call.methodFullName("<operator>.when").l
      c.methodFullName shouldBe "<operator>.when"
    }
  }

  "CPG for code with destructuring expression with an _if_ expression RHS" should {
    val cpg = code("""
      |package mypkg
      |import kotlin.random.Random
      |fun f1() {
      |    data class Entry(val p: String, val q: String)
      |    val (p, q) =
      |        if (Random(1).nextBoolean()) {
      |            Entry("onep", "oneq")
      |        } else {
      |            Entry("twop", "twoq")
      |        }
      |    println(p)
      |    println(q)
      |}
      |""".stripMargin)

    "should contain a `conditional`-operator CALL node for the RHS" in {
      cpg.call.methodFullName("<operator>.conditional").size shouldBe 1
    }
  }

}
