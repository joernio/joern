package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers

class ComprehensionCpgTests extends PySrc2CpgFixture(withOssDataflow = false) with Matchers {

  "list comprehension" should {
    lazy val cpg = code("""x = [x * 2 for x in range(10)]""".stripMargin, "test.py")

    "create a block node for the lowered comprehension" in {
      cpg.block.size should be > 0
    }

    "contain a listLiteral operator call for the initial empty list" in {
      val listLiteralCalls = cpg.call.methodFullName("<operator>.listLiteral").l
      listLiteralCalls should not be empty
    }

    "contain an append call for adding elements" in {
      val appendCalls = cpg.call.name("append").l
      appendCalls should not be empty
    }

    "contain the iterator variable x as an identifier" in {
      val xIdentifiers = cpg.identifier.name("x").l
      xIdentifiers should not be empty
    }

    "contain a call to range" in {
      val rangeCalls = cpg.call.name("range").l
      rangeCalls should not be empty
    }

    "contain a multiplication operation for x * 2" in {
      val mulCalls = cpg.call.methodFullName(Operators.multiplication).l
      mulCalls should not be empty
    }
  }

  "list comprehension with filter" should {
    lazy val cpg = code("""x = [x for x in range(10) if x > 5]""".stripMargin, "test.py")

    "create a block node for the lowered comprehension" in {
      cpg.block.size should be > 0
    }

    "contain a listLiteral operator call" in {
      val listLiteralCalls = cpg.call.methodFullName("<operator>.listLiteral").l
      listLiteralCalls should not be empty
    }

    "contain the iterator variable x as an identifier" in {
      val xIdentifiers = cpg.identifier.name("x").l
      xIdentifiers should not be empty
    }

    "contain a greaterThan comparison for the filter condition" in {
      val gtCalls = cpg.call.methodFullName(Operators.greaterThan).l
      gtCalls should not be empty
    }

    "contain a conditional structure for the filter" in {
      cpg.controlStructure.l should not be empty
    }
  }

  "set comprehension" should {
    lazy val cpg = code("""x = {x * 2 for x in range(10)}""".stripMargin, "test.py")

    "create a block node for the lowered comprehension" in {
      cpg.block.size should be > 0
    }

    "contain a setLiteral operator call for the initial empty set" in {
      val setLiteralCalls = cpg.call.methodFullName("<operator>.setLiteral").l
      setLiteralCalls should not be empty
    }

    "contain an add call for adding elements to the set" in {
      val addCalls = cpg.call.name("add").l
      addCalls should not be empty
    }

    "contain the iterator variable x as an identifier" in {
      val xIdentifiers = cpg.identifier.name("x").l
      xIdentifiers should not be empty
    }

    "contain a call to range" in {
      val rangeCalls = cpg.call.name("range").l
      rangeCalls should not be empty
    }

    "contain a multiplication operation for x * 2" in {
      val mulCalls = cpg.call.methodFullName(Operators.multiplication).l
      mulCalls should not be empty
    }
  }

  "dict comprehension" should {
    lazy val cpg = code("""x = {k: v for k, v in items.items()}""".stripMargin, "test.py")

    "create a block node for the lowered comprehension" in {
      cpg.block.size should be > 0
    }

    "contain a dictLiteral operator call for the initial empty dict" in {
      val dictLiteralCalls = cpg.call.methodFullName("<operator>.dictLiteral").l
      dictLiteralCalls should not be empty
    }

    "contain the iterator variable k as an identifier" in {
      val kIdentifiers = cpg.identifier.name("k").l
      kIdentifiers should not be empty
    }

    "contain the iterator variable v as an identifier" in {
      val vIdentifiers = cpg.identifier.name("v").l
      vIdentifiers should not be empty
    }

    "contain an indexAccess call for tmp[k] = v lowering" in {
      val indexAccessCalls = cpg.call.methodFullName(Operators.indexAccess).l
      indexAccessCalls should not be empty
    }

    "contain an assignment for tmp[k] = v" in {
      val assignmentCalls = cpg.call.methodFullName(Operators.assignment).l
      assignmentCalls should not be empty
    }

    "contain a call to items() on the iterable" in {
      val itemsCalls = cpg.call.name("items").l
      itemsCalls should not be empty
    }
  }

  "generator expression" should {
    lazy val cpg = code("""x = sum(x * 2 for x in range(10))""".stripMargin, "test.py")

    "contain a genExp operator call" in {
      val genExpCalls = cpg.call.methodFullName("<operator>.genExp").l
      genExpCalls should not be empty
    }

    "contain a call to sum wrapping the generator" in {
      val sumCalls = cpg.call.name("sum").l
      sumCalls should not be empty
    }

    "contain an append call for the generator lowering" in {
      val appendCalls = cpg.call.name("append").l
      appendCalls should not be empty
    }

    "contain the iterator variable x as an identifier" in {
      val xIdentifiers = cpg.identifier.name("x").l
      xIdentifiers should not be empty
    }

    "contain a call to range" in {
      val rangeCalls = cpg.call.name("range").l
      rangeCalls should not be empty
    }

    "contain a multiplication operation for x * 2" in {
      val mulCalls = cpg.call.methodFullName(Operators.multiplication).l
      mulCalls should not be empty
    }
  }
}
