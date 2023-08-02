package io.joern.pysrc2cpg.slicing

import io.joern.dataflowengineoss.slicing.*
import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators

class PyUsageSliceTests extends PySrc2CpgFixture {

  private val config = UsagesConfig(excludeOperatorCalls = true).withParallelism(1)

  "extracting a usage slice from basic objects" should {

    lazy val cpg = code("""
        |from flask_sqlalchemy import SQLAlchemy
        |
        |x = 1
        |y = "test"
        |db = SQLAlchemy()
        |
        |db.createTable()
        |db.deleteTable()
        |""".stripMargin)

    val programSlice = UsageSlicing.calculateUsageSlice(cpg, config.copy(excludeOperatorCalls = false))

    "should successfully extract 'db' usages" in {
      val slice = programSlice.objectSlices.head.slices.head
      slice.targetObj shouldBe LocalDef("db", "flask_sqlalchemy.py:<module>.SQLAlchemy", Some(6), Some(1))
      slice.definedBy shouldBe Option(
        CallDef("SQLAlchemy", "ANY", Some("flask_sqlalchemy.py:<module>.SQLAlchemy.__init__"), Some(6), Some(6))
      )

      val inv1 = slice.invokedCalls.find(_.callName == "createTable").get
      val inv2 = slice.invokedCalls.find(_.callName == "deleteTable").get

      inv1.resolvedMethod shouldBe Some("flask_sqlalchemy.py:<module>.SQLAlchemy.createTable")
      inv1.paramTypes shouldBe List.empty
      inv1.returnType shouldBe "ANY"

      inv2.resolvedMethod shouldBe Some("flask_sqlalchemy.py:<module>.SQLAlchemy.deleteTable")
      inv2.paramTypes shouldBe List.empty
      inv2.returnType shouldBe "ANY"

      val List(arg1) = slice.argToCalls

      arg1.callName shouldBe Operators.assignment
    }

  }

}
