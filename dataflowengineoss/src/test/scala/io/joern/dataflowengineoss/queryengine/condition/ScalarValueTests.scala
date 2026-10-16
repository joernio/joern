package io.joern.dataflowengineoss.queryengine.condition

import flatgraph.misc.TestUtils.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal, NewCall, NewIdentifier, NewLiteral, NewLocal}
import io.shiftleft.semanticcpg.language.operatorextension.allAssignmentTypes
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ScalarValueTests extends AnyWordSpec with Matchers {
  import ScalarValue.*

  private val graph      = Cpg.empty.graph
  private val evaluation = new ScalarEvaluation(
    Map("int" -> (32, true), "unsigned int" -> (32, false), "long" -> (64, true))
  )

  private def call(name: String, resultType: String = "int", argumentTypes: Vector[String] = Vector.empty): Call = {
    val builder = Cpg.newDiffGraphBuilder
    val node    = NewCall().name(name).typeFullName(resultType)
    builder.addNode(node)
    argumentTypes.zipWithIndex.foreach { case (tpe, index) =>
      val argument =
        if (allAssignmentTypes.contains(name) && index == 0) {
          NewIdentifier().name("target").code("target").typeFullName(tpe).argumentIndex(index + 1)
        } else NewLiteral().code("0").typeFullName(tpe).argumentIndex(index + 1)
      builder.addNode(argument)
      builder.addEdge(node, argument, EdgeTypes.ARGUMENT)
    }
    builder.apply(graph)
    node.storedRef.get
  }

  private def literal(code: String, tpe: String = "int"): Literal =
    graph.addNode(NewLiteral().code(code).typeFullName(tpe))

  private def evaluate(name: String, values: ScalarValue*): ScalarValue =
    evaluation.operator(call(name), values.toVector)

  "ScalarValue" should {
    "distinguish constants, partially known values and unknown values" in {
      exact(-1).constant shouldBe Some((BigInt(1) << 64) - 1)
      exact(-1).signedConstant shouldBe Some(BigInt(-1))
      exact(0).truth shouldBe Some(false)
      ScalarValue(0, 4).truth shouldBe Some(true)
      Unknown.truth shouldBe None
      Unknown.containsBits(1) shouldBe None
      Unknown.containsBits(0) shouldBe Some(true)
      ScalarValue(2, 1).containsBits(1) shouldBe Some(true)
      ScalarValue(2, 1).containsBits(2) shouldBe Some(false)
      ScalarValue(2, 1).containsBits(5) shouldBe None
    }

    "retain facts valid for both alternatives" in {
      val result = exact(5).join(exact(7))
      result.constant shouldBe None
      result.containsBits(5) shouldBe Some(true)
      result.containsBits(2) shouldBe None
      result.containsBits(8) shouldBe Some(false)
      result.join(Unknown) shouldBe Unknown
    }

    "keep every concrete alternative in a join" in {
      for {
        left  <- 0 until 16
        right <- 0 until 16
        value <- List(left, right)
      } {
        val joined = exact(left, 4).join(exact(right, 4))
        (BigInt(value) & joined.knownZero) shouldBe BigInt(0)
        (BigInt(value) & joined.knownOne) shouldBe joined.knownOne
      }
    }

    "extend signed and unsigned values without guessing an unknown sign" in {
      exact(-1, 8).resize(64, signed = true).signedConstant shouldBe Some(BigInt(-1))
      exact(-1, 8).resize(64, signed = false).constant shouldBe Some(BigInt(255))
      ScalarValue(0, 1, 8).resize(64, signed = true).knownOne shouldBe BigInt(1)
      ScalarValue(0, 1, 8).resize(64, signed = true).constant shouldBe None
    }
  }

  "ScalarEvaluation" should {
    "read integer literal tokens and preserve their C signedness" in {
      evaluation.literal(literal("0x20UL", "long")).constant shouldBe Some(BigInt(32))
      evaluation.literal(literal("010")).constant shouldBe Some(BigInt(8))
      evaluation.literal(literal("0b101U", "unsigned int")).constant shouldBe Some(BigInt(5))
      evaluation.literal(literal("4294967295U", "unsigned int")).constant shouldBe Some(BigInt("4294967295"))
      evaluation.literal(literal("\"M_WAITOK\"", "char *")) shouldBe Unknown
      evaluation.literal(literal("1.0", "double")) shouldBe Unknown
      evaluation.literal(literal("18446744073709551616", "uint64_t")) shouldBe Unknown
    }

    "preserve set and clear flag bits without cross tainting operands" in {
      evaluate(Operators.or, Unknown, exact(1)).containsBits(1) shouldBe Some(true)
      evaluate(Operators.and, Unknown, exact(-2)).containsBits(1) shouldBe Some(false)
      evaluate(Operators.xor, exact(3), exact(1)).constant shouldBe Some(BigInt(2))
      evaluate(Operators.not, exact(0)).signedConstant shouldBe Some(BigInt(-1))
    }

    "evaluate logical operations and merge only unknown conditional alternatives" in {
      evaluate(Operators.logicalAnd, Unknown, exact(0)).constant shouldBe Some(BigInt(0))
      evaluate(Operators.logicalOr, Unknown, exact(1)).constant shouldBe Some(BigInt(1))
      evaluate(Operators.logicalNot, exact(0)).constant shouldBe Some(BigInt(1))
      evaluate(Operators.conditional, exact(0), exact(3), exact(4)).constant shouldBe Some(BigInt(4))
      evaluate(Operators.conditional, Unknown, exact(5), exact(7)).containsBits(5) shouldBe Some(true)
    }

    "apply signed and unsigned usual arithmetic conversions to comparisons" in {
      val mixed = call(Operators.equals, argumentTypes = Vector("int", "unsigned int"))
      evaluation.operator(mixed, Vector(exact(-1), exact(BigInt("4294967295")))).constant shouldBe Some(BigInt(1))
      val wide = call(Operators.lessThan, argumentTypes = Vector("long", "unsigned int"))
      evaluation.operator(wide, Vector(exact(-1), exact(BigInt("4294967295")))).constant shouldBe Some(BigInt(1))
      val narrow = call(Operators.lessThan, argumentTypes = Vector("int8_t", "uint8_t"))
      evaluation.operator(narrow, Vector(exact(-1), exact(255))).constant shouldBe Some(BigInt(1))
      val unknownPromotion = call(Operators.lessThan, argumentTypes = Vector("int8_t", "uint64_t"))
      new ScalarEvaluation()
        .operator(unknownPromotion, Vector(exact(-1), exact((BigInt(1) << 64) - 1))) shouldBe Unknown
    }

    "convert casts and Boolean values before returning uniform width values" in {
      val cast = call(Operators.cast, "int")
      evaluation.operator(cast, Vector(Unknown, exact(BigInt("4294967295")))).signedConstant shouldBe Some(BigInt(-1))
      evaluation.convert(exact(-1), "uint8_t").constant shouldBe Some(BigInt(255))
      evaluation.convert(exact(2), "bool").constant shouldBe Some(BigInt(1))
      evaluation.convert(Unknown, "bool") shouldBe Unknown
      evaluation.convert(exact(1), "unknown_integer") shouldBe Unknown
    }

    "evaluate arithmetic only when the target type and result are defined" in {
      evaluate(Operators.addition, exact(3), exact(4)).constant shouldBe Some(BigInt(7))
      evaluate(Operators.addition, exact(BigInt("2147483647")), exact(1)) shouldBe Unknown
      val unsigned = call(Operators.addition, "unsigned int")
      evaluation.operator(unsigned, Vector(exact(BigInt("4294967295")), exact(1))).constant shouldBe Some(BigInt(0))
      evaluate(Operators.division, exact(3), exact(0)) shouldBe Unknown
      evaluate(Operators.modulo, exact(BigInt("-2147483648")), exact(-1)) shouldBe Unknown
      evaluate(Operators.shiftLeft, exact(1), exact(32)) shouldBe Unknown
      evaluate(Operators.shiftLeft, exact(1), exact(BigInt(1) << 32)) shouldBe Unknown
      evaluate(Operators.shiftLeft, exact(-1), exact(1)) shouldBe Unknown
      evaluate(Operators.arithmeticShiftRight, exact(-2), exact(1)) shouldBe Unknown
      new ScalarEvaluation().operator(call(Operators.addition), Vector(exact(1), exact(2))) shouldBe Unknown
    }

    "evaluate compound division in the promoted type before narrowing the result" in {
      val division = call(Operators.assignmentDivision, "uint8_t", Vector("uint8_t", "int"))
      evaluation.operator(division, Vector(exact(100), exact(258))).constant shouldBe Some(BigInt(0))
    }

    "keep unknown assignment conversions conservative" in {
      val assignment = call(Operators.assignment, "unknown_integer")
      evaluation.operator(assignment, Vector(Unknown, exact(256))) shouldBe Unknown
      val bitwise = call(Operators.assignmentOr, "unknown_integer")
      evaluation.operator(bitwise, Vector(exact(255), exact(256))) shouldBe Unknown
    }

    "use the assignment target type for initializer calls typed void" in {
      val assignment = call(Operators.assignment, "void", Vector("int", "int"))
      evaluation.operator(assignment, Vector(Unknown, exact(3))).constant shouldBe Some(BigInt(3))
      val compound = call(Operators.assignmentOr, "void", Vector("int", "int"))
      evaluation.operator(compound, Vector(exact(1), exact(2))).constant shouldBe Some(BigInt(3))
      val addition = call(Operators.assignmentPlus, "void", Vector("int", "int"))
      evaluation.operator(addition, Vector(exact(3), exact(2))).constant shouldBe Some(BigInt(5))
    }

    "resolve assignment target types through native declaration references" in {
      val builder     = Cpg.newDiffGraphBuilder
      val assignment  = NewCall().name(Operators.assignment).typeFullName("void")
      val target      = NewIdentifier().name("flags").code("flags").typeFullName("ANY").argumentIndex(1)
      val declaration = NewLocal().name("flags").code("flags").typeFullName("int")
      val source      = NewLiteral().code("3").typeFullName("int").argumentIndex(2)
      List(assignment, target, declaration, source).foreach(builder.addNode)
      builder.addEdge(assignment, target, EdgeTypes.ARGUMENT)
      builder.addEdge(assignment, source, EdgeTypes.ARGUMENT)
      builder.addEdge(target, declaration, EdgeTypes.REF)
      builder.apply(graph)
      evaluation.operator(assignment.storedRef.get, Vector(Unknown, exact(3))).constant shouldBe Some(BigInt(3))
    }

    "infer integer types through nested native operator expressions" in {
      val builder    = Cpg.newDiffGraphBuilder
      val addition   = NewCall().name(Operators.addition).typeFullName("ANY")
      val assignment = NewCall().name(Operators.assignment).typeFullName("ANY").argumentIndex(1)
      val target     = NewIdentifier().name("flags").code("flags").typeFullName("int").argumentIndex(1)
      val xor        = NewCall().name(Operators.xor).typeFullName("ANY").argumentIndex(2)
      val source     = NewIdentifier().name("flags").code("flags").typeFullName("int").argumentIndex(1)
      val xorOperand = NewLiteral().code("1").typeFullName("int").argumentIndex(2)
      val addend     = NewLiteral().code("1").typeFullName("int").argumentIndex(2)
      List(addition, assignment, target, xor, source, xorOperand, addend).foreach(builder.addNode)
      builder.addEdge(addition, assignment, EdgeTypes.ARGUMENT)
      builder.addEdge(addition, addend, EdgeTypes.ARGUMENT)
      builder.addEdge(assignment, target, EdgeTypes.ARGUMENT)
      builder.addEdge(assignment, xor, EdgeTypes.ARGUMENT)
      builder.addEdge(xor, source, EdgeTypes.ARGUMENT)
      builder.addEdge(xor, xorOperand, EdgeTypes.ARGUMENT)
      builder.apply(graph)
      val toggled  = evaluation.operator(xor.storedRef.get, Vector(exact(0), exact(1)))
      val assigned = evaluation.operator(assignment.storedRef.get, Vector(exact(0), toggled))
      evaluation.operator(addition.storedRef.get, Vector(assigned, exact(1))).constant shouldBe Some(BigInt(2))
    }

    "infer shift width from the promoted left operand" in {
      val shift = call(Operators.shiftLeft, "ANY", Vector("uint32_t", "int64_t"))
      evaluation.operator(shift, Vector(exact(BigInt("2147483648")), exact(1))).constant shouldBe Some(BigInt(0))
      val invalid = evaluation.operator(shift, Vector(exact(1), exact(32)))
      invalid.constant shouldBe None
      invalid.truth shouldBe None
    }

    "infer conditional conversions without inferring an unknown cast target" in {
      val conditional = call(Operators.conditional, "ANY", Vector("int", "int", "uint32_t"))
      evaluation.operator(conditional, Vector(exact(1), exact(-1), exact(0))).constant shouldBe
        Some(BigInt("4294967295"))
      val cast = call(Operators.cast, "ANY", Vector("int", "int"))
      evaluation.operator(cast, Vector(Unknown, exact(1))) shouldBe Unknown
    }
  }
}
