package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.validation.IonSchemaValidator
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source
import scala.util.Try

class IonSchemaValidatorTests extends AnyWordSpec with Matchers {
  private val typeSchema: String            = Source.fromResource("TypeInfoSchema.isl").mkString
  private val validator: IonSchemaValidator = IonSchemaValidator(typeSchema, "type_decl")

  "valid ion" should {
    val test1: String =
      """
        |{
        | FULL_NAME:"com.amazon.ion.IonFloat",
        | NAME:"IonFloat",
        | TYPE_PARAMETERS:[
        | ],
        | INHERITS:[
        |   "java.lang.Cloneable"
        | ],
        | METHODS:[
        |   {
        |     NAME:"bigIntegerValue",
        |     FULL_NAME:"com.amazon.ion.IonFloat.bigIntegerValue:java.math.BigInteger()",
        |     SIGNATURE:"java.math.BigInteger()"
        |   }
        | ],
        | MEMBERS:[
        |   {
        |     NAME:"EMPTY_ARRAY",
        |     TYPE_FULL_NAME:"com.amazon.ion.IonValue"
        |   }
        | ]
        |}""".stripMargin

    "validate" in {
      val violations = validator.validate(test1)
      violations.isValid shouldEqual true
    }

    "not validate when fails schema" in {
      val missingRequiredField: String =
        """
          |{
          |      NAME:"IonFloat",
          |}
          |""".stripMargin
      val violations = validator.validate(missingRequiredField)
      violations.isValid shouldEqual false
      violations.forEach(v => println(s"${v.getCode} -- ${v.getMessage}"))
    }
  }

  "invalid ion" should {
    "missing closing square brace errors" in {
      val missingSquareBrace: String =
        """
          |{
          |      FULL_NAME:"com.amazon.ion.IonFloat",
          |      NAME:"IonFloat",
          |      TYPE_PARAMETERS:[,
          |}
          |""".stripMargin
      val violations = validator.validate(missingSquareBrace)
      violations.isValid shouldEqual false
      violations.forEach(v => println(s"${v.getCode} -- ${v.getMessage}"))
    }

    "missing closing brace should error" in {
      val missingClosingBrace: String =
        """
          |{ FULL_NAME: "com.amazon.ion.IonFloat", NAME: "IonFloat",
          |""".stripMargin
      val violations = validator.validate(missingClosingBrace)
      violations.isValid shouldEqual false
      violations.forEach(v => println(s"${v.getCode} -- ${v.getMessage}"))
    }

    "missing comma between fields should error" in {
      val missingCommaBetweenFields: String =
        """
          |{
          |      FULL_NAME:"com.amazon.ion.IonFloat",
          |      NAME:"IonFloat"
          |      TYPE_PARAMETERS:[],
          |}
          |""".stripMargin
      val violations = validator.validate(missingCommaBetweenFields)
      violations.isValid shouldEqual false
      violations.forEach(v => println(s"${v.getCode} -- ${v.getMessage}"))
    }

    "missing interior comma in list should error" in {
      val missingInteriorCommaInList: String =
        """
          |{
          |      FULL_NAME:"com.amazon.ion.IonFloat",
          |      NAME:"IonFloat",
          |      TYPE_PARAMETERS:[ "T" "M" ],
          |}
          |""".stripMargin
      val violations = validator.validate(missingInteriorCommaInList)
      violations.isValid shouldEqual false
      violations.forEach(v => println(s"${v.getCode} -- ${v.getMessage}"))
    }
  }
}
