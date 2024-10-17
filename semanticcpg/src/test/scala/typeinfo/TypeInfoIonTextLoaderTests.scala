package io.shiftleft.semanticcpg.typeinfo

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.util.Try

class TypeInfoIonTextLoaderTests extends AnyWordSpec with Matchers {
  private val test1: String = """
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

  "simple struct reader" should {
    "read into object without errors" in {
      val result = TypeInfoIonTextLoader.loadFromString(test1)

      result.fullName shouldEqual "com.amazon.ion.IonFloat"
      result.name shouldEqual "IonFloat"
      result.typeParams shouldBe empty
      result.inherits should have length 1
      result.inherits should contain("java.lang.Cloneable")

      result.methods should have length 1
      val method = result.methods.head
      method.name shouldBe "bigIntegerValue"
      method.fullName shouldBe "com.amazon.ion.IonFloat.bigIntegerValue:java.math.BigInteger()"
      method.signature shouldBe "java.math.BigInteger()"

      result.members should have length 1
      val member = result.members.head
      member.name shouldBe "EMPTY_ARRAY"
      member.typeFullName shouldBe "com.amazon.ion.IonValue"
    }
  }
}
