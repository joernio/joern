package typeinfo

import io.shiftleft.semanticcpg.typeinfo.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonLoaderTests extends AnyWordSpec with Matchers {
  private val test1: String =
    """
      |{
      | "FULL_NAME":"com.amazon.ion.IonFloat",
      | "NAME":"IonFloat",
      | "TYPE_PARAMETERS":[
      | ],
      | "INHERITS":[
      |   "java.lang.Cloneable"
      | ],
      | "METHODS":[
      |   {
      |     "NAME":"bigIntegerValue",
      |     "FULL_NAME":"com.amazon.ion.IonFloat.bigIntegerValue:java.math.BigInteger()",
      |     "SIGNATURE":"java.math.BigInteger()"
      |   }
      | ],
      | "MEMBERS":[
      |   {
      |     "NAME":"EMPTY_ARRAY",
      |     "TYPE_FULL_NAME":"com.amazon.ion.IonValue"
      |   }
      | ],
      | "DEPENDS":[
      |   {
      |     "FULL_NAME": "java.lang",
      |     "VERSION": "4.1.2"
      |   }
      | ]
      |}""".stripMargin

  "simple struct reader" should {
    "read into object without errors" in {
      val result: Either[String, TypeDecl] = JsonLoader.parse(test1)
      result.isRight shouldEqual true

      val t = result.right.get
      t.fullName shouldEqual "com.amazon.ion.IonFloat"
      t.name shouldEqual "IonFloat"
      t.typeParams shouldBe empty
      t.inherits should have length 1
      t.inherits should contain("java.lang.Cloneable")

      t.methods should have length 1
      val method = t.methods.head
      method.name shouldBe "bigIntegerValue"
      method.fullName shouldBe "com.amazon.ion.IonFloat.bigIntegerValue:java.math.BigInteger()"
      method.signature shouldBe "java.math.BigInteger()"

      t.members should have length 1
      val member = t.members.head
      member.name shouldBe "EMPTY_ARRAY"
      member.typeFullName shouldBe "com.amazon.ion.IonValue"

      t.dependencies should have length 1
      val dependency = t.dependencies.head
      dependency.fullName shouldBe "java.lang"
      dependency.version shouldBe Some("4.1.2")
    }
  }
}
