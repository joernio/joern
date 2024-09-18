package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.Loader
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

// TODO:
// import org.json4s.*
// import org.json4s.native.JsonMethods.parse

// TODO: "bench"
import java.time.Duration

class IonLoadingTests extends AnyWordSpec with Matchers {
  private val test1: String = """
      |{
      | FULL_NAME: "com.amazon.ion.IonFloat",
      | NAME: "IonFloat",
      | TYPE_PARAMETERS: [],
      | INHERITS: ["java.lang.Cloneable"],
      | METHOD: {
      |   NAME: "bigIntegerValue",
      |   FULL_NAME: "com.amazon.ion.IonFloat.bigIntegerValue:java.math.BigInteger()",
      |   SIGNATURE: "java.math.BigInteger()",
      | },
      | MEMBER: {
      |   NAME: "EMPTY_ARRAY",
      |   TYPE_FULL_NAME: "com.amazon.ion.IonValue",
      | },
      | DEPENDS: [
      |   {
      |     FULL_NAME: "java.math",
      |   },
      |   {
      |     FULL_NAME: "java.lang",
      |     VERSION: "4.1.2",
      |   }
      | ],
      |}
      |""".stripMargin
    
    "simple struct reader" should {
      "read into object without errors" in {
        val typ = IonTypeLoader.parse(test1)
        typ.isRight shouldBe true
        typ.toString shouldBe ""
      }
    }
}