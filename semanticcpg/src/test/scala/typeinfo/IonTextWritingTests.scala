package typeinfo


import io.shiftleft.semanticcpg.typeinfo.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IonTextWritingTests extends AnyWordSpec with Matchers {
  private val test1: String = """
      |{
      | FULL_NAME:"com.amazon.ion.IonFloat",
      | NAME:"IonFloat",
      | TYPE_PARAMETERS:[
      | ],
      | INHERITS:[
      |   "java.lang.Cloneable"
      | ],
      | METHOD:{
      |   NAME:"bigIntegerValue",
      |   FULL_NAME:"com.amazon.ion.IonFloat.bigIntegerValue:java.math.BigInteger()",
      |   SIGNATURE:"java.math.BigInteger()"
      | },
      | MEMBER:{
      |   NAME:"EMPTY_ARRAY",
      |   TYPE_FULL_NAME:"com.amazon.ion.IonValue"
      | },
      | DEPENDS:[
      |   {
      |     FULL_NAME: "java.lang",
      |     VERSION: "4.1.2"
      |   },
      |   {
      |     FULL_NAME: "java.math",
      |   }
      | ]
      |}""".stripMargin

    "simple struct writer" should {
      "write same output as input from loader" in {
        val typ: Either[String, TypeDecl] = IonTypeLoader.parse(test1)
        typ.isRight shouldEqual true
        val output: String = IonStringWriter.writeToString(typ.right.get)
        output shouldEqual test1
      }
    }
}
