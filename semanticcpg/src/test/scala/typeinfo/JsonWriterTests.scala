package typeinfo

import io.shiftleft.semanticcpg.typeinfo.*
import io.shiftleft.semanticcpg.typeinfo.util.DataGen
import org.apache.commons.math3.random.RandomDataGenerator
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

class JsonWriterTests extends AnyWordSpec with Matchers {
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
      | ]
      |}""".stripMargin

  "text writer" should {
    "roundtrip without error" in {
      val typ = DataGen.genTypeDecl()
      val result = JsonWriter.writeToString(typ).flatMap(JsonLoader.parse)
      result.isSuccess shouldEqual true
      result.get shouldEqual typ
    }
  }
  
  "binary writer" should {
    "roundtrip without error" in {
      val typ = DataGen.genTypeDecl()
      val result = JsonWriter.writeToBinaryFormat(typ).flatMap(JsonLoader.parse)
      result.isSuccess shouldEqual true
      result.get shouldEqual typ
    }
  }
}
