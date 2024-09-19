package typeinfo

import io.shiftleft.semanticcpg.typeinfo.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.util.Try

class IonWriterTests extends AnyWordSpec with Matchers {
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
      | ],
      | DEPENDS:[
      |   {
      |     FULL_NAME: "java.lang",
      |     VERSION: "4.1.2"
      |   }
      | ]
      |}""".stripMargin

  // TODO: why can't ion roundtrip generally? it seems like order of struct fields
  // is nondeterministic?
  "text writer" should {
    "roundtrip without error" in {
      val typ = IonLoader.parse(test1).get
      val result = IonWriter.writeToString(typ).flatMap(IonLoader.parse)
      result.isSuccess shouldEqual true
      result.get shouldEqual typ
    }
  }

  "binary writer" should {
    "roundtrip without error" in {
      val typ = IonLoader.parse(test1).get
      val result = IonWriter.writeToBinaryFormat(typ).flatMap(IonLoader.parse)
      result.isSuccess shouldEqual true
      result.get shouldEqual typ
    }
  }
}
