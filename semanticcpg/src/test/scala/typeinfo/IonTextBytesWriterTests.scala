package typeinfo

import io.shiftleft.semanticcpg.typeinfo.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.util.Try

import java.io.ByteArrayInputStream

class IonTextBytesWriterTests extends AnyWordSpec with Matchers {
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

  // TODO: why can't ion roundtrip generally? it seems like order of struct fields
  // is nondeterministic?
  "text writer" should {
    "roundtrip without error" in {
      val typ    = TypeInfoIonTextLoader.loadFromString(test1)
      val byteStream = ByteArrayInputStream(IonTextBytesWriter.write(typ))
      val result = TypeInfoIonTextLoader.loadFromStream(byteStream)
      result shouldEqual typ
    }
  }
}
