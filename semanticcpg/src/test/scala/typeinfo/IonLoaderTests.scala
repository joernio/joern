package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.loading.Loader
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.util.Try

class IonLoaderTests extends AnyWordSpec with Matchers {
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
      val result: Try[TypeDecl] = IonLoader.parse(test1)
      result.isSuccess shouldEqual true
      
      val t = result.get
      t.fullName shouldEqual "com.amazon.ion.IonFloat"
      t.name shouldEqual "IonFloat"
      t.typeParams shouldBe empty
      t.inherits should have length 1
      t.inherits should contain ("java.lang.Cloneable")
      
      t.methods should have length 1
      val method = t.methods.head
      method.name shouldBe "bigIntegerValue"
      method.fullName shouldBe "com.amazon.ion.IonFloat.bigIntegerValue:java.math.BigInteger()"
      method.signature shouldBe "java.math.BigInteger()"
      
      t.members should have length 1
      val member = t.members.head
      member.name shouldBe "EMPTY_ARRAY"
      member.typeFullName shouldBe "com.amazon.ion.IonValue"
    }
  }
}