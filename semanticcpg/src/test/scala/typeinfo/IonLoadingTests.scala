package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.Loader
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Duration

class IonLoadingTests extends AnyWordSpec with Matchers {
  private val test1: String = """
      |{
      | FULL_NAME: "com.amazon.ion.IonReader",
      | NAME: "IonReader",
      | MEMBER: {
      | },
      |}
      |""".stripMargin
    
    "simple struct reader" should {
      "read into object without errors" in {
        val txt = Loader.readFromString(test1)
        txt shouldBe "hello"
      }
    }
}