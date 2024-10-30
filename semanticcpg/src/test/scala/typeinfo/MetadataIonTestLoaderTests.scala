package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.PackageMetadata
import io.shiftleft.semanticcpg.typeinfo.loading.MetadataIonTextLoader
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MetadataIonTestLoaderTests extends AnyWordSpec with Matchers {
  private val test1: Array[Byte] =
    """
      |{
      |  VERSIONS: ["1.0.0", "1.11.9", "1.11.9-rc2"],
      |}""".stripMargin.getBytes("UTF-8")

  "test1" should {
    "read into PackageMetadata case class without errors" in {
      val packageMetadata = MetadataIonTextLoader.loadFromBytes(test1)
      val expected = PackageMetadata(List("1.0.0", "1.11.9", "1.11.9-rc2"))
      packageMetadata shouldEqual expected
    }
  }
}
