package io.shiftleft.semanticcpg.typeinfo

import io.shiftleft.semanticcpg.typeinfo.{PackageMetadata, TypeMetadata}
import io.shiftleft.semanticcpg.typeinfo.loading.MetadataIonTextLoader
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MetadataIonTestLoaderTests extends AnyWordSpec with Matchers {
  private val test1: Array[Byte] =
    """
      |{
      |  VERSIONS: ["1.0.0", "1.11.9", "1.11.9-rc2"],
      |  TYPE_NAME_INFO: [
      |    {
      |      VERSION: "1.0.0",
      |      TYPE_NAMES: ["IonFloat", "IonType"]
      |    },
      |    {
      |      VERSION: "1.11.9",
      |      TYPE_NAMES: ["IonFloat"]
      |    },
      |    {
      |      VERSION: "1.11.9-rc2",
      |      TYPE_NAMES: ["IonFloat"]
      |    }
      |  ]
      |}""".stripMargin.getBytes("UTF-8")

  "test1" should {
    "read into PackageMetadata case class without errors" in {
      val packageMetadata = MetadataIonTextLoader.loadFromBytes(test1)

      val expected = PackageMetadata(
        List("1.0.0", "1.11.9", "1.11.9-rc2"),
        List(
          TypeMetadata("1.0.0", List("IonFloat", "IonType")),
          TypeMetadata("1.11.9", List("IonFloat")),
          TypeMetadata("1.11.9-rc2", List("IonFloat"))
        )
      )

      packageMetadata shouldEqual expected
    }
  }
}
