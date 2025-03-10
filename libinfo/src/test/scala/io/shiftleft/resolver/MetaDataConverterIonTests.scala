package io.shiftleft.resolver

import com.amazon.ion.system.IonReaderBuilder
import io.shiftleft.resolver.api.Coordinate
import io.shiftleft.resolver.impl.{CoordinateConverterIon, IdConverterIonMaven, IdMaven, MetaDataConverterIon}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MetaDataConverterIonTests extends AnyWordSpec with Matchers {
  "foo" in {
    val testData =
      """|{
         |  id: { kind: mvn, groupId: "some.group", artifactId: "a.b" },
         |  version: "1.0",
         |  dependencies: [
         |    { id: { kind: mvn, groupId: "other.group", artifactId: "c.d" }, version: "1.2" },
         |  ]
         |}
         |""".stripMargin
    val converter = new MetaDataConverterIon(CoordinateConverterIon(IdConverterIonMaven()))
    val reader = IonReaderBuilder.standard().build(testData)
    val metaData = converter.readMetaData(reader)
    metaData.coordinate.id.groupId shouldBe "some.group"
    metaData.coordinate.id.artifactId shouldBe "a.b"
    metaData.directDeps should contain theSameElementsAs Vector(
      Coordinate(IdMaven("other.group", "c.d"), "1.2")
    )
  }
}
