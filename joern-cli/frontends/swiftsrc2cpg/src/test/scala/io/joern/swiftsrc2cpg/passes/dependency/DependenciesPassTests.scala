package io.joern.swiftsrc2cpg.passes.dependency

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language._

class DependenciesPassTests extends SwiftSrc2CpgSuite {

  private val codeString = """
     |// swift-tools-version: 5.9
     |
     |import PackageDescription
     |
     |let package = Package(
     |  name: "TestApp",
     |  platforms: [],
     |  products: [],
     |  dependencies: [
     |    .package(name: "DepA", path: "PathA"),
     |    .package(url: "https://github.com/DepB", from: "1.0.0"),
     |    .package(url: "https://github.com/DepC", "1.2.3"..<"1.2.6"),
     |    .package(url: "https://github.com/DepD", "1.2.3"..."1.2.6"),
     |    .package(url: "https://github.com/DepE", branch: "main"),
     |    .package(url: "https://github.com/DepF", revision: "aa681bd6c61e22df0fd808044a886fc4a7ed3a65"),
     |    .package(url: "https://github.com/DepG", exact: "1.2.3"),
     |    .package(path: "../some/path/DepH")
     |  ],
     |  targets: []
     |)
     |}""".stripMargin

  private val cpg = code(codeString, "Package.swift")

  "DependenciesPass" should {

    "generate dependency nodes correctly" in {
      inside(cpg.dependency.l) { case List(depA, depB, depC, depD, depE, depF, depG, depH) =>
        depA.name shouldBe "DepA"
        depA.version shouldBe "<empty>"
        depA.dependencyGroupId shouldBe Some("PathA")

        depB.name shouldBe "https://github.com/DepB"
        depB.version shouldBe "1.0.0"
        depB.dependencyGroupId shouldBe None

        depC.name shouldBe "https://github.com/DepC"
        depC.version shouldBe "1.2.3..<1.2.6"
        depC.dependencyGroupId shouldBe None

        depD.name shouldBe "https://github.com/DepD"
        depD.version shouldBe "1.2.3...1.2.6"
        depD.dependencyGroupId shouldBe None

        depE.name shouldBe "https://github.com/DepE"
        depE.version shouldBe "main"
        depE.dependencyGroupId shouldBe None

        depF.name shouldBe "https://github.com/DepF"
        depF.version shouldBe "aa681bd6c61e22df0fd808044a886fc4a7ed3a65"
        depF.dependencyGroupId shouldBe None

        depG.name shouldBe "https://github.com/DepG"
        depG.version shouldBe "1.2.3"
        depG.dependencyGroupId shouldBe None

        depH.name shouldBe "../some/path/DepH"
        depH.version shouldBe "<empty>"
        depH.dependencyGroupId shouldBe None
      }
    }

  }

}
