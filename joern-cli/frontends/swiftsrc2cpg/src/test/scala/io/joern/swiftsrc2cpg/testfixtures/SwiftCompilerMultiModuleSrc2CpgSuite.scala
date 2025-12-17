package io.joern.swiftsrc2cpg.testfixtures

import io.joern.swiftsrc2cpg.Config
import io.joern.x2cpg.testfixtures.Code2CpgFixture

class SwiftCompilerMultiModuleSrc2CpgSuite(fileSuffix: String = ".swift")
    extends Code2CpgFixture(() => new SwiftCompilerTestCpg(fileSuffix)) {

  private val PackageSwiftContent = """
      |// swift-tools-version: 6.1
      |// The swift-tools-version declares the minimum version of Swift required to build this package.
      |import PackageDescription
      |
      |let package = Package(
      |    name: "SwiftTest",
      |    products: [
      |        .library(name: "ModuleA", targets: ["ModuleA"]),
      |        .library(name: "ModuleB", targets: ["ModuleB"])
      |    ],
      |    targets: [
      |        .target(
      |            name: "ModuleA"
      |        ),
      |        .target(
      |            name: "ModuleB",
      |            dependencies: ["ModuleA"]
      |        ),
      |       .executableTarget(
      |            name: "SwiftTest",
      |            dependencies: ["ModuleA", "ModuleB"]
      |       )
      |    ]
      |)""".stripMargin

  private val PackageSwiftFilename = "SwiftTest/Package.swift"

  protected def codeWithSwiftSetup(codeString: String, fileName: String): SwiftCompilerTestCpg = {
    code(codeString, fileName)
      .moreCode(PackageSwiftContent, PackageSwiftFilename)
      .withConfig(Config(swiftBuild = true))
  }

}
