package io.joern.swiftsrc2cpg.testfixtures

import io.joern.swiftsrc2cpg.Config
import io.joern.x2cpg.testfixtures.Code2CpgFixture

class SwiftCompilerSrc2CpgSuite(fileSuffix: String = ".swift")
    extends Code2CpgFixture(() => new SwiftCompilerTestCpg(fileSuffix)) {

  private val PackageSwiftContent = """
      |// swift-tools-version: 6.1
      |// The swift-tools-version declares the minimum version of Swift required to build this package.
      |import PackageDescription
      |let package = Package(
      |    name: "SwiftTest",
      |    targets: [
      |        .executableTarget(name: "SwiftTest"),
      |    ]
      |)""".stripMargin

  private val TestFilename         = "SwiftTest/Sources/main.swift"
  private val PackageSwiftFilename = "SwiftTest/Package.swift"

  override def code(code: String): SwiftCompilerTestCpg = super.code(code, TestFilename)

  protected def codeWithSwiftSetup(codeString: String): SwiftCompilerTestCpg = {
    code(codeString)
      .moreCode(PackageSwiftContent, PackageSwiftFilename)
      .withConfig(Config(swiftBuild = true))
  }

}
