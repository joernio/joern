package io.joern.swiftsrc2cpg.io

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

class SwiftCompilerTests extends AnyWordSpec with Matchers {

  private object SwiftCompilerTestsFixture {
    private val PackageSwiftContent: String =
      """
        |// swift-tools-version: 5.10
        |
        |import PackageDescription
        |
        |let package = Package(
        |  name: "SwiftHelloWorld",
        |  platforms: [
        |    .macOS(.v10_15)
        |  ],
        |  products: [
        |    .library(name: "SwiftHelloWorldLib", targets: ["SwiftHelloWorldLib"])
        |  ],
        |  targets: [
        |    .target(
        |      name: "SwiftHelloWorldLib",
        |      swiftSettings: [
        |        .unsafeFlags(["-warnings-as-errors"])
        |      ]
        |    ),
        |    .executableTarget(
        |      name: "SwiftHelloWorld",
        |      dependencies: [ "SwiftHelloWorldLib" ],
        |      swiftSettings: [
        |        .unsafeFlags(["-warnings-as-errors"])
        |      ]
        |    )
        |  ]
        |)
        |""".stripMargin

    private val HelloWorldSwiftContent =
      """
        |import Foundation
        |
        |public class HelloWorld {
        |
        |	private let greeting: String = "Hello World"
        |	private let suffix: String = "!"
        |
        |	public init() {}
        |
        |	public func greet(from name: String) {
        |		print(greeting + " from " + name + suffix)
        |	}
        |
        |}
        |""".stripMargin

    private val MainSwiftContent: String =
      """
        |import Foundation
        |import SwiftHelloWorldLib
        |
        |@main
        |struct Main {
        |	static func main() -> Void {
        |		let greeter = HelloWorld()
        |		greeter.greet(from: "me")
        |	}
        |
        |}
        |""".stripMargin

    val SwiftcInvocation: String =
      """
        |/usr/bin/swiftc
        | -module-name CodeGeneration
        | -emit-dependencies
        | -emit-module
        | -emit-module-path /tmp/project/.build/debug/Modules/CodeGeneration.swiftmodule
        | -output-file-map /tmp/project/.build/debug/CodeGeneration.build/output-file-map.json
        | -parse-as-library -incremental -c @/tmp/project/.build/debug/CodeGeneration.build/sources
        | -I /tmp/project/.build/debug/Modules
        | -target x86_64-unknown-windows-msvc
        | -v
        | -enable-batch-mode
        | -index-store-path /tmp/project/.build/debug/index/store
        | -Onone
        | -enable-testing
        | -j16
        | -DSWIFT_PACKAGE
        | -DDEBUG
        | -module-cache-path /tmp/project/.build/debug/ModuleCache
        | -parseable-output
        | -parse-as-library
        | -static
        | -swift-version 5
        | -sdk /some/SDK/
        | -libc MD
        | -I /path/to/XCTest/usr/lib/swift/windows
        | -I /path/to/XCTest/usr/lib/swift/windows/x86_64
        | -L /path/to/XCTest/usr/lib/swift/windows/x86_64
        | -I /path/to/Testing/usr/lib/swift/windows
        | -L /path/to/Testing/usr/lib/swift/windows/x86_64
        | -use-ld=lld -g -use-ld=lld -Xcc -D_MT -Xcc -D_DLL -Xcc -Xclang -Xcc --dependent-lib=msvcrt -Xcc -gdwarf -package-name swiftastgen""".stripMargin
        .replace("\n", "")

    val ExpectedSwiftcInvocations: Seq[Seq[String]] = Seq(
      Seq(
        "/usr/bin/swiftc",
        "-module-name",
        "CodeGeneration",
        "-parse-as-library",
        "-c",
        "@/tmp/project/.build/debug/CodeGeneration.build/sources",
        "-I",
        "/tmp/project/.build/debug/Modules",
        "-target",
        "x86_64-unknown-windows-msvc",
        "-enable-batch-mode",
        "-index-store-path",
        "/tmp/project/.build/debug/index/store",
        "-Onone",
        "-enable-testing",
        "-j16",
        "-DSWIFT_PACKAGE",
        "-DDEBUG",
        "-module-cache-path",
        "/tmp/project/.build/debug/ModuleCache",
        "-parse-as-library",
        "-static",
        "-swift-version",
        "5",
        "-sdk",
        "/some/SDK",
        "-libc",
        "MD",
        "-I",
        "/path/to/XCTest/usr/lib/swift/windows",
        "-I",
        "/path/to/XCTest/usr/lib/swift/windows/x86_64",
        "-L",
        "/path/to/XCTest/usr/lib/swift/windows/x86_64",
        "-I",
        "/path/to/Testing/usr/lib/swift/windows",
        "-L",
        "/path/to/Testing/usr/lib/swift/windows/x86_64",
        "-use-ld=lld",
        "-g",
        "-use-ld=lld",
        "-Xcc",
        "-D_MT",
        "-Xcc",
        "-D_DLL",
        "-Xcc",
        "-Xclang",
        "-Xcc",
        "--dependent-lib=msvcrt",
        "-Xcc",
        "-gdwarf",
        "-package-name",
        "swiftastgen",
        "-dump-ast",
        "-dump-ast-format",
        "json"
      )
    )

    def apply(f: Path => Unit): Unit = {
      FileUtil.usingTemporaryDirectory("swiftsrc2cpgTests") { tmpDir =>
        val packageSwift = tmpDir / "Package.swift"
        Files.writeString(packageSwift, PackageSwiftContent)

        val helloWorldSwift = tmpDir / "Sources" / "SwiftHelloWorldLib" / "HelloWorldSwift.swift"
        helloWorldSwift.createWithParentsIfNotExists(createParents = true)
        Files.writeString(helloWorldSwift, HelloWorldSwiftContent)

        val mainSwift = tmpDir / "Sources" / "SwiftHelloWorld" / "Main.swift"
        mainSwift.createWithParentsIfNotExists(createParents = true)
        Files.writeString(mainSwift, MainSwiftContent)

        val xCodeOut = tmpDir / "out.log"
        Files.writeString(xCodeOut, SwiftcInvocation)

        f(tmpDir)
      }
    }
  }

  "Processing the compile output" should {

    "parsing the swiftc arguments correctly" in {
      val provider = SwiftTypesProvider.build(Config(), Seq(SwiftCompilerTestsFixture.SwiftcInvocation))
      provider.parsedSwiftcInvocations shouldBe SwiftCompilerTestsFixture.ExpectedSwiftcInvocations
    }

    "handle a SwiftPM project correctly" in SwiftCompilerTestsFixture { dir =>
      val config             = Config().withInputPath(dir.toString).withOutputPath(dir.toString)
      val swiftTypesProvider = SwiftTypesProvider.apply(config)
      swiftTypesProvider match {
        case Some(provider) =>
          val Seq(swiftHelloWorldLibCommand, swiftHelloWorldCommand) = provider.parsedSwiftcInvocations
          swiftHelloWorldLibCommand.exists(c => c.endsWith("/swiftc") || c.endsWith("\\swiftc.exe")) shouldBe true
          swiftHelloWorldLibCommand should contain("-module-name")
          swiftHelloWorldLibCommand should contain("SwiftHelloWorldLib")
          swiftHelloWorldLibCommand should contain("-parse-as-library")
          swiftHelloWorldLibCommand should contain("-c")
          swiftHelloWorldLibCommand should contain("-I")
          swiftHelloWorldLibCommand should contain("-target")
          swiftHelloWorldLibCommand should contain("-parse-as-library")
          swiftHelloWorldLibCommand should contain("-swift-version")
          swiftHelloWorldLibCommand should contain("-sdk")
          swiftHelloWorldLibCommand should contain("-L")
          swiftHelloWorldLibCommand should contain("-dump-ast")
          swiftHelloWorldLibCommand should contain("-dump-ast-format")
          swiftHelloWorldLibCommand should contain("json")

          swiftHelloWorldLibCommand.exists(c => c.endsWith("/swiftc") || c.endsWith("\\swiftc.exe")) shouldBe true
          swiftHelloWorldCommand should contain("-module-name")
          swiftHelloWorldCommand should contain("SwiftHelloWorld")
          swiftHelloWorldCommand should contain("-parse-as-library")
          swiftHelloWorldCommand should contain("-c")
          swiftHelloWorldCommand should contain("-I")
          swiftHelloWorldCommand should contain("-target")
          swiftHelloWorldCommand should contain("-parse-as-library")
          swiftHelloWorldCommand should contain("-swift-version")
          swiftHelloWorldCommand should contain("-sdk")
          swiftHelloWorldCommand should contain("-L")
          swiftHelloWorldCommand should contain("-dump-ast")
          swiftHelloWorldCommand should contain("-dump-ast-format")
          swiftHelloWorldCommand should contain("json")

          provider.retrieveDeclFullnameMapping() shouldBe Map(
            "s:15SwiftHelloWorld4MainV4mainyyFZ"                                            -> "fullname",
            "s:18SwiftHelloWorldLib0bC0CACycfc"                                             -> "fullname",
            "s:18SwiftHelloWorldLib0bC0C8greeting33_C6D5E4A96804CD03B7512662F178D1D8LLSSvg" -> "fullname",
            "s:15SwiftHelloWorld4MainVACycfc"                                               -> "fullname",
            "s:18SwiftHelloWorldLib0bC0C"                                                   -> "fullname",
            "s:18SwiftHelloWorldLib0bC0Cfd"                                                 -> "fullname",
            "s:15SwiftHelloWorld4MainV"                                                     -> "fullname",
            "s:18SwiftHelloWorldLib0bC0C8greeting33_C6D5E4A96804CD03B7512662F178D1D8LLSSvp" -> "fullname",
            "s:15SwiftHelloWorld4MainV5$mainyyFZ"                                           -> "fullname",
            "s:18SwiftHelloWorldLib0bC0C5greet4fromySS_tF"                                  -> "fullname",
            "s:18SwiftHelloWorldLib0bC0C6suffix33_C6D5E4A96804CD03B7512662F178D1D8LLSSvp"   -> "fullname",
            "s:18SwiftHelloWorldLib0bC0C6suffix33_C6D5E4A96804CD03B7512662F178D1D8LLSSvg"   -> "fullname"
          )
        case None => fail("Can't build the SwiftTypesProvider")
      }
    }

    "handle a Xcode project correctly with a given compiler log file" in SwiftCompilerTestsFixture { dir =>
      val config = Config().withInputPath(dir.toString).withOutputPath(dir.toString).withXcodeOutput(dir / "out.log")
      val swiftTypesProvider = SwiftTypesProvider.apply(config)
      swiftTypesProvider match {
        case Some(provider) =>
          provider.parsedSwiftcInvocations shouldBe SwiftCompilerTestsFixture.ExpectedSwiftcInvocations
        case None => fail("Can't build the SwiftTypesProvider")
      }
    }

  }

}
