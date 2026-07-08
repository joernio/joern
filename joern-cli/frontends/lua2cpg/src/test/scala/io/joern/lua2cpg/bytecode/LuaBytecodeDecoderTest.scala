package io.joern.lua2cpg.bytecode

import io.joern.lua2cpg.bytecode.LuaConstantValue.NumberValue
import io.joern.lua2cpg.bytecode.LuaConstantValue.StringValue
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LuaBytecodeDecoderTest extends AnyWordSpec with Matchers {

  "LuaBytecodeDecoder" should {
    "decode nested prototype identity and parameter counts" in {
      val result = decodeResource("bytecode-model/bc-prototype-params/input.luac")
      val root   = acceptedRoot(result)

      root.nested.size.shouldBe(1)
      val nestedPrototype = root.nested.head
      nestedPrototype.prototypeId.shouldBe("root.0")
      nestedPrototype.parentPrototypeId.shouldBe(Some("root"))
      nestedPrototype.numParams.shouldBe(2)
    }

    "decode string and number constants used by call fixtures" in {
      val result = decodeResource("bytecode-model/bc-constants-call/input.luac")
      val root   = acceptedRoot(result)

      val constants = root.constants.map(constant => constant.luaType -> constant.value)
      constants.contains("string" -> StringValue("alpha")).shouldBe(true)
      constants.contains("number" -> NumberValue(7.0)).shouldBe(true)
    }

    "accept stripped metadata bytecode while preserving structural bytecode facts" in {
      val result = decodeResource("bytecode-model/bc-stripped-metadata/input.luac")
      val root   = acceptedRoot(result)

      root.prototypeId.shouldBe("root")
      root.nested.map(_.prototypeId).contains("root.0").shouldBe(true)
      root.constants
        .map(constant => constant.luaType -> constant.value)
        .contains("string" -> StringValue("metadata"))
        .shouldBe(true)
      root.locals.isEmpty.shouldBe(true)
      root.upvalueNames.isEmpty.shouldBe(true)
    }

    "return diagnostics without accepted prototype models for malformed inputs" in {
      val cases = Seq(
        "not-lua-bytecode.bin"       -> "not-lua-bytecode",
        "truncated.luac"            -> "truncated-bytecode",
        "unsupported-version.luac"  -> "unsupported-bytecode-version",
        "unsupported-profile.luac"  -> "unsupported-bytecode-profile",
        "malformed-constant.luac"   -> "malformed-constant"
      )

      cases.foreach { case (fileName, expectedKind) =>
        val result = decodeResource(s"bytecode-model/bc-malformed-diagnostic/$fileName")

        result.artifact.accepted.shouldBe(false)
        result.artifact.diagnostic.kind.shouldBe(expectedKind)
        result.artifact.diagnostic.severity.shouldBe("error")
        result.artifact.diagnostic.successFactsAllowed.shouldBe(false)
        result.root.shouldBe(None)
      }
    }
  }

  private def acceptedRoot(result: LuaBytecodeDecodeResult): LuaPrototype = {
    result.artifact.accepted.shouldBe(true)
    result.artifact.diagnostic.kind.shouldBe("accepted")
    result.artifact.diagnostic.successFactsAllowed.shouldBe(true)
    result.profile.isDefined.shouldBe(true)
    result.root.get
  }

  private def decodeResource(path: String): LuaBytecodeDecodeResult =
    LuaBytecodeDecoder.decode(path, readResourceBytes(path))

  private def readResourceBytes(path: String): Array[Byte] = {
    val stream = Option(getClass.getClassLoader.getResourceAsStream(path)).getOrElse {
      fail(s"Missing lua2cpg bytecode test resource: $path")
    }
    try stream.readAllBytes()
    finally stream.close()
  }
}
