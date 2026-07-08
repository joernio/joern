package io.joern.lua2cpg

import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths

class BytecodeModelSmokeTest extends AnyWordSpec with Matchers {

  "Lua2Cpg" should {
    "emit bytecode model nodes that survive CPG reopen" in {
      val resourceRoot = Paths.get(getClass.getClassLoader.getResource("bytecode-model").toURI)

      FileUtil.usingTemporaryDirectory("lua2cpg-bytecode-model-smoke") { tmpDir =>
        val outputPath = tmpDir.resolve("bytecode-model.cpg.bin").toString
        val cpg = new Lua2Cpg()
          .createCpg(Config().withInputPath(resourceRoot.toString).withOutputPath(outputPath))
          .get
        cpg.close()

        val reopened = CpgLoader.load(outputPath)
        try {
          reopened.file.nameNot(FileTraversal.UNKNOWN).name.sorted.l should contain allOf (
            "bc-prototype-params/input.luac",
            "bc-constants-call/input.luac",
            "bc-stripped-metadata/input.luac"
          )

          val methodFullNames = reopened.method.fullName.sorted.l
          methodFullNames should contain allOf (
            "lua:bc-prototype-params/input.luac:root",
            "lua:bc-prototype-params/input.luac:root.0",
            "lua:bc-constants-call/input.luac:root",
            "lua:bc-constants-call/input.luac:root.0",
            "lua:bc-stripped-metadata/input.luac:root",
            "lua:bc-stripped-metadata/input.luac:root.0"
          )

          reopened.method
            .fullNameExact("lua:bc-prototype-params/input.luac:root.0")
            .parameter
            .indexGt(0)
            .index
            .sorted
            .l shouldBe List(1, 2)

          val literalCodes = reopened.literal.code.l
          literalCodes should contain allOf ("alpha", "7")

          val diagnostics = reopened.typeDecl
            .name("lua-bytecode-diagnostic")
            .fullName
            .l
          diagnostics should contain allOf (
            "lua:bc-malformed-diagnostic/not-lua-bytecode.bin:diagnostic:not-lua-bytecode",
            "lua:bc-malformed-diagnostic/truncated.luac:diagnostic:truncated-bytecode",
            "lua:bc-malformed-diagnostic/unsupported-version.luac:diagnostic:unsupported-bytecode-version",
            "lua:bc-malformed-diagnostic/unsupported-profile.luac:diagnostic:unsupported-bytecode-profile",
            "lua:bc-malformed-diagnostic/malformed-constant.luac:diagnostic:malformed-constant",
            "lua:bc-stripped-metadata/input.luac:diagnostic:metadata-unavailable"
          )

          methodFullNames.filter(_.startsWith("lua:bc-malformed-diagnostic/")) shouldBe Nil
        } finally {
          reopened.close()
        }
      }
    }
  }
}
