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
      val resourceRoot = Paths.get(getClass.getClassLoader.getResource("bytecode-model").toURI).getParent

      FileUtil.usingTemporaryDirectory("lua2cpg-bytecode-model-smoke") { tmpDir =>
        val outputPath = tmpDir.resolve("bytecode-model.cpg.bin").toString
        val cpg = new Lua2Cpg()
          .createCpg(Config().withInputPath(resourceRoot.toString).withOutputPath(outputPath))
          .get
        cpg.close()

        val reopened = CpgLoader.load(outputPath)
        try {
          reopened.file.nameNot(FileTraversal.UNKNOWN).name.sorted.l should contain allOf (
            "bytecode-model/bc-prototype-params/input.luac",
            "bytecode-model/bc-constants-call/input.luac",
            "bytecode-model/bc-stripped-metadata/input.luac"
          )

          // Expected rows are anchored in the committed bytecode fixtures for
          // bc-prototype-params, bc-constants-call, bc-stripped-metadata, and bc-malformed-diagnostic.
          val methodFullNames = reopened.method.fullName.sorted.l
          methodFullNames should contain allOf (
            "lua:bytecode-model/bc-prototype-params/input.luac:root",
            "lua:bytecode-model/bc-prototype-params/input.luac:root.0",
            "lua:bytecode-model/bc-constants-call/input.luac:root",
            "lua:bytecode-model/bc-constants-call/input.luac:root.0",
            "lua:bytecode-model/bc-stripped-metadata/input.luac:root",
            "lua:bytecode-model/bc-stripped-metadata/input.luac:root.0"
          )

          reopened.method
            .fullNameExact("lua:bytecode-model/bc-prototype-params/input.luac:root.0")
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
            "lua:bytecode-model/bc-malformed-diagnostic/not-lua-bytecode.luac:diagnostic:not-lua-bytecode",
            "lua:bytecode-model/bc-malformed-diagnostic/truncated.luac:diagnostic:truncated-bytecode",
            "lua:bytecode-model/bc-malformed-diagnostic/unsupported-version.luac:diagnostic:unsupported-bytecode-version",
            "lua:bytecode-model/bc-malformed-diagnostic/unsupported-profile.luac:diagnostic:unsupported-bytecode-profile",
            "lua:bytecode-model/bc-malformed-diagnostic/malformed-constant.luac:diagnostic:malformed-constant",
            "lua:bytecode-model/bc-stripped-metadata/input.luac:diagnostic:metadata-unavailable"
          )
          diagnostics.exists(_.contains("not-lua-bytecode.bin")) shouldBe false

          methodFullNames.filter(_.startsWith("lua:bytecode-model/bc-malformed-diagnostic/")) shouldBe Nil
        } finally {
          reopened.close()
        }
      }
    }
  }
}
