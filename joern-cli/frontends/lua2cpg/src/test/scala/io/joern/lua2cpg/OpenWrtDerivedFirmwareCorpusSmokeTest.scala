package io.joern.lua2cpg

import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

class OpenWrtDerivedFirmwareCorpusSmokeTest extends AnyWordSpec with Matchers {

  "Lua2Cpg" should {
    "analyze the OpenWrt-derived Lua corpus and export decoder evidence" in {
      val resourceRoot =
        Paths.get(getClass.getClassLoader.getResource("openwrt-derived-firmware-lua/usr/lib/lua").toURI)

      FileUtil.usingTemporaryDirectory("lua2cpg-openwrt-derived-corpus-smoke") { tmpDir =>
        val outputPath = tmpDir.resolve("openwrt-derived-firmware-lua.cpg.bin").toString
        val exportDir  = tmpDir.resolve("openwrt-derived-firmware-lua-evidence")
        val cpg = new Lua2Cpg()
          .createCpg(
            Config(realFirmwareOutputDir = Some(exportDir.toString))
              .withInputPath(resourceRoot.toString)
              .withOutputPath(outputPath)
          )
          .get
        cpg.close()

        Files.isRegularFile(Paths.get(outputPath)) shouldBe true

        val reopened = CpgLoader.load(outputPath)
        try {
          reopened.metaData.language.l shouldBe List("LUA")
          reopened.file.nameNot(FileTraversal.UNKNOWN).name.l should contain allOf (
            "luci/http.lua",
            "luci/http.luac",
            "luci/controller/mtkwifi.lua",
            "luci/controller/mtkwifi.luac",
            "mtkwifi.lua",
            "mtkwifi.luac"
          )
        } finally {
          reopened.close()
        }

        val decoderTotals = ujson.read(Files.readString(exportDir.resolve("decoder-summary.json"))).obj("totals").obj
        decoderTotals("input_count").num.toInt shouldBe 42
        decoderTotals("decoded_count").num.toInt shouldBe 42
        decoderTotals("diagnostic_count").num.toInt shouldBe 0

        val stagingStream = Files.list(exportDir.resolve("staging"))
        try {
          stagingStream.iterator.asScala.count(Files.isRegularFile(_)) shouldBe 42
        } finally {
          stagingStream.close()
        }
      }
    }
  }
}
