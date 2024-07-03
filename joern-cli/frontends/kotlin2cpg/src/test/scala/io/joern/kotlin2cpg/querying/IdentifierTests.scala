package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class IdentifierTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with two simple methods" should {
    val cpg = code("""
        |fun add(x: Int, y: Int): Int {
        |  return x + y
        |}
        |
        |fun main(args : Array<String>) {
        |  val argc: Int = args.size
        |  println(add(argc, 1))
        |}
        |""".stripMargin)

    "should contain the correct number of IDENTIFIER nodes" in {
      cpg.identifier.size shouldBe 5
    }

    "IDENTIFIER nodes have the correct CODE property set" in {
      cpg.identifier.code.l.toSet shouldBe Set("x", "y", "argc", "args")
    }

    "IDENTIFIER nodes have the correct LINE_NUMBER property set" in {
      cpg.identifier("x").lineNumber.l.head shouldBe 3
      cpg.identifier("y").lineNumber.l.head shouldBe 3
      cpg.identifier("argc").lineNumber.l.head shouldBe 7
      cpg.identifier("args").lineNumber.l.head shouldBe 7
    }

    "IDENTIFIER nodes have the correct COLUMN_NUMBER property set" in {
      cpg.identifier("x").columnNumber.l.head shouldBe 9
      cpg.identifier("y").columnNumber.l.head shouldBe 13
      cpg.identifier("argc").columnNumber.l.head shouldBe 6
      cpg.identifier("args").columnNumber.l.head shouldBe 18
    }
  }

  "CPG for code with various missing types, but some information available in the imports" should {
    val cpg = code("""package io.vrooom.vulnerableapp
       |
       |import android.os.Bundle
       |import android.support.v7.app.AppCompatActivity
       |import android.webkit.WebView
       |import android.content.IntentFilter
       |
       |class MainActivity : AppCompatActivity() {
       |    override fun onCreate(savedInstanceState: Bundle?) {
       |        super.onCreate(savedInstanceState)
       |        setContentView(R.layout.activity_main)
       |
       |        val filter: IntentFilter = IntentFilter()
       |        filter.addAction(packageName + "io.vrooom.intent.action.WRITE_FILE")
       |        val receiver = WriteFileBroadcastReceiver()
       |        registerReceiver(receiver, filter)
       |    }
       |}
       |""".stripMargin)

    "contain a LOCAL node with the TYPE_FULL_NAME taken from the imports" in {
      cpg.identifier("filter").typeFullName.l.head shouldBe "android.content.IntentFilter"
    }

    "contain an IDENTIFIER node with the TYPE_FULL_NAME taken from the LOCAL node" in {
      cpg.call
        .code("registerReceiver.*")
        .argument
        .codeExact("filter")
        .isIdentifier
        .typeFullName
        .head shouldBe "android.content.IntentFilter"
    }
  }
}
