package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class UnicodeWithCompilerTests extends SwiftCompilerSrc2CpgSuite {

  "UnicodeWithCompilerTests" should {

    "be correct with unicode embedded" in {
      val cpg = codeWithSwiftSetup("""
         |struct Factory {
         |  var unicode: String = "✓ Hello, World! ✓"
         |  var emoticon: String = "😊"
         |  var combined: String = "✓😊 Hello, World! 😊✓"
         |  var party: String = "🥳🎉"
         |  var numbers: String = "1️⃣2️⃣3️⃣4️⃣5️⃣"
         |  var chinese: String = "你好，世界！"
         |  var arabic: String = "مرحبا بالعالم!"
         |  var spanish: String = "¡Hola, Mundo!"
         |  var greek: String = "Γειά σου, Κόσμε!"
         |  var portuguese: String = "Olá, Mundo!"
         |  var comment: String = "This is a comment with emojis! 🚀💻"
         |}
         |
         |extension Factory {
         |  static func id(x: Int) -> Int {
         |    print("😊Hello, World! 😊")
         |    return x
         |  }
         |}
         |func main(source: Int) {
         |  // ✓ This works
         |  Factory.id(x: source)
         |}
         |""".stripMargin)
      val List(idCall) = cpg.call("id").l
      idCall.code shouldBe "Factory.id(x: source)"
      idCall.methodFullName shouldBe "SwiftTest.Factory<extension>.id:(x:Swift.Int)->Swift.Int"
      val List(idMethod) = cpg.method("id").l
      idMethod.fullName shouldBe "SwiftTest.Factory<extension>.id:(x:Swift.Int)->Swift.Int"

      cpg.typeDecl.nameExact("Factory").ast.isLiteral.code.l shouldBe List(
        "\"✓ Hello, World! ✓\"",
        "\"😊\"",
        "\"✓😊 Hello, World! 😊✓\"",
        "\"🥳🎉\"",
        "\"1️⃣2️⃣3️⃣4️⃣5️⃣\"",
        "\"你好，世界！\"",
        "\"مرحبا بالعالم!\"",
        "\"¡Hola, Mundo!\"",
        "\"Γειά σου, Κόσμε!\"",
        "\"Olá, Mundo!\"",
        "\"This is a comment with emojis! 🚀💻\""
      )
    }

  }

}
