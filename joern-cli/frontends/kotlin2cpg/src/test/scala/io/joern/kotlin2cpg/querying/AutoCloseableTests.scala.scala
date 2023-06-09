package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, AnnotationLiteral}
import io.shiftleft.semanticcpg.language._

class AutoCloseableTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with AutoCloseable interface" should {
    val cpg = code("""
        |interface XMLWriter : AutoCloseable {
        |    fun document(encoding: String, version: String, content: XMLWriter.() -> Unit)
        |    fun element(name: String, content: XMLWriter.() -> Unit)
        |    fun attribute(name: String, value: String)
        |    fun text(value: String)
        |}
        |
        |fun writeBooksTo(writer: XMLWriter) {
        |    writer.use { xml ->
        |        xml.document(encoding = "UTF-8", version = "1.0") {
        |            element("bookstore") {
        |                element("book") {
        |                    attribute("category", "fiction")
        |                    element("title") { text("Harry Potter and the Prisoner of Azkaban") }
        |                }
        |                element("book") {
        |                    attribute("category", "programming")
        |                    element("title") { text("Kotlin in Action") }
        |                }
        |            }
        |        }
        |    }
        |}
        |""".stripMargin)

    "should find method with the right parameter type" in {
      cpg.method("writeBooksTo").parameter.typeFullName.toSet shouldBe Set("XMLWriter")
    }

    "should have AutoCloseable in typeDecl" in {
      cpg.typeDecl("AutoCloseable").toSet shouldBe Set("AutoCloseable")
    }
  }
}
