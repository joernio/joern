package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class NamespaceTests extends CSharpCode2CpgFixture {

  "a basic file with namespace" should {

    val cpg = code(basicBoilerplate(), "Program.cs")

    "create a namespace block node" in {
      val helloWorld = cpg.namespaceBlock.nameExact("HelloWorld").head
      helloWorld.code shouldBe "namespace HelloWorld"
      helloWorld.filename shouldBe "Program.cs"
      helloWorld.lineNumber shouldBe Some(2)
      helloWorld.columnNumber shouldBe Some(1)
      helloWorld.fullName shouldBe "HelloWorld"
    }

  }

  "a basic file with namespace that has a dot separator" should {

    val cpg = code(basicBoilerplate(namespace = "Hello.World"), "Program.cs")

    "create a namespace block node" in {
      val helloWorld = cpg.namespaceBlock.nameExact("World").head
      helloWorld.code shouldBe "namespace Hello.World"
      helloWorld.filename shouldBe "Program.cs"
      helloWorld.fullName shouldBe "Hello.World"
    }

  }

  "a file with three namespaces" should {
    val cpg = code(
      """
        |namespace N1.N2
        |{
        |    class A {}
        |}
        |
        |namespace N1.N3
        |{
        |    class B {}
        |}
        |""".stripMargin,
      "Program.cs"
    )

    "link N1.N2 and N1.N3 under Program.cs" in {
      val List(n2, n3) = cpg.namespaceBlock.nameExact("N2", "N3").l
      n2.name shouldBe "N2"
      n2.fullName shouldBe "N1.N2"
      n3.name shouldBe "N3"
      n3.fullName shouldBe "N1.N3"
      n2.filename shouldBe "Program.cs"
      n2.filename shouldBe n3.filename
      n2.file.head shouldBe n3.file.head
    }

    "create three namespaces under a single file" ignore {
      // TODO: The presence of N1 is implicit and needs to be handled separately
      val List(n1, n2, n3) = cpg.file("Program.cs").namespaceBlock.name.sorted.l
    }
  }

  "a file with nested namespaces" should {
    val cpg = code(
      """
        |namespace N1
        |{
        |    namespace N2
        |    {
        |        class A {}
        |        class B {}
        |    }
        |}
        |""".stripMargin,
      "Program.cs"
    )

    "create two namespaces under Program.cs" in {
      val List(n1, n2) = cpg.namespaceBlock.nameExact("N1", "N2").l
      n1.name shouldBe "N1"
      n1.fullName shouldBe "N1"
      n2.name shouldBe "N2"
      n2.fullName shouldBe "N1.N2"
      n1.filename shouldBe "Program.cs"
      n1.filename shouldBe n2.filename
      n1.file.head shouldBe n2.file.head
    }
  }

}
