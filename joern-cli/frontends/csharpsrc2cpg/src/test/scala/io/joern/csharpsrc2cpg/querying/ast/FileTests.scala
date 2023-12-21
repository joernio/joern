package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class FileTests extends CSharpCode2CpgFixture {

  "a .cs file on the project root" should {

    val cpg = code(basicBoilerplate(), "Program.cs")

    "create a file node with file meta data" in {
      val programFile = cpg.file.nameExact("Program.cs").head
      programFile.name shouldBe "Program.cs"
      programFile.content should startWith("using System;")
      programFile.hash match
        case Some(hash) => hash should startWith("bd9afc24308")
        case None       => fail("No file hash detected")
    }

  }

  "multiple .cs files in directories" should {

    val fooName = "Foo.cs"
    val barName = Seq("sub", "Bar.cs").mkString(java.io.File.separator)

    val cpg = code(basicBoilerplate(className = "Foo"), fooName)
      .moreCode(basicBoilerplate(className = "Bar"), barName)

    "create file nodes with file meta data" in {
      val fooFile = cpg.file.nameExact(fooName).head
      fooFile.name shouldBe fooName
      fooFile.content should startWith("using System;")
      fooFile.hash match
        case Some(hash) => hash should startWith("79d4a0d812")
        case None       => fail("No file hash detected")

      val barFile = cpg.file.nameExact(barName).head
      barFile.name shouldBe barName
      barFile.content should startWith("using System;")
      barFile.hash match
        case Some(hash) => hash should startWith("5847f5ce0e")
        case None       => fail("No file hash detected")
    }

  }

}
