package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

import java.io.File

class InheritanceFullNamePassTests extends DataFlowCodeToCpgSuite {

  "inherited type full names" should {
    lazy val cpg = code(
      """
        |import Musician from "./domain/music";
        |
        |class MusicWithLyrics extends Musician {
        |  constructor(name, song, lyrics) {
        |    super(name, song);
        |    this.lyrics = lyrics;
        |  }
        |}
        |""".stripMargin,
      "inheritance.js"
    ).moreCode(
      """
        |class Musician {
        |    constructor(name, song) {
        |        this.username = name;
        |        this.song = song;
        |    }
        |    sing() {
        |        console.log(`${this.username} says ${this.song}`);
        |    }
        |}
        |
        |export default Musician;
        |""".stripMargin,
      Seq("domain", "music.js").mkString(File.separator)
    )

    "resolve the type being inherited fully" in {
      val Some(tgtType) = cpg.typeDecl.nameExact("MusicWithLyrics").headOption
      tgtType.fullName shouldBe "inheritance.js::program:MusicWithLyrics"
      cpg.typeDecl("Musician").fullName.headOption shouldBe Some(
        Seq("domain", "music.js::program:Musician").mkString(File.separator)
      )

      tgtType.inheritsFromTypeFullName.headOption shouldBe Some(
        Seq("domain", "music.js::program:Musician").mkString(File.separator)
      )
    }
  }

  "inherited external types" should {
    lazy val cpg = code(
      """
        |import Musician from "music";
        |
        |class MusicWithLyrics extends Musician {
        |  constructor(name, song, lyrics) {
        |    super(name, song);
        |    this.lyrics = lyrics;
        |  }
        |}
        |""".stripMargin,
      "inheritance.js"
    )

    "resolve the type to a type stub from an external module" in {
      val Some(tgtType) = cpg.typeDecl.nameExact("MusicWithLyrics").headOption
      tgtType.fullName shouldBe "inheritance.js::program:MusicWithLyrics"
      tgtType.inheritsFromTypeFullName.headOption shouldBe Some("music.js::program:Musician")
    }

  }

}
