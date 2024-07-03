package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*
import java.io.File
import scala.annotation.nowarn

@nowarn // otherwise scalac warns about interpolated expressions
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
        |
        |const myMusician = new Musician('Rafi', 'song1');
        |const myMusicWithLyrics = new MusicWithLyrics('Fido', 'song1', 'lyrics');
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
      val List(tgtType) = cpg.typeDecl.nameExact("MusicWithLyrics").l
      tgtType.fullName shouldBe "inheritance.js::program:MusicWithLyrics"
      cpg.typeDecl("Musician").fullName.headOption shouldBe Option(
        Seq("domain", "music.js::program:Musician").mkString(File.separator)
      )

      tgtType.inheritsFromTypeFullName.headOption shouldBe Option(
        Seq("domain", "music.js::program:Musician").mkString(File.separator)
      )
    }

    "identifiers instantiated from these types should have their fully resolved types" in {
      val List(musician)           = cpg.identifier.nameExact("myMusician").l
      val List(musicianWithLyrics) = cpg.identifier.nameExact("myMusicWithLyrics").l
      musician.typeFullName shouldBe Seq("domain", "music.js::program:Musician").mkString(File.separator)
      musicianWithLyrics.typeFullName shouldBe "inheritance.js::program:MusicWithLyrics"
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
      val List(tgtType) = cpg.typeDecl.nameExact("MusicWithLyrics").l
      tgtType.fullName shouldBe "inheritance.js::program:MusicWithLyrics"
      tgtType.inheritsFromTypeFullName.headOption shouldBe Option("music.js::program:Musician")
    }

  }

}
