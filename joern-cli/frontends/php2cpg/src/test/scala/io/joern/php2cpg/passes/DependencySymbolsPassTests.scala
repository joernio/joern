package io.joern.php2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

/** Tests of [[DependencySymbolsPass]] on the records produced by the bundled `ClassParser.php`: full names, constructor
  * stubs per kind, merging of duplicate declarations and deterministic results.
  */
class DependencySymbolsPassTests extends AnyWordSpec with Matchers with Inside {

  /** Writes the files into a fresh dependency root and runs the pass on an empty CPG. */
  private def cpgFor(files: (String, String)*): Cpg =
    FileUtil.usingTemporaryDirectory("php2cpg-dependency-symbols") { parent =>
      val root = parent.resolve("deps")
      files.foreach { case (relativePath, content) =>
        val file = root.resolve(relativePath)
        Files.createDirectories(file.getParent)
        Files.writeString(file, content)
      }
      Files.createDirectories(root)
      val cpg = Cpg.empty
      new DependencySymbolsPass(cpg, root).createAndApply()
      cpg
    }

  private def methodNamesOf(cpg: Cpg, typeFullName: String): Set[String] =
    cpg.typeDecl.fullNameExact(typeFullName).method.name.toSet

  "types in the global namespace" should {
    "get full names without a leading backslash, also for their methods" in {
      val cpg = cpgFor("Top.php" -> "<?php class Top { function run() {} }")

      inside(cpg.typeDecl.l) { case List(top) =>
        top.fullName shouldBe "Top"
        top.isExternal shouldBe true
        top.method.fullName.toSet shouldBe Set("Top.run", "Top.__construct")
        top.method.astParentFullName.toSet shouldBe Set("Top")
      }
      cpg.typeDecl.fullName.filter(_.startsWith("\\")).l shouldBe Nil
      cpg.method.fullName.filter(_.startsWith("\\")).l shouldBe Nil
    }
  }

  "types in a namespace" should {
    "get the declared namespace in their full name and method full names" in {
      val cpg = cpgFor("Somewhere/Else.php" -> "<?php namespace Acme\\Sub; class Thing { function make() {} }")

      inside(cpg.typeDecl.l) { case List(thing) =>
        thing.fullName shouldBe "Acme\\Sub\\Thing"
        thing.method.fullName.toSet shouldBe Set("Acme\\Sub\\Thing.make", "Acme\\Sub\\Thing.__construct")
        thing.method.astParentFullName.toSet shouldBe Set("Acme\\Sub\\Thing")
      }
    }
  }

  "constructor stubs" should {
    "be added to classes with methods only" in {
      val cpg = cpgFor("Kinds.php" -> """<?php
          |namespace K;
          |class WithMethods { function m() {} }
          |class WithOwnConstructor { private function __construct() {} function m() {} }
          |class WithoutMethods {}
          |interface I { function i(); }
          |trait T { function t() {} }
          |enum E { case A; function e() {} }
          |""".stripMargin)

      methodNamesOf(cpg, "K\\WithMethods") shouldBe Set("m", "__construct")
      cpg.typeDecl.fullNameExact("K\\WithOwnConstructor").method.nameExact("__construct").size shouldBe 1
      methodNamesOf(cpg, "K\\WithoutMethods") shouldBe Set.empty
      methodNamesOf(cpg, "K\\I") shouldBe Set("i")
      methodNamesOf(cpg, "K\\T") shouldBe Set("t")
      methodNamesOf(cpg, "K\\E") shouldBe Set("e")
    }
  }

  "duplicate declarations of one full name" should {
    "become one type with the union of the methods, the first declaration winning case-insensitively" in {
      val cpg = cpgFor(
        "A/One.php" -> """<?php
            |namespace Dup;
            |if (PHP_VERSION_ID >= 80000) { class Twice { function first() {} function Shared() {} } }
            |else { class Twice { function fromElse() {} } }
            |""".stripMargin,
        "B/Two.php" -> "<?php namespace Dup; class Twice { function shared() {} function second() {} }"
      )

      inside(cpg.typeDecl.fullNameExact("Dup\\Twice").l) { case List(twice) =>
        twice.method.name.toSet shouldBe Set("first", "Shared", "fromElse", "second", "__construct")
        twice.method.nameExact("__construct").size shouldBe 1
      }
      cpg.method.fullName.l.groupBy(identity).collect { case (name, all) if all.size > 1 => name } shouldBe empty
    }

    "stay separate when the namespaces differ, also only by case" in {
      val cpg = cpgFor(
        "A/Same.php"  -> "<?php namespace A; class Same {}",
        "B/Same.php"  -> "<?php namespace B; class Same {}",
        "a2/Same.php" -> "<?php namespace a; class Same {}",
        "G/Same.php"  -> "<?php class Same {}"
      )

      cpg.typeDecl.fullName.toSet shouldBe Set("A\\Same", "B\\Same", "a\\Same", "Same")
    }
  }

  "the result" should {
    "be the same on every run" in {
      val files = (1 to 20).map { i =>
        s"N$i/T.php" -> s"<?php namespace N${i % 3}; class T { function z$i() {} function a$i() {} function m$i() {} }"
      }
      def snapshot(cpg: Cpg): List[(String, List[String])] =
        cpg.typeDecl.l.sortBy(_.fullName).map(typeDecl => typeDecl.fullName -> typeDecl.method.fullName.l)

      val first = snapshot(cpgFor(files*))
      first.map(_._1) shouldBe List("N0\\T", "N1\\T", "N2\\T")
      first.flatMap(_._2).size shouldBe 20 * 3 + 3
      snapshot(cpgFor(files*)) shouldBe first
    }
  }
}
