package io.joern.php2cpg.parser

import io.joern.php2cpg.parser.ClassParser.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import scala.util.Try

/** Direct tests for [[ClassParser]] and the bundled `ClassParser.php` script. Requires `php` on the PATH, like the rest
  * of the php2cpg test suite.
  */
class ClassParserTests extends AnyWordSpec with Matchers with Inside {

  /** Writes the given (relative path -> content) files into a fresh temporary directory and runs the parser on it. */
  private def parseProject(files: (String, String)*): Try[List[ClassParserClass]] =
    FileUtil.usingTemporaryDirectory("php2cpg-class-parser-test") { dir =>
      files.foreach { case (relativePath, content) =>
        val file = dir.resolve(relativePath)
        Files.createDirectories(file.getParent)
        Files.writeString(file, content)
      }
      new ClassParser(dir).parse()
    }

  private def parseSuccessfully(files: (String, String)*): List[ClassParserClass] = {
    val result = parseProject(files*)
    withClue(s"ClassParser failed: ${result.failed.map(_.getMessage).getOrElse("")}") {
      result.isSuccess shouldBe true
    }
    result.get
  }

  private def functionsOf(clazz: ClassParserClass): Map[String, List[String]] =
    clazz.functions.map(function => function.name -> function.modifiers).toMap

  "a class with modifiers, inheritance and methods" should {
    lazy val classes = parseSuccessfully("Foo/Bar/Baz.php" -> """<?php
        |namespace Foo\Bar;
        |
        |abstract class Baz extends Base implements I1, I2 {
        |  public function __construct() {}
        |  public static function make() { if (true) { return 1; } }
        |  private function hidden() {}
        |  protected abstract function todo();
        |  function noModifiers() {}
        |}
        |""".stripMargin)

    "be found with its name, file and declared namespace" in {
      inside(classes) { case List(baz) =>
        baz.name shouldBe "Baz"
        baz.namespace shouldBe "Foo\\Bar"
        baz.relativeFile shouldBe "Foo/Bar/Baz.php"
      }
    }

    "record the class modifiers" in {
      inside(classes) { case List(baz) => baz.modifiers shouldBe List("abstract") }
    }

    "record every method with its modifiers, ignoring nested blocks in method bodies" in {
      inside(classes) { case List(baz) =>
        functionsOf(baz) shouldBe Map(
          "__construct" -> List("public"),
          "make"        -> List("public", "static"),
          "hidden"      -> List("private"),
          "todo"        -> List("protected", "abstract"),
          "noModifiers" -> Nil
        )
      }
    }
  }

  "a class without an explicit constructor" should {
    "be given a default public `__construct`" in {
      val classes = parseSuccessfully("Foo/NoCtor.php" -> """<?php
          |class NoCtor {
          |  public function run() {}
          |}
          |""".stripMargin)

      inside(classes) { case List(noCtor) =>
        functionsOf(noCtor) shouldBe Map("run" -> List("public"), "__construct" -> List("public"))
      }
    }
  }

  "a class without any methods" should {
    "have no functions, not even a default constructor" in {
      // The script emits no `functions` key, so ClassParser.scala does not add `__construct`.
      val classes = parseSuccessfully("Top.php" -> """<?php
          |final class Top {}
          |""".stripMargin)

      inside(classes) { case List(top) =>
        top.name shouldBe "Top"
        top.namespace shouldBe ""
        top.relativeFile shouldBe "Top.php"
        top.modifiers shouldBe List("final")
        top.functions shouldBe Nil
      }
    }
  }

  "functions declared outside of a class body" should {
    "not be attributed to any class" in {
      val classes = parseSuccessfully("Foo/Mixed.php" -> """<?php
          |function before() {}
          |class Mixed {
          |  public function inside() {}
          |}
          |function after() {}
          |""".stripMargin)

      inside(classes) { case List(mixed) =>
        functionsOf(mixed) shouldBe Map("inside" -> List("public"), "__construct" -> List("public"))
      }
    }
  }

  "interfaces, traits and enums" should {
    lazy val types = parseSuccessfully("Foo/Contracts.php" -> """<?php
        |class First { public function one() {} }
        |interface IFace { public function m(); }
        |trait Helper { public function help() {} }
        |enum Suit: string { case A = 'a'; public function label() {} }
        |""".stripMargin)

    "be reported with their kind and own methods" in {
      types.map(t => t.name -> t.kind) shouldBe
        List("First" -> "class", "IFace" -> "interface", "Helper" -> "trait", "Suit" -> "enum")
      types.map(t => t.name -> functionsOf(t)) shouldBe List(
        "First"  -> Map("one" -> List("public"), "__construct" -> List("public")),
        "IFace"  -> Map("m" -> List("public")),
        "Helper" -> Map("help" -> List("public")),
        "Suit"   -> Map("label" -> List("public"))
      )
    }

    "not get a default constructor" in {
      types.filter(_.kind != "class").foreach(_.functions.map(_.name) should not contain "__construct")
    }
  }

  "things that are not declarations or methods" should {
    "be ignored" in {
      val types = parseSuccessfully("Lib/Misc.php" -> """<?php
          |namespace Lib;
          |function helper() {}
          |$classes = [Alpha::class, Beta::class];
          |$obj = new class($x) extends Gamma { public function hidden() {} };
          |class Real { function m() { $s = "{$x}${y}"; function inner() {} } function list() {} }
          |""".stripMargin)

      inside(types) { case List(real) =>
        real.name shouldBe "Real"
        functionsOf(real).keySet shouldBe Set("m", "list", "__construct")
      }
    }
  }

  "the global namespace" should {
    "be reported as the empty namespace, giving a full name without leading backslash" in {
      inside(parseSuccessfully("Top.php" -> "<?php class Top {}")) { case List(top) =>
        top.namespace shouldBe ""
        top.fullName shouldBe "Top"
      }
    }
  }

  "methods" should {
    "keep declaration order" in {
      val classes =
        parseSuccessfully("Ordered.php" -> "<?php class Ordered { function z() {} function a() {} function m() {} }")
      inside(classes) { case List(ordered) =>
        ordered.functions.map(_.name) shouldBe List("z", "a", "m", "__construct")
      }
    }
  }

  "files without a `.php` extension" should {
    "be ignored" in {
      val classes = parseSuccessfully(
        "Foo/Real.php"  -> "<?php class Real {}",
        "Foo/Ghost.txt" -> "<?php class Ghost {}",
        "Foo/Ghost.inc" -> "<?php class Ghost2 {}"
      )

      classes.map(_.name) shouldBe List("Real")
    }
  }

  "multiple classes across nested directories" should {
    "all be found with their respective namespaces" in {
      val classes = parseSuccessfully(
        "A/One.php"     -> "<?php namespace A; class One {}",
        "A/B/Two.php"   -> "<?php namespace A\\B; class Two {} class Three {}",
        "C/D/E/Six.php" -> "<?php namespace C\\D\\E; class Six {}"
      )

      classes.map(clazz => clazz.name -> clazz.namespace).toSet shouldBe Set(
        "One"   -> "A",
        "Two"   -> "A\\B",
        "Three" -> "A\\B",
        "Six"   -> "C\\D\\E"
      )
    }
  }

  "two classes with the same short name in different namespaces" should {
    "be reported separately, ordered by file" in {
      val classes = parseSuccessfully(
        "B/Dup.php" -> "<?php namespace B; class Dup {}",
        "A/Dup.php" -> "<?php namespace A; class Dup {}"
      )

      classes.map(clazz => clazz.name -> clazz.namespace) shouldBe List("Dup" -> "A", "Dup" -> "B")
    }
  }

  "an empty directory" should {
    "yield no classes" in {
      parseSuccessfully() shouldBe Nil
    }
  }

  "a non-existent target directory" should {
    "fail" in {
      FileUtil.usingTemporaryDirectory("php2cpg-class-parser-test") { dir =>
        new ClassParser(dir.resolve("does-not-exist")).parse().isFailure shouldBe true
      }
    }
  }

}
