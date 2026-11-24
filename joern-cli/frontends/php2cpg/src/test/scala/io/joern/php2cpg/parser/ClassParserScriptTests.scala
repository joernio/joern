package io.joern.php2cpg.parser

import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{FileSystems, Files, Path}
import scala.util.Using

/** Tests of the bundled `ClassParser.php` through its process interface only: files are written to a temporary
  * dependency root, the script is run with `php`, and its exit status, stdout and stderr are checked. This covers the
  * parts of the contract that [[ClassParserTests]] cannot see through the Scala decoder (exact JSON, stderr, exit
  * codes).
  *
  * The suite runs on whatever `php` is on the PATH. Inputs are chosen so that the expected result is the same on PHP 7
  * and PHP 8 tokenizers (qualified names, attributes, `enum`, `readonly`); running it on both is the CI matrix's job.
  */
class ClassParserScriptTests extends AnyWordSpec with Matchers with Inside {

  /** Name of the dependency root, below the temporary working directory. */
  private val RootName = "root"

  private case class ScriptRun(exitCode: Int, stdOut: Seq[String], stdErr: Seq[String]) {

    /** The decoded records. Fails the test unless the run succeeded with exactly one line of JSON on stdout. */
    def records: Seq[ujson.Value] = {
      withClue(s"stderr: ${stdErr.mkString("\n")}\nstdout: ${stdOut.mkString("\n")}\n") {
        exitCode shouldBe 0
        stdOut.size shouldBe 1
      }
      ujson.read(stdOut.head).arr.toSeq
    }

    def names: Seq[String] = records.map(_("name").str)

    def json: String = {
      val _ = records
      stdOut.head
    }

    def stdOutIsBlank: Boolean = stdOut.forall(_.trim.isEmpty)
  }

  /** Copies the script into `workingDir` (outside the dependency root) and runs `php [phpOptions] script args`. */
  private def runScript(workingDir: Path, args: Seq[String], phpOptions: Seq[String] = Nil): ScriptRun = {
    val script = workingDir.resolve("ClassParser.php")
    if (!Files.exists(script)) {
      Using.resource(getClass.getResourceAsStream("/ClassParser.php"))(input => Files.copy(input, script))
    }
    val result = ExternalCommand.run(Seq("php") ++ phpOptions ++ Seq(script.toString) ++ args, Option(workingDir))
    ScriptRun(result.exitCode, result.stdOut, result.stdErr)
  }

  /** Runs `body` with a fresh working directory and an empty dependency root `<working dir>/root` in it. */
  private def withRoot[T](body: (Path, Path) => T): T =
    FileUtil.usingTemporaryDirectory("php2cpg-class-parser-script") { workingDir =>
      val root = workingDir.resolve(RootName)
      Files.createDirectories(root)
      body(workingDir, root)
    }

  private def write(root: Path, relativePath: String, content: Array[Byte]): Path = {
    val file = root.resolve(relativePath)
    Files.createDirectories(file.getParent)
    Files.write(file, content)
  }

  private def write(root: Path, relativePath: String, content: String): Path =
    write(root, relativePath, content.getBytes(UTF_8))

  /** Writes the files into a fresh dependency root and runs the script on it the way the frontend does. */
  private def scan(files: (String, String)*): ScriptRun = withRoot { (workingDir, root) =>
    files.foreach { case (relativePath, content) => write(root, relativePath, content) }
    runScript(workingDir, Seq(RootName))
  }

  private def strings(value: ujson.Value): Seq[String] = value.arr.map(_.str).toSeq

  /** The single record with the given name. */
  private def recordOf(run: ScriptRun, name: String): ujson.Value =
    inside(run.records.filter(_("name").str == name)) { case Seq(record) => record }

  /** The methods of a record in output order, with their modifiers. */
  private def methodsOf(record: ujson.Value): Seq[(String, Seq[String])] =
    record.obj
      .get("functions")
      .toSeq
      .flatMap(_.obj.toSeq.map { case (name, method) =>
        name -> strings(method("modifiers"))
      })

  private def methodNames(record: ujson.Value): Seq[String] = methodsOf(record).map(_._1)

  private def summaryLine(parsed: Int, considered: Int): String = s"Parsed $parsed/$considered files successfully."

  private def symlink(link: Path, target: Path): Unit = {
    try {
      Files.createSymbolicLink(link, target)
    } catch {
      case exception @ (_: UnsupportedOperationException | _: IOException) =>
        cancel(s"cannot create symbolic links here: ${exception.getMessage}")
    }
  }

  /** Runs `body` while `path` has no permissions, and restores them afterwards. Cancelled where that has no effect. */
  private def withoutPermissions[T](path: Path)(body: => T): T = {
    assume(FileSystems.getDefault.supportedFileAttributeViews.contains("posix"), "needs POSIX file permissions")
    val original = Files.getPosixFilePermissions(path)
    Files.setPosixFilePermissions(path, java.util.Set.of())
    try {
      assume(!Files.isReadable(path), "permissions are not enforced (running as root?)")
      body
    } finally {
      Files.setPosixFilePermissions(path, original)
    }
  }

  // ---- Process interface -------------------------------------------------------------------------------------------

  "the script called with wrong arguments" should {
    "print usage to stderr only and exit non-zero" in {
      withRoot { (workingDir, _) =>
        val argumentLists = Seq(Nil, Seq(RootName, RootName), Seq("--help"), Seq("-help"), Seq("-h"), Seq("-?"))
        argumentLists.foreach { args =>
          withClue(s"arguments $args: ") {
            val run = runScript(workingDir, args)
            run.exitCode should not be 0
            run.stdOutIsBlank shouldBe true
            run.stdErr.mkString("\n") should include("Usage")
          }
        }
      }
    }
  }

  "an invalid dependency root" should {
    "fail with a message on stderr and nothing on stdout when it does not exist" in {
      withRoot { (workingDir, _) =>
        val run = runScript(workingDir, Seq("does-not-exist"))
        run.exitCode should not be 0
        run.stdOutIsBlank shouldBe true
        run.stdErr should not be empty
      }
    }

    "fail with a message on stderr and nothing on stdout when it is a file" in {
      withRoot { (workingDir, root) =>
        write(root, "File.php", "<?php class InFile {}")
        val run = runScript(workingDir, Seq(s"$RootName/File.php"))
        run.exitCode should not be 0
        run.stdOutIsBlank shouldBe true
        run.stdErr should not be empty
      }
    }

    "fail with a message on stderr and nothing on stdout when it cannot be read" in {
      withRoot { (workingDir, root) =>
        write(root, "A/X.php", "<?php class X {}")
        withoutPermissions(root) {
          val run = runScript(workingDir, Seq(RootName))
          run.exitCode should not be 0
          run.stdOutIsBlank shouldBe true
          run.stdErr should not be empty
        }
      }
    }
  }

  "a successful run" should {
    "exit 0, write one line of JSON to stdout and end stderr with the summary" in {
      withRoot { (workingDir, root) =>
        write(root, "A/One.php", "<?php namespace A; class One { function m() {} }")
        write(root, "A/Two.php", "<?php\n\n\nnamespace A;\n\nclass Two {}\n")
        val run = runScript(workingDir, Seq(RootName))
        run.names shouldBe Seq("One", "Two")
        run.json should not include "\n"
        run.stdErr.last shouldBe summaryLine(2, 2)
      }
    }

    "write `[]` for a root without PHP files" in {
      val run = scan("README.md" -> "class NotPhp {}")
      run.json shouldBe "[]"
      run.stdErr.last shouldBe summaryLine(0, 0)
    }

    "be byte-identical when repeated over the same tree" in {
      withRoot { (workingDir, root) =>
        (1 to 30).foreach { i =>
          write(root, s"N${i % 4}/Sub$i/T$i.php", s"<?php namespace N${i % 4}; class T$i { function m$i() {} }")
        }
        val first  = runScript(workingDir, Seq(RootName))
        val second = runScript(workingDir, Seq(RootName))
        first.records.size shouldBe 30
        second.stdOut shouldBe first.stdOut
      }
    }
  }

  "the php.ini settings" should {
    "not let warnings reach stdout, even with display_errors on" in {
      withRoot { (workingDir, root) =>
        write(root, "Good.php", "<?php class Good {}")
        // An unterminated comment makes the tokenizer emit a warning.
        write(root, "Warn.php", "<?php class Warned {} /* unterminated")
        val run = runScript(
          workingDir,
          Seq(RootName),
          Seq("-d", "display_errors=stdout", "-d", "error_reporting=-1", "-d", "html_errors=1")
        )
        run.names should contain("Good")
        run.stdOut.mkString should not include "Warning"
      }
    }

    "not depend on a low memory_limit" in {
      withRoot { (workingDir, root) =>
        write(root, "Big.php", "<?php class Big {\n" + (1 to 20000).map(i => s"  function m$i() {}\n").mkString + "}")
        val run = runScript(workingDir, Seq(RootName), Seq("-d", "memory_limit=8M"))
        inside(run.records) { case Seq(big) => methodNames(big).size shouldBe 20000 }
      }
    }

    "only treat `<?` as an open tag when short_open_tag is on" in {
      withRoot { (workingDir, root) =>
        write(root, "Short.php", "<? class Short {}")
        runScript(workingDir, Seq(RootName), Seq("-d", "short_open_tag=1")).names shouldBe Seq("Short")
        runScript(workingDir, Seq(RootName), Seq("-d", "short_open_tag=0")).names shouldBe Nil
      }
    }
  }

  "the dependency root argument" should {
    "accept a relative path with spaces and non-ASCII characters" in {
      FileUtil.usingTemporaryDirectory("php2cpg-class-parser-script") { workingDir =>
        val rootName = "dep root ü"
        write(workingDir.resolve(rootName), "Ä/X.php", "<?php class X {}")
        inside(runScript(workingDir, Seq(rootName)).records) { case Seq(record) =>
          record("file").str shouldBe "Ä/X.php"
        }
      }
    }

    "accept an absolute path" in {
      withRoot { (workingDir, root) =>
        write(root, "A/X.php", "<?php class X {}")
        inside(runScript(workingDir, Seq(root.toAbsolutePath.toString)).records) { case Seq(record) =>
          record("file").str shouldBe "A/X.php"
        }
      }
    }

    "accept a symlink, reporting paths relative to it" in {
      withRoot { (workingDir, root) =>
        write(root, "A/X.php", "<?php class X {}")
        symlink(workingDir.resolve("linked"), root)
        inside(runScript(workingDir, Seq("linked")).records) { case Seq(record) =>
          record("file").str shouldBe "A/X.php"
        }
      }
    }
  }

  // ---- Stderr and excluded directories -----------------------------------------------------------------------------

  "excluded directories" should {
    "not be scanned at any depth, matched case-insensitively, and not be counted" in {
      withRoot { (workingDir, root) =>
        write(root, "Lib/Kept.php", "<?php class Kept {}")
        write(root, "tests/InTests.php", "<?php class InTests {}")
        write(root, "Lib/Examples/InExamples.php", "<?php class InExamples {}")
        write(root, "Lib/a/b/VENDOR/InVendor.php", "<?php class InVendor {}")
        write(root, "testsuite/NotExcluded.php", "<?php class NotExcluded {}")
        write(root, "Lib/tests.php", "<?php class FileNamedTests {}")

        val run = runScript(workingDir, Seq(RootName))
        run.names shouldBe Seq("Kept", "FileNamedTests", "NotExcluded")
        run.stdErr.last shouldBe summaryLine(3, 3)
      }
    }

    "each be reported on stderr" in {
      withRoot { (workingDir, root) =>
        write(root, "tests/A.php", "<?php class A {}")
        write(root, "Lib/Examples/B.php", "<?php class B {}")
        write(root, "Lib/a/VENDOR/C.php", "<?php class C {}")

        val run = runScript(workingDir, Seq(RootName))
        run.names shouldBe Nil
        run.stdErr.filter(_.startsWith("Skipping excluded directory:")).sorted shouldBe Seq(
          "Skipping excluded directory: Lib/Examples",
          "Skipping excluded directory: Lib/a/VENDOR",
          "Skipping excluded directory: tests"
        )
        run.stdErr.last shouldBe summaryLine(0, 0)
      }
    }
  }

  "stderr" should {
    "be capped at 100 diagnostics, followed by the number suppressed and the summary" in {
      withRoot { (workingDir, root) =>
        write(root, "Real.php", "<?php class Real {}")
        (1 to 105).foreach(i => Files.createDirectories(root.resolve(f"d$i%03d/tests")))

        val run = runScript(workingDir, Seq(RootName))
        run.names shouldBe Seq("Real")
        run.stdErr.size shouldBe 102
        run.stdErr.count(_.startsWith("Skipping excluded directory:")) shouldBe 100
        run.stdErr.takeRight(2) shouldBe Seq("5 further messages suppressed.", summaryLine(1, 1))
      }
    }
  }

  "a file that cannot be read" should {
    "be skipped with a message, and other files still reported" in {
      withRoot { (workingDir, root) =>
        write(root, "Good.php", "<?php class Good {}")
        val locked = write(root, "Locked.php", "<?php class Locked {}")
        withoutPermissions(locked) {
          val run = runScript(workingDir, Seq(RootName))
          run.names shouldBe Seq("Good")
          run.stdErr.exists(line => line.contains("Locked.php") && line.startsWith("Skipping")) shouldBe true
          run.stdErr.last shouldBe summaryLine(1, 2)
        }
      }
    }
  }

  "a directory that cannot be read" should {
    "be skipped with a message, and the run continue" in {
      withRoot { (workingDir, root) =>
        write(root, "Good/Good.php", "<?php class Good {}")
        write(root, "Locked/Hidden.php", "<?php class Hidden {}")
        withoutPermissions(root.resolve("Locked")) {
          val run = runScript(workingDir, Seq(RootName))
          run.names shouldBe Seq("Good")
          run.stdErr.exists(_.startsWith("Skipping unreadable directory")) shouldBe true
        }
      }
    }
  }

  "invalid UTF-8 in a declared name" should {
    "not affect the other files, and give valid JSON (the file is skipped or the bytes replaced)" in {
      withRoot { (workingDir, root) =>
        write(root, "A/Valid.php", "<?php class Valid { function m() {} }")
        write(root, "B/Broken.php", "<?php class Bad".getBytes(UTF_8) ++ Array(0xff.toByte) ++ " {}".getBytes(UTF_8))

        val run = runScript(workingDir, Seq(RootName))
        // `records` already requires exit 0 and parseable JSON.
        run.names.head shouldBe "Valid"
        run.names.size should be <= 2
        run.stdErr.last should (be(summaryLine(1, 2)) or be(summaryLine(2, 2)))
      }
    }
  }

  "invalid UTF-8 outside names" should {
    "not cause the file to be skipped" in {
      withRoot { (workingDir, root) =>
        val junk = Array(0xff, 0xfe, 0xc3).map(_.toByte)
        write(
          root,
          "Junk.php",
          "<?php // ".getBytes(UTF_8) ++ junk ++ "\nclass Junk { function m() { return '".getBytes(UTF_8) ++ junk ++
            "'; } }".getBytes(UTF_8)
        )
        val run = runScript(workingDir, Seq(RootName))
        run.names shouldBe Seq("Junk")
        run.stdErr.last shouldBe summaryLine(1, 1)
      }
    }
  }

  "non-ASCII names" should {
    "be written raw, not escaped, like `/`" in {
      val run = scan("Ü/N.php" -> "<?php namespace Über; class Grüße {}")
      run.json should include("\"name\":\"Grüße\"")
      run.json should include("\"file\":\"Ü/N.php\"")
      run.json should not include "\\u00"
      run.json should not include "\\/"
    }
  }

  // ---- Output format -----------------------------------------------------------------------------------------------

  "a class with modifiers, supertypes and various methods" should {
    "give exactly one record with the expected JSON" in {
      val run = scan("Acme/Shapes/Polygon.php" -> """<?php
          |namespace Acme\Shapes;
          |
          |abstract class Polygon extends Shape implements \Countable, HasArea {
          |    public function __construct(private array $points) {}
          |    final public static function triangle(): self { if (true) { return new Square(); } }
          |    protected abstract function area(): float;
          |    function count(): int { $f = function () { return 1; }; return $f(); }
          |    public function list() { return "{$this->points[0]}"; }
          |}
          |""".stripMargin)

      run.json shouldBe
        """[{"name":"Polygon","namespace":"Acme\\Shapes","file":"Acme/Shapes/Polygon.php","kind":"class",""" +
        """"modifiers":["abstract"],"extends":["Shape"],"implements":["\\Countable","HasArea"],""" +
        """"functions":{"__construct":{"modifiers":["public"]},"triangle":{"modifiers":["final","public","static"]},""" +
        """"area":{"modifiers":["protected","abstract"]},"count":{"modifiers":[]},"list":{"modifiers":["public"]}}}]"""
    }
  }

  "a file mixing functions, anonymous classes and several type kinds" should {
    "give exactly the records of the interface, trait, enum and class" in {
      val run = scan("Lib/Misc.php" -> """<?php
          |namespace Lib;
          |function helper() {}
          |$classes = [Alpha::class, Beta::class];
          |$obj = new class($x) extends Gamma { public function hidden() {} };
          |interface Greeter { public function greet(string $who): string; }
          |trait Loud { public function shout() {} }
          |enum Suit: string { case Hearts = 'H'; public function label(): string { return 'x'; } }
          |final class Empty_ {}
          |function after() {}
          |""".stripMargin)

      run.records.map(record => (record("name").str, record("kind").str, methodNames(record))) shouldBe Seq(
        ("Greeter", "interface", Seq("greet")),
        ("Loud", "trait", Seq("shout")),
        ("Suit", "enum", Seq("label")),
        ("Empty_", "class", Nil)
      )
      strings(recordOf(run, "Empty_")("modifiers")) shouldBe Seq("final")
      Seq("Alpha", "Beta", "Gamma", "hidden", "helper", "after").foreach(word => run.json should not include word)
    }
  }

  "a type without methods" should {
    "have all fields except `functions`, and never `[]` for `functions`" in {
      val run = scan("A/Props.php" -> "<?php namespace A; class Props { public $x = 1; const C = 2; }")
      inside(run.records) { case Seq(record) =>
        record.obj.keySet shouldBe Set("name", "namespace", "file", "kind", "modifiers", "extends", "implements")
        strings(record("extends")) shouldBe Nil
        strings(record("implements")) shouldBe Nil
        strings(record("modifiers")) shouldBe Nil
      }
      run.json should not include "\"functions\":[]"
    }
  }

  "a class without a constructor" should {
    "not be given one by the script" in {
      inside(scan("C.php" -> "<?php class C { function m() {} }").records) { case Seq(record) =>
        methodNames(record) shouldBe Seq("m")
      }
    }
  }

  "the order of records" should {
    "be bytewise by file, then by position in the file, with nested types after their outer type" in {
      val run = scan(
        "a/X.php" -> "<?php class Lower {}",
        "_/X.php" -> "<?php class Underscore {}",
        "B/X.php" -> """<?php
            |final class First { function m() { class Nested {} } function n() {} }
            |class Second {}
            |""".stripMargin,
        "B/W.php" -> "<?php class BeforeX {}"
      )
      run.records.map(record => record("file").str -> record("name").str) shouldBe Seq(
        "B/W.php" -> "BeforeX",
        "B/X.php" -> "First",
        "B/X.php" -> "Nested",
        "B/X.php" -> "Second",
        "_/X.php" -> "Underscore",
        "a/X.php" -> "Lower"
      )
      methodNames(recordOf(run, "First")) shouldBe Seq("m", "n")
    }
  }

  // ---- Files -------------------------------------------------------------------------------------------------------

  "file extensions" should {
    "match `.php` case-insensitively and nothing else" in {
      val run = scan(
        "X/Real.php"    -> "<?php class Real {}",
        "X/Upper.PHP"   -> "<?php class Upper {}",
        "X/Mixed.Php"   -> "<?php class Mixed {}",
        "X/Ghost.inc"   -> "<?php class GhostInc {}",
        "X/Ghost.phtml" -> "<?php class GhostPhtml {}",
        "X/Ghost.txt"   -> "<?php class GhostTxt {}",
        "X/php"         -> "<?php class GhostNoExtension {}",
        "X/Ghost.php.b" -> "<?php class GhostBackup {}"
      )
      run.names.toSet shouldBe Set("Real", "Upper", "Mixed")
      run.stdErr.last shouldBe summaryLine(3, 3)
    }
  }

  "hidden files and directories" should {
    "be scanned" in {
      scan(".hidden/H.php" -> "<?php class InHidden {}", "A/.Dot.php" -> "<?php class DotFile {}").names shouldBe
        Seq("InHidden", "DotFile")
    }
  }

  "symbolic links" should {
    "not be followed, neither to files nor to directories, so cycles do not matter" in {
      withRoot { (workingDir, root) =>
        write(root, "real/A.php", "<?php class A {}")
        symlink(root.resolve("linkdir"), root.resolve("real"))
        symlink(root.resolve("real/cycle"), root)
        symlink(root.resolve("real/Link.php"), root.resolve("real/A.php"))

        val run = runScript(workingDir, Seq(RootName))
        run.records.map(record => record("file").str) shouldBe Seq("real/A.php")
        run.stdErr.last shouldBe summaryLine(1, 1)
      }
    }
  }

  "the `file` field" should {
    "be relative to the root and use `/` on every platform" in {
      inside(scan("A/B/C/D.php" -> "<?php class D {}", "Top.php" -> "<?php class Top {}").records) {
        case Seq(nested, top) =>
          nested("file").str shouldBe "A/B/C/D.php"
          top("file").str shouldBe "Top.php"
      }
    }
  }

  "a file with a shebang, a BOM, inline HTML and several PHP blocks" should {
    "only count code inside PHP blocks" in {
      val run = scan(
        "Shebang.php" -> "#!/usr/bin/env php\n<?php class AfterShebang {}",
        "Bom.php"     -> "\uFEFF<?php class AfterBom {}",
        "Html.php"    -> """<html><body>class FakeInHtml {}</body>
            |<?php class InFirstBlock { function a() {} } ?>
            |<p>interface FakeToo {}</p>
            |<?php class InSecondBlock {}
            |""".stripMargin
      )
      run.names.toSet shouldBe Set("AfterShebang", "AfterBom", "InFirstBlock", "InSecondBlock")
    }
  }

  "`__halt_compiler`" should {
    "end the scan of that file only" in {
      val run = scan(
        "A.php" -> "<?php class Before {} __halt_compiler(); class After {} \u0000ÿ binary",
        "B.php" -> "<?php class OtherFile {}"
      )
      run.names shouldBe Seq("Before", "OtherFile")
    }
  }

  "unbalanced braces in one file" should {
    "not affect other files" in {
      val run = scan(
        "A.php" -> "<?php class Truncated { function m() { if (true) {",
        "B.php" -> "<?php class Fine { function n() {} } function free() {}",
        "C.php" -> "<?php } } } class AfterExtraBraces { function o() {} }"
      )
      methodNames(recordOf(run, "Fine")) shouldBe Seq("n")
      methodNames(recordOf(run, "AfterExtraBraces")) shouldBe Seq("o")
    }
  }

  // ---- Types -------------------------------------------------------------------------------------------------------

  "types in conditional blocks, functions and methods" should {
    "be reported with the namespace in effect" in {
      val run = scan("Poly.php" -> """<?php
          |namespace Poly;
          |if (!class_exists('Polyfilled')) { class Polyfilled { function p() {} } }
          |if (PHP_VERSION_ID < 80000) { interface OldI {} } else { interface NewI {} }
          |function factory() { class FromFunction { function f() {} } }
          |class Outer { function m() { trait FromMethod { function t() {} } } function after() {} }
          |""".stripMargin)

      run.records.map(record => record("name").str -> record("namespace").str) shouldBe Seq(
        "Polyfilled"   -> "Poly",
        "OldI"         -> "Poly",
        "NewI"         -> "Poly",
        "FromFunction" -> "Poly",
        "Outer"        -> "Poly",
        "FromMethod"   -> "Poly"
      )
      methodNames(recordOf(run, "FromFunction")) shouldBe Seq("f")
      methodNames(recordOf(run, "Outer")) shouldBe Seq("m", "after")
      methodNames(recordOf(run, "FromMethod")) shouldBe Seq("t")
    }
  }

  "duplicate declarations" should {
    "each give a record, in one file and across files" in {
      val run = scan(
        "A/One.php" -> """<?php
            |namespace A;
            |if (PHP_VERSION_ID >= 80000) { class Dup { function eight() {} } } else { class Dup { function seven() {} } }
            |""".stripMargin,
        "B/Two.php" -> "<?php namespace A; class Dup { function other() {} }"
      )
      run.records.map(record => (record("namespace").str, record("name").str, methodNames(record))) shouldBe Seq(
        ("A", "Dup", Seq("eight")),
        ("A", "Dup", Seq("seven")),
        ("A", "Dup", Seq("other"))
      )
    }
  }

  "anonymous classes" should {
    "not be reported, nor their methods or supertypes" in {
      val run = scan("Anon.php" -> """<?php
          |$a = new class {};
          |$b = new class($x, [1, 2]) extends BaseOne implements IfaceOne { public function hiddenOne() {} };
          |$c = new class extends BaseTwo { function hiddenTwo() {} };
          |$d = new readonly class { function hiddenThree() {} };
          |$e = new #[Attr]
          |  class { function hiddenFour() {} };
          |class Outer {
          |  function make() { return new class { function hiddenFive() {} }; }
          |  function after() {}
          |}
          |""".stripMargin)

      run.names shouldBe Seq("Outer")
      methodNames(recordOf(run, "Outer")) shouldBe Seq("make", "after")
      Seq("BaseOne", "BaseTwo", "IfaceOne", "hidden").foreach(word => run.json should not include word)
    }
  }

  "`::class` and member access" should {
    "not be taken as declarations" in {
      val run = scan("Refs.php" -> """<?php
          |$list = [Alpha::class, Beta::class, static::class, self::class];
          |$x = $obj->class; $y = $obj?->interface; $z = Foo::trait;
          |echo Gamma::class . Delta::class;
          |class Real { function m() { return [Epsilon::class, parent::class]; } }
          |""".stripMargin)

      run.names shouldBe Seq("Real")
      methodNames(recordOf(run, "Real")) shouldBe Seq("m")
    }
  }

  "declaration keywords in strings and comments" should {
    "be ignored" in {
      val run = scan("Text.php" -> """<?php
          |$s = 'class InSingle {}'; $d = "interface InDouble {} {$x}";
          |$h = <<<EOT
          |trait InHeredoc { {$y} }
          |EOT;
          |$n = <<<'EOT'
          |enum InNowdoc {
          |EOT;
          |// class InLineComment {
          |# interface InHashComment {
          |/* trait InBlockComment { */
          |/** enum InDocblock { */
          |class Real { function m() {} }
          |""".stripMargin)

      run.names shouldBe Seq("Real")
      methodNames(recordOf(run, "Real")) shouldBe Seq("m")
    }
  }

  "`enum` and `readonly` as identifiers" should {
    "not start a declaration or add a modifier" in {
      val run = scan("Ctx.php" -> """<?php
          |echo enum(1); $x = Foo::enum; $y = $o->enum; $z = $enum;
          |function readonly() {} readonly();
          |class Enum { const enum = 1; function enum() {} function readonly() {} }
          |class Plain { public readonly int $p; function after() {} }
          |""".stripMargin)

      run.records.map(record => record("name").str -> record("kind").str) shouldBe Seq(
        "Enum"  -> "class",
        "Plain" -> "class"
      )
      strings(recordOf(run, "Enum")("modifiers")) shouldBe Nil
      methodsOf(recordOf(run, "Enum")) shouldBe Seq("enum" -> Nil, "readonly" -> Nil)
      methodsOf(recordOf(run, "Plain")) shouldBe Seq("after" -> Nil)
    }

    "still declare enums and readonly classes where they start a declaration" in {
      val run = scan("Decl.php" -> """<?php
          |enum Plain { case A; }
          |enum Backed: int implements I1, \N\I2 { case B = 1; }
          |readonly class R {}
          |final readonly class FR {}
          |abstract readonly class AR {}
          |""".stripMargin)

      run.records.map(record => (record("name").str, record("kind").str, strings(record("modifiers")))) shouldBe Seq(
        ("Plain", "enum", Nil),
        ("Backed", "enum", Nil),
        ("R", "class", Seq("readonly")),
        ("FR", "class", Seq("final", "readonly")),
        ("AR", "class", Seq("abstract", "readonly"))
      )
      strings(recordOf(run, "Backed")("implements")) shouldBe Seq("I1", "\\N\\I2")
      strings(recordOf(run, "Backed")("extends")) shouldBe Nil
    }
  }

  "attributes" should {
    "not disturb modifiers or names" in {
      val run = scan("Attr.php" -> """<?php
          |#[Attr]
          |#[Other(1, [2, 3], "]")]
          |final class Annotated {
          |  #[Pure]
          |  public static function m(
          |    #[SensitiveParameter] $secret
          |  ) {}
          |  #[Deprecated]
          |  protected function n() {}
          |}
          |#[Attr]
          |interface AnnotatedI {}
          |""".stripMargin)

      run.names shouldBe Seq("Annotated", "AnnotatedI")
      strings(recordOf(run, "Annotated")("modifiers")) shouldBe Seq("final")
      methodsOf(recordOf(run, "Annotated")) shouldBe Seq("m" -> Seq("public", "static"), "n" -> Seq("protected"))
    }
  }

  // ---- Methods -----------------------------------------------------------------------------------------------------

  "methods with semi-reserved names and return by reference" should {
    "be recognised" in {
      val names =
        Seq("list", "print", "new", "default", "function", "class", "match", "fn", "echo", "array", "static", "use")
      val body = names.map(name => s"public function $name() {}").mkString("\n")
      val run  =
        scan("Words.php" -> s"<?php class Words {\n$body\nfunction &getRef() {}\nstatic function & spaced() {}\n}")
      methodNames(recordOf(run, "Words")) shouldBe names ++ Seq("getRef", "spaced")
    }
  }

  "a method declared twice" should {
    "be reported once, with the first declaration, compared case-insensitively" in {
      val run = scan("Twice.php" -> "<?php class Twice { function Foo() {} private static function foo() {} }")
      methodsOf(recordOf(run, "Twice")) shouldBe Seq("Foo" -> Nil)
    }
  }

  "modifiers" should {
    "not leak from properties, constants, `use` or other methods into the next method" in {
      val run = scan("Leak.php" -> """<?php
          |abstract class Leak {
          |  use SomeTrait { foo as protected bar; }
          |  function afterUse() {}
          |  public static $prop = 1;
          |  function afterProperty() {}
          |  final public const C = 1;
          |  function afterConst() {}
          |  abstract protected function abstractOne();
          |  function afterAbstract() {}
          |  public function __construct(private readonly int $x, protected $y) {}
          |  function afterPromotion() {}
          |  private ?int $typed = null;
          |  function afterTyped() {}
          |}
          |""".stripMargin)

      methodsOf(recordOf(run, "Leak")) shouldBe Seq(
        "afterUse"       -> Nil,
        "afterProperty"  -> Nil,
        "afterConst"     -> Nil,
        "abstractOne"    -> Seq("abstract", "protected"),
        "afterAbstract"  -> Nil,
        "__construct"    -> Seq("public"),
        "afterPromotion" -> Nil,
        "afterTyped"     -> Nil
      )
    }
  }

  "closures, arrow functions and nested named functions" should {
    "not be reported as methods" in {
      val run = scan("Nest.php" -> """<?php
          |class Nest {
          |  function m() {
          |    $a = function ($x) use ($y) { return function () {}; };
          |    $b = fn($x) => fn($y) => $x + $y;
          |    $c = static function () {};
          |    function innerNamed() {}
          |    if (true) { function innerConditional() {} }
          |  }
          |  function n(): \Closure { return function () {}; }
          |}
          |function free() {}
          |""".stripMargin)

      methodNames(recordOf(run, "Nest")) shouldBe Seq("m", "n")
    }
  }

  "interface, abstract, trait and enum methods" should {
    "be reported on their own type" in {
      val run = scan("Kinds.php" -> """<?php
          |class C { function c() {} }
          |interface I { public function i(); public static function j(): void; }
          |abstract class A { abstract public function a(); }
          |trait T { abstract function t(); private function u() {} }
          |enum E { case X; public static function e(): self { return self::X; } }
          |""".stripMargin)

      run.records.map(record => record("name").str -> methodsOf(record)) shouldBe Seq(
        "C" -> Seq("c" -> Nil),
        "I" -> Seq("i" -> Seq("public"), "j" -> Seq("public", "static")),
        "A" -> Seq("a" -> Seq("abstract", "public")),
        "T" -> Seq("t" -> Seq("abstract"), "u" -> Seq("private")),
        "E" -> Seq("e" -> Seq("public", "static"))
      )
    }
  }

  // ---- Braces that are not blocks ----------------------------------------------------------------------------------

  "string interpolation and braces in strings or comments" should {
    "not throw off the nesting" in {
      val run = scan("Interp.php" -> """<?php
          |class Interp {
          |  function a() { return "{$this->x} ${y} {$a['}']} {$b->c->d} ${z['k']}"; }
          |  function b() { return <<<EOT
          |    {$this->x} ${y} } {
          |EOT;
          |  }
          |  function c() { return '{' . "}" . '}}}'; } // } {
          |  /* { { { */
          |  function d() {}
          |}
          |function notAMethod() {}
          |""".stripMargin)

      methodNames(recordOf(run, "Interp")) shouldBe Seq("a", "b", "c", "d")
    }
  }

  "trait adaptation blocks" should {
    "not end the type body" in {
      val run = scan("Adapt.php" -> """<?php
          |class Adapt {
          |  use A, B { A::foo insteadof B; B::foo as protected bar; }
          |  function after() {}
          |}
          |""".stripMargin)
      methodNames(recordOf(run, "Adapt")) shouldBe Seq("after")
    }
  }

  // ---- Namespaces --------------------------------------------------------------------------------------------------

  "namespaces" should {
    "come from the declarations, not from the directory" in {
      val run = scan("Lib/Wrong/Place.php" -> "<?php namespace Other\\Place; class Here {}")
      recordOf(run, "Here")("namespace").str shouldBe "Other\\Place"
    }

    "follow `namespace X;` switches in one file" in {
      val run = scan("Multi.php" -> """<?php
          |namespace First;
          |class InFirst {}
          |namespace Second\Level;
          |class InSecond {}
          |""".stripMargin)
      run.records.map(record => record("name").str -> record("namespace").str) shouldBe Seq(
        "InFirst"  -> "First",
        "InSecond" -> "Second\\Level"
      )
    }

    "follow braced namespace blocks, including the global one" in {
      val run = scan("Braced.php" -> """<?php
          |namespace A\B { class InAB { function m() { if (true) {} } } }
          |namespace { class InGlobal {} }
          |namespace C { interface InC {} }
          |""".stripMargin)
      run.records.map(record => record("name").str -> record("namespace").str) shouldBe Seq(
        "InAB"     -> "A\\B",
        "InGlobal" -> "",
        "InC"      -> "C"
      )
    }

    "not change on the relative `namespace\\Name` form" in {
      val run = scan("Rel.php" -> """<?php
          |namespace Real;
          |namespace\helper();
          |$x = new namespace\Other();
          |class Kept extends namespace\Base {}
          |""".stripMargin)
      inside(run.records) { case Seq(kept) =>
        kept("namespace").str shouldBe "Real"
        strings(kept("extends")) shouldBe Seq("namespace\\Base")
      }
    }

    "be normalized when comments surround the name" in {
      val run = scan("Comments.php" -> "<?php namespace /* before */ Foo\\Bar /* after */ ; class C {}")
      recordOf(run, "C")("namespace").str shouldBe "Foo\\Bar"
    }

    "be the empty string for files without a namespace declaration" in {
      inside(scan("Deep/Dir/G.php" -> "<?php class G {}").records) { case Seq(record) =>
        record("namespace").str shouldBe ""
      }
    }
  }

  // ---- Supertypes --------------------------------------------------------------------------------------------------

  "supertype names" should {
    "be reported as written, in source order, whatever the PHP version's tokenizer" in {
      val run = scan("Super.php" -> """<?php
          |namespace N;
          |class C extends \Fully\Qualified\Base implements Partly\Qualified, Plain, \Root {}
          |interface I extends \A\B, C\D, E {}
          |class D extends /* comment */ Base /* comment */ implements /* c */ \X\Y /* c */ , Z {}
          |""".stripMargin)

      strings(recordOf(run, "C")("extends")) shouldBe Seq("\\Fully\\Qualified\\Base")
      strings(recordOf(run, "C")("implements")) shouldBe Seq("Partly\\Qualified", "Plain", "\\Root")
      strings(recordOf(run, "I")("extends")) shouldBe Seq("\\A\\B", "C\\D", "E")
      strings(recordOf(run, "I")("implements")) shouldBe Nil
      strings(recordOf(run, "D")("extends")) shouldBe Seq("Base")
      strings(recordOf(run, "D")("implements")) shouldBe Seq("\\X\\Y", "Z")
    }
  }

  // ---- Regressions for earlier defects ------------------------------------------------------------------------------

  "the defects of the previous implementation" should {
    "not come back" in {
      val run = scan(
        "B/Same.php" -> "<?php namespace B; class Same {}",
        "A/Same.php" -> "<?php namespace A; class Same {}",
        "Mix.php"    -> """<?php
            |class First { function f() {} }
            |interface Follows { function fromInterface(); }
            |$x = [A::class, B::class];
            |$y = new class($a) extends AnonBase {};
            |class Strings { function s() { return "{$x}"; } function t() {} }
            |final class Fin { final public function g() { function nested() {} } function list() {} }
            |""".stripMargin
      )
      // 1: short names do not collapse, 10: deterministic order
      run.records.filter(_("name").str == "Same").map(_("namespace").str) shouldBe Seq("A", "B")
      // 2: interfaces are reported and do not lend methods to the class before them
      methodNames(recordOf(run, "First")) shouldBe Seq("f")
      methodNames(recordOf(run, "Follows")) shouldBe Seq("fromInterface")
      // 3, 4: no class from `::class` or from an anonymous class
      run.names should contain noneOf ("A", "B", "AnonBase")
      // 5: interpolation does not drop later methods
      methodNames(recordOf(run, "Strings")) shouldBe Seq("s", "t")
      // 6, 7, 8: no nested functions, semi-reserved names, `final` kept
      methodsOf(recordOf(run, "Fin")) shouldBe Seq("g" -> Seq("final", "public"), "list" -> Nil)
      strings(recordOf(run, "Fin")("modifiers")) shouldBe Seq("final")
      // 13: nothing before the JSON on stdout
      run.stdOut.head should startWith("[")
    }
  }
}
