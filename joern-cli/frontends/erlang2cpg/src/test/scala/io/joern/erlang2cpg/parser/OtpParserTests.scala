package io.joern.erlang2cpg.parser

import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class OtpParserTests extends AnyFunSuite {
  private def functions(unit: ParsedModule): Vector[Form] = unit.forms.filter(_.term.tag == "function")

  test("preserve OTP terms and locations for an individual source with spaces") {
    FileUtil.usingTemporaryDirectory("otp source ") { root =>
      val source = Files.writeString(root.resolve("id demo.erl"), "-module(id_demo).\nid(X) -> X.\n")
      val parsed = OtpParser.parse(source)
      val form   = functions(parsed).ensuring(_.size == 1).head
      assert(form.file == source.toString)
      assert(form.term(2) == Term.Atom("id"))
      assert(form.term(3) == Term.Integer(1))
      assert(form.term.line.contains(2) && form.term.column.contains(1))
      val clause = form.term(4).items.ensuring(_.size == 1).head
      val input  = clause(2).items.ensuring(_.size == 1).head
      val body   = clause(4).items.ensuring(_.size == 1).head
      assert(input(2) == Term.Atom("X") && input.column.contains(4))
      assert(body(2) == Term.Atom("X") && body.column.contains(10))
      assert(parsed.autoImports.contains("is_integer" -> 1))
    }
  }

  test("expand a shared header separately for each module, preserving included function location") {
    FileUtil.usingTemporaryDirectory("otp-includes") { root =>
      val includes = Files.createDirectory(root.resolve("include dir"))
      val header   = Files.writeString(
        includes.resolve("shared.hrl"),
        "-ifdef(ENABLED).\nchosen() -> ?VALUE.\n-else.\nchosen() -> disabled.\n-endif.\n"
      )
      def unit(name: String, macros: Seq[String]): ParsedModule = {
        val source = Files.writeString(
          root.resolve(s"$name.erl"),
          s"""-module($name).
             |-include("shared.hrl").
             |-include_lib("kernel/include/file.hrl").
             |record() -> #file_info{}.
             |""".stripMargin
        )
        OtpParser.parse(source, Seq(includes), macros)
      }
      val large    = BigInt("123456789123456789123456789")
      val versions = Seq(
        (unit("first", Seq("ENABLED", s"VALUE=$large")), 2, Term.Integer(large)),
        (unit("second", Seq("ENABLED", "VALUE=alternate")), 2, Term.Atom("alternate")),
        (unit("third", Nil), 4, Term.Atom("disabled"))
      )
      versions.foreach { case (parsed, line, literal) =>
        val methods = functions(parsed)
        assert(methods.map(_.term(2).text) == Vector("chosen", "record"))
        assert(Path.of(methods.head.file).normalize == header)
        assert(methods.head.term.line.contains(line))
        assert(methods.head.term(4)(0)(4)(0)(2) == literal)
        assert(methods(1).file == parsed.file.toString)
        assert(parsed.forms.exists(form => form.file.endsWith("file.hrl") && form.term(0).text == "attribute"))
      }
    }
  }

  test("embedded missing-include and syntax errors reject the file while a sibling still parses") {
    FileUtil.usingTemporaryDirectory("otp-errors") { root =>
      val good = Files.writeString(root.resolve("good.erl"), "-module(good).\nf() -> ok.\n")
      val bad  = Files.writeString(root.resolve("bad.erl"), "-module(bad).\n-include(\"missing.hrl\").\nf() -> ok.\n")
      val missing = intercept[SourceParseException](OtpParser.parse(bad))
      assert(missing.getMessage.contains("missing.hrl"))
      assert(missing.getMessage.contains("bad.erl"))
      Files.writeString(bad, "-module(bad).\nf({,}) -> ok.\n")
      assert(intercept[SourceParseException](OtpParser.parse(bad)).getMessage.contains("syntax error"))
      assert(functions(OtpParser.parse(good)).map(_.term(2).text) == Vector("f"))
    }
  }

  test("a nonexistent parse transform is an inert attribute") {
    FileUtil.usingTemporaryDirectory("otp-transform") { root =>
      val source = Files.writeString(
        root.resolve("transform.erl"),
        "-module(transform).\n-compile({parse_transform,joern_nonexistent_transform}).\nf() -> untouched.\n"
      )
      assert(functions(OtpParser.parse(source)).head.term(4)(0)(4)(0)(2) == Term.Atom("untouched"))
    }
  }

  test("helper configuration failures are distinct from source diagnostics") {
    FileUtil.usingTemporaryDirectory("otp-helper") { root =>
      val source = Files.writeString(root.resolve("valid.erl"), "-module(valid).\n")
      val error  = intercept[java.io.IOException](OtpParser.parse(source, Nil, Seq("DEF=not_a_term()")))
      assert(error.getMessage.contains("Helper failed"))
    }
  }
}
