package io.joern.erlang2cpg.parser

import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}

import java.io.IOException
import java.nio.file.{Files, Path}
import scala.util.Using

final class SourceParseException(message: String) extends RuntimeException(message)

object OtpParser {
  private val helperResource = "/io/joern/erlang2cpg/parse.escript"

  /** Each translation unit has an independent macro environment. The helper runs in a private working directory, with
    * source and include paths passed as separate arguments. Only epp reads the project.
    */
  def parse(file: Path, includes: Seq[Path] = Nil, macros: Seq[String] = Nil): ParsedModule = {
    val source = file.toAbsolutePath.normalize
    FileUtil.usingTemporaryDirectory("erlang2cpg") { temp =>
      val helper = temp.resolve("parse.escript")
      val output = temp.resolve("forms.json")
      val stream = Option(getClass.getResourceAsStream(helperResource))
        .getOrElse(throw new IOException(s"Missing packaged OTP helper $helperResource"))
      Using.resource(stream)(input => Files.copy(input, helper))
      val args = Seq("escript", helper.toString, source.toString, output.toString) ++
        includes.flatMap(path => Seq("-I", path.toAbsolutePath.normalize.toString)) ++
        macros.flatMap(macroDefinition => Seq("-D", macroDefinition))
      val result = ExternalCommand.run(args, workingDir = Some(temp))
      if (!result.successful) {
        val detail = result.stdOutAndError.mkString("\n")
        if (result.exitCode == 2 && Files.isRegularFile(output)) {
          val message = ujson.read(Files.readAllBytes(output))("sourceError").str
          throw new SourceParseException(message)
        }
        throw new IOException(s"erlang2cpg requires escript from Erlang/OTP 27. Helper failed for $source: $detail")
      }
      val response    = ujson.read(Files.readAllBytes(output))
      val autoImports = response("autoImports").arr.iterator.map(pair => pair(0).str -> pair(1).num.toInt).toSet
      var activeFile  = source.toString
      val forms       = response("forms").arr.iterator.map { encoded =>
        val term = Term.decode(encoded)
        term match {
          case Term.Tuple(Vector(Term.Atom("attribute"), _, Term.Atom("file"), Term.Tuple(Vector(name, _)))) =>
            activeFile = name.text
          case _ =>
        }
        Form(activeFile, term)
      }.toVector
      ParsedModule(source, forms, autoImports)
    }
  }
}
