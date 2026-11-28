package io.joern.erlang2cpg

import io.joern.erlang2cpg.ast.ErlangAstPass
import io.joern.erlang2cpg.parser.{OtpParser, SourceParseException}
import io.joern.x2cpg.{SourceFiles, X2Cpg, X2CpgFrontend}
import io.joern.x2cpg.frontendspecific.ErlangLanguage
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import scala.util.Try

class Erlang2Cpg extends X2CpgFrontend {
  override type ConfigType = Config
  override val defaultConfig: Config = Config()
  private val logger                 = LoggerFactory.getLogger(getClass)

  override def createCpg(config: Config): Try[Cpg] = Try {
    val files = SourceFiles.determine(
      config.inputPath,
      Set(".erl"),
      Some(config.defaultIgnoredFilesRegex),
      Some(config.ignoredFilesRegex),
      Some(config.ignoredFiles)
    )
    val includes = config.includes.map(dir => Path.of(dir).toAbsolutePath.normalize())
    val units    = files.flatMap { file =>
      try Some(OtpParser.parse(Path.of(file), includes, config.macros))
      catch {
        case err: SourceParseException =>
          logger.warn(s"Skipping Erlang module $file: ${err.getMessage}")
          None
      }
    }
    require(units.nonEmpty, s"No Erlang modules parsed from ${config.inputPath}")
    units.groupBy(_.moduleName).toSeq.sortBy(_._1).foreach { case (name, sources) =>
      require(sources.size == 1, s"Duplicate Erlang module $name: ${sources.map(_.file).mkString(", ")}")
    }
    units
  }.flatMap { units =>
    X2Cpg.withNewEmptyCpg(config.outputPath, config) { (cpg, _) =>
      MetaDataPass(cpg, ErlangLanguage, config.inputPath).createAndApply()
      new ErlangAstPass(cpg, Path.of(config.inputPath), units).createAndApply()
      TypeNodePass.withTypesFromCpg(cpg).createAndApply()
    }
  }

  /** Publish only a successfully closed graph, including when updating a previous import. */
  override def run(config: Config): Unit = {
    if (config.outputPath.isEmpty) super.run(config)
    else {
      val output    = Path.of(config.outputPath).toAbsolutePath
      val temporary = Files.createTempDirectory(output.getParent, ".erlang2cpg-")
      val candidate = temporary.resolve("cpg.bin")
      try {
        super.run(config.withOutputPath(candidate.toString))
        try Files.move(candidate, output, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        catch {
          case _: AtomicMoveNotSupportedException => Files.move(candidate, output, StandardCopyOption.REPLACE_EXISTING)
        }
      } finally {
        Files.deleteIfExists(candidate)
        Files.delete(temporary)
      }
    }
  }
}
