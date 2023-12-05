package io.joern.csharpsrc2cpg.utils

import better.files.File
import com.typesafe.config.ConfigFactory
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.{Environment, ExternalCommand}
import io.joern.x2cpg.astgen.AstGenRunnerBase
import org.slf4j.LoggerFactory
import versionsort.VersionHelper
import io.joern.csharpsrc2cpg.Config
import io.joern.x2cpg.astgen.AstGenRunner.AstGenProgramMetaData

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

class DotNetAstGenRunner(config: Config) extends AstGenRunnerBase(config) {

  override def fileFilter(file: String, out: File): Boolean = {
    file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath) => false
      case filePath if filePath.endsWith(".csproj")    => false
      case _                                           => true
    }
  }

  override def runAstGenNative(in: String, out: File, exclude: String)(implicit
    metaData: AstGenProgramMetaData
  ): Try[Seq[String]] = {
    // TODO: Need to add exclude support in dotnetAstgen
    // val excludeCommand = if (exclude.isEmpty) "" else s"-exclude \"$exclude\""
    ExternalCommand.run(s"$astGenCommand -o ${out.toString()} -i $in", ".")
  }

}
