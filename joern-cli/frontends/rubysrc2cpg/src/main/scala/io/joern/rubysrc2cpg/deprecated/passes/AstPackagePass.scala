package io.joern.rubysrc2cpg.deprecated.passes

import better.files.File
import io.joern.rubysrc2cpg.deprecated.astcreation.{AstCreator, ResourceManagedParser}
import io.joern.rubysrc2cpg.deprecated.utils.{PackageContext, PackageTable}
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

class AstPackagePass(
  cpg: Cpg,
  tempExtDir: String,
  parser: ResourceManagedParser,
  packageTable: PackageTable,
  inputPath: String
)(implicit withSchemaValidation: ValidationMode)
    extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[String] =
    getRubyDependenciesFile(inputPath) ++ getRubyDependenciesFile(tempExtDir)

  override def runOnPart(diffGraph: DiffGraphBuilder, filePath: String): Unit = {
    parser.parse(filePath) match
      case Failure(exception) => logger.warn(s"Could not parse file: $filePath, skipping", exception);
      case Success(programCtx) =>
        Try(
          new AstCreator(
            filePath,
            programCtx,
            PackageContext(resolveModuleNameFromPath(filePath), packageTable),
            Option(inputPath)
          ).createAst()
        )

  }

  private def getRubyDependenciesFile(inputPath: String): Array[String] = {
    val currentDir = File(inputPath)
    if (currentDir.exists) {
      currentDir.listRecursively.filter(_.extension.exists(_ == ".rb")).map(_.path.toString).toArray
    } else {
      Array.empty
    }
  }

  private def resolveModuleNameFromPath(path: String): String = {
    if (path.contains(tempExtDir)) {
      val moduleNameRegex = Seq("gems", "([^", "]+)", "lib", ".*").mkString(java.io.File.separator).r
      moduleNameRegex
        .findFirstMatchIn(path)
        .map(_.group(1))
        .getOrElse("")
        .split(java.io.File.separator)
        .last
        .split("-")
        .head
    } else {
      path
    }
  }
}
