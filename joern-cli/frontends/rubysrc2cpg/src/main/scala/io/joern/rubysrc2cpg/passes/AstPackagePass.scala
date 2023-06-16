package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.utils.{PackageContext, PackageTable}
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._

import java.io.File
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.util.Try

class AstPackagePass(cpg: Cpg, tempExtDir: String, global: Global, packageTable: PackageTable, inputPath: String)
    extends ConcurrentWriterCpgPass[String](cpg) {
  override def generateParts(): Array[String] = getRubyDependenciesFile(inputPath)

  override def runOnPart(diffGraph: DiffGraphBuilder, filePath: String): Unit = {
    Try(new AstCreator(filePath, global, PackageContext(resolveModuleNameFromPath(filePath), packageTable)).createAst())
  }

  private def getRubyDependenciesFile(inputPath: String): Array[String] = {
    Files
      .walk(Paths.get(inputPath))
      .filter(_.toString.endsWith(".rb"))
      .map(_.toString)
      .toArray
      .map(_.asInstanceOf[String])
  }

  private def resolveModuleNameFromPath(path: String): String = {
    if (path.contains(tempExtDir)) {
      val moduleNameRegex = """\/gems\/([^\/]+)\/lib\/.*""".r
      moduleNameRegex.findFirstMatchIn(path).map(_.group(1)).getOrElse("").split("/").last.split("-").head
    } else {
      path
    }
  }
}
