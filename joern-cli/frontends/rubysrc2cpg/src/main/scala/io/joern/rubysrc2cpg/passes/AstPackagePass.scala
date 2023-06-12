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

class AstPackagePass(cpg: Cpg, gemPaths: List[String], global: Global, packageTable: PackageTable, inputPath: String)
    extends ConcurrentWriterCpgPass[String](cpg) {
  override def generateParts(): Array[String] = getAllPackage(cpg)

  override def runOnPart(diffGraph: DiffGraphBuilder, modulePart: String): Unit = {

    val module = PackageTable.resolveImportPath(modulePart)

    val packageFiles = ListBuffer[String]()

    if (Files.isRegularFile(Paths.get(module))) {
      packageFiles.addOne(module)
    }

    gemPaths
      .flatMap(path => {
        val fullDirectoryName = Paths.get(getDependencyPath(path, module).getOrElse(""))

        if (Files.isDirectory(fullDirectoryName)) {
          Files.walk(fullDirectoryName).filter(p => p.toString.endsWith(".rb")).map(_.toString).toArray
        } else {
          Array.empty[String]
        }
      })
      .foreach(file => packageFiles += file.toString)

    packageFiles.foreach(filePath => {
      if (!filePath.matches(s".*${inputPath}.*")) {
        Try(new AstCreator(filePath, global, PackageContext(module, packageTable)).createAst())
      }
    })
  }

  private def getAllPackage(cpg: Cpg): Array[String] = {
    cpg.imports.code.l.toArray
  }

  private def getDependencyPath(directoryPath: String, dependencyName: String): Option[String] = {
    val directory = new File(s"${directoryPath}/gems")

    if (directory.exists && directory.isDirectory) {
      val dependencies = directory.listFiles.filter(_.isDirectory)
      dependencies.find(_.getName.contains(dependencyName)).map(_.getPath)
    } else {
      None
    }
  }
}
