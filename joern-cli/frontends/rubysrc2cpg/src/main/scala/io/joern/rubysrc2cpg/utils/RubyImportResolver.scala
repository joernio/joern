package io.joern.rubysrc2cpg.utils

import better.files.File as BFile
import io.joern.x2cpg.datastructures.DefaultImportResolver.*
import io.joern.x2cpg.datastructures.ImportResolver.FileSystem.FileSystemBuilder
import io.joern.x2cpg.datastructures.ImportResolver.{Entity, FileSystem, Import, Resolvable}
import io.joern.x2cpg.datastructures.{FileSystemAdapter, ImportResolver}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Method, TypeDecl, File}
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

class RubySrcImportResolver(cpg: Cpg, fileSystemAdapter: FileSystemAdapter) extends ImportResolver {

  override protected lazy val fileSystem: ImportResolver.FileSystem = fileSystemAdapter.fileSystem

  override def resolve(i: Import): Either[Entity, Seq[Entity & Resolvable[?]]] = ???

}

trait RubySrcFileSystemAdapter(cpg: Cpg, additionalLoaderPaths: Seq[String] = Seq.empty) extends FileSystemAdapter {

  override def fileSystem: ImportResolver.FileSystem = {
    val builder = FileSystemBuilder(cpg.metaData.root.l ++ additionalLoaderPaths)
    // TODO: Look out for `require_all 'path/to/directory'`
    // Build packages to accommodate for `require_all`
    val packages = mutable.Map.empty[String, RegularPackage]
    cpg.file.foreach(extractFileEntities(_, packages.updateWith))
    // TODO Link `packages` with children/parents
    builder.build
  }

  private def extractFileEntities(
    file: File,
    updatePackage: String => (Option[RegularPackage] => Option[RegularPackage]) => Option[RegularPackage]
  ): Unit = {
    val filename = file.name
    // Extract exported modules
    val modules = file.method.isModule.map { module =>
      def variables = module.local.moduleVariables.map { mv => ResolvableVariable(mv.name, filename, () => mv) }.toSeq

      ResolvableModule(module.name, module.filename, variables, None, Seq.empty, () => module)
    }.toSeq
    // TODO: Include more entities
    val entities = modules
    // Add new package entry
    val parent = BFile(filename).parent
    updatePackage(parent.toString) {
      case Some(pack) => Option(pack.withEntities(entities))
      case None       => Option(new RegularPackage(parent.name, parent.toString, entities))
    }
  }

}
