package io.joern.rubysrc2cpg.utils

import io.joern.x2cpg.datastructures.DefaultImportResolver.*
import io.joern.x2cpg.datastructures.ImportResolver.FileSystem.FileSystemBuilder
import io.joern.x2cpg.datastructures.ImportResolver.{Entity, FileSystem, Import, Resolvable}
import io.joern.x2cpg.datastructures.{FileSystemAdapter, ImportResolver}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Method, TypeDecl}
import io.shiftleft.semanticcpg.language.*

class RubySrcImportResolver(cpg: Cpg, fileSystemAdapter: FileSystemAdapter) extends ImportResolver {

  override protected lazy val fileSystem: ImportResolver.FileSystem = fileSystemAdapter.fileSystem

  override def resolve(i: Import): Either[Entity, Seq[Entity & Resolvable[?]]] = ???

}

trait RubySrcFileSystemAdapter(cpg: Cpg, additionalLoaderPaths: Seq[String] = Seq.empty) extends FileSystemAdapter {

  override def fileSystem: ImportResolver.FileSystem = {
    val builder = FileSystemBuilder(cpg.metaData.root.l ++ additionalLoaderPaths)
    // TODO: Look out for `require_all 'path/to/directory'`
    // TODO: Build packages to accommodate for `require_all`
    cpg.file.foreach { file =>
      val filename = file.name
      // Extract exported modules
      file.method.isModule.foreach { module =>
        def variables = module.local.moduleVariables.map { mv => ResolvableVariable(mv.name, filename, () => mv) }.toSeq

        ResolvableModule(module.name, module.filename, variables, None, Seq.empty, () => module)
      }
    }
    builder.build
  }

}
