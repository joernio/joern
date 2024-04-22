package io.joern.php2cpg.passes

import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl

import scala.util.{Failure, Success}

/**
 * This pass parses the high-level symbols of the dependencies to include their types to the CPG.
 * @param cpg the code property graph.
 * @param dependencyDir the directory holding the downloaded dependencies.
 */
class DependencySymbolsPass(cpg: Cpg, dependencyDir: File) extends ConcurrentWriterCpgPass[ClassParserClass](cpg) {

  override def generateParts(): Array[ClassParserClass] = new ClassParser(dependencyDir).parse() match {
    case Success(classes) => classses.toArray
    case Failure(exception) => 
      logger.error("Exception encountered while parsing symbols from dependencies", exception)
      Array.empty
  }

  override def runOnPart(builder: DiffGraphBuilder, clazz: ClassParserClass): Unit = {
    val className = clazz.name
    val classFullName = {
      val classParentDir = clazz.file.parent.pathAsString
      classParentDir.stripPrefix(dependencyDir.pathAsString).replaceAll(java.io.File.separator, "\\\\")
    }
    val typeDecl = NewTypeDecl()
      .name(clazz.name)
      .fullName(classFullName)
      .isExternal(true)

    builder.addNode(typeDecl)
  }

}
