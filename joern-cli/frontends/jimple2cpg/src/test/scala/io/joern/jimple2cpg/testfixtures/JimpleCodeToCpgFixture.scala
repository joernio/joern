package io.joern.jimple2cpg.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.jimple2cpg.{Config, Jimple2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend, TestCpg}
import io.shiftleft.codepropertygraph.generated.Cpg

import java.io.File
import java.nio.file.Path
import java.util.Collections
import javax.tools.{JavaCompiler, JavaFileObject, StandardLocation, ToolProvider}
import scala.jdk.CollectionConverters.IterableHasAsJava

trait Jimple2CpgFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".java"

  override def execute(sourceCodeFile: File): Cpg = {
    val config = getConfig().map(_.asInstanceOf[Config]).getOrElse(Config())
    new Jimple2Cpg().createCpg(sourceCodeFile.getAbsolutePath)(config).get
  }
}

class JimpleCode2CpgFixture(withOssDataflow: Boolean = false, semantics: Semantics = DefaultSemantics())
    extends Code2CpgFixture(() => new JimpleTestCpg().withOssDataflow(withOssDataflow).withSemantics(semantics))
    with SemanticCpgTestFixture(semantics) {}

class JimpleTestCpg extends DefaultTestCpg with Jimple2CpgFrontend with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }

  override protected def codeDirPreProcessing(rootFile: Path, codeFiles: List[Path]): Unit = {
    val sourceFiles = codeFiles.map(_.toFile).filter(_.getName.endsWith(".java"))
    if (sourceFiles.nonEmpty) JimpleCodeToCpgFixture.compileJava(rootFile, sourceFiles)
  }

}

object JimpleCodeToCpgFixture {

  /** Compiles the source code with debugging info.
    */
  def compileJava(root: Path, sourceCodeFiles: List[File]): Unit = {
    val javac       = getJavaCompiler
    val fileManager = javac.getStandardFileManager(null, null, null)
    javac
      .getTask(
        null,
        fileManager,
        null,
        Seq("-g", "-d", root.toString).asJava,
        null,
        fileManager.getJavaFileObjectsFromFiles(sourceCodeFiles.asJava)
      )
      .call()

    fileManager
      .list(StandardLocation.CLASS_OUTPUT, "", Collections.singleton(JavaFileObject.Kind.CLASS), true)
      .forEach(x => new File(x.toUri).deleteOnExit())
  }

  /** Programmatically obtains the system Java compiler.
    */
  def getJavaCompiler: JavaCompiler = {
    Option(ToolProvider.getSystemJavaCompiler) match {
      case Some(javac) => javac
      case None        => throw new RuntimeException("Unable to find a Java compiler on the system!")
    }
  }
}
