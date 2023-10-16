package io.joern.jimple2cpg.testfixtures

import better.files.File as BFile
import io.joern.jimple2cpg.{Config, Jimple2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend, TestCpg}
import io.shiftleft.codepropertygraph.Cpg

import java.io.File
import java.nio.file.Path
import java.util.Collections
import javax.tools.{JavaCompiler, JavaFileObject, StandardLocation, ToolProvider}
import scala.jdk.CollectionConverters.IterableHasAsJava

trait Jimple2CpgFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".java"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config = Config()
    new Jimple2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class JimpleCode2CpgFixture() extends Code2CpgFixture(() => new JimpleTestCpg()) {}

class JimpleTestCpg() extends TestCpg with Jimple2CpgFrontend {

  override protected def codeDirPreProcessing(rootFile: Path, codeFiles: List[Path]): Unit =
    JimpleCodeToCpgFixture.compileJava(rootFile, codeFiles.map(_.toFile))

  override protected def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)
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
