package io.joern.jimple2cpg.testfixtures

import io.joern.jimple2cpg.{Config, Jimple2Cpg}
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.{CodeToCpgFixture, LanguageFrontend}

import java.io.{File, PrintWriter}
import java.nio.file.Files
import java.util.Collections
import javax.tools.{JavaCompiler, JavaFileObject, StandardLocation, ToolProvider}
import scala.jdk.CollectionConverters

class JimpleFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".java"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config = Config()
    new Jimple2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class JimpleCodeToCpgFixture extends CodeToCpgFixture(new JimpleFrontend) {

  override def writeCodeToFile(sourceCode: String): File = {
    val tmpDir = Files.createTempDirectory("semanticcpgtest").toFile
    tmpDir.deleteOnExit()
    val codeFile = File.createTempFile("Test", frontend.fileSuffix, tmpDir)
    codeFile.deleteOnExit()
    new PrintWriter(codeFile) { write(sourceCode); close() }
    JimpleCodeToCpgFixture.compileJava(codeFile)
    tmpDir
  }

}

object JimpleCodeToCpgFixture {

  /** Compiles the source code with debugging info.
    */
  def compileJava(sourceCodeFile: File): Unit = {
    val javac       = getJavaCompiler
    val fileManager = javac.getStandardFileManager(null, null, null)
    javac
      .getTask(
        null,
        fileManager,
        null,
        CollectionConverters.SeqHasAsJava(Seq("-g", "-d", sourceCodeFile.getParent)).asJava,
        null,
        fileManager.getJavaFileObjectsFromFiles(CollectionConverters.SeqHasAsJava(Seq(sourceCodeFile)).asJava)
      )
      .call()

    fileManager
      .list(StandardLocation.CLASS_OUTPUT, "", Collections.singleton(JavaFileObject.Kind.CLASS), false)
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
