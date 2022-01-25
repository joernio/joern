package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.{Kt2Cpg, KtFileWithMeta}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import org.slf4j.LoggerFactory
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment
import com.intellij.psi.PsiManager
import com.intellij.testFramework.LightVirtualFile
import io.joern.kotlin2cpg.types.NameGenerator
import org.jetbrains.kotlin.idea.KotlinFileType
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.cli.common.messages.{
  CompilerMessageSeverity,
  CompilerMessageSourceLocation,
  MessageCollector
}

import java.nio.file.Paths
import java.util.concurrent.ConcurrentHashMap

case class Global(
    usedTypes: ConcurrentHashMap[String, Boolean] = new ConcurrentHashMap[String, Boolean]()
)

class AstCreationPass(
    filesWithMeta: Iterable[KtFileWithMeta],
    nameGenerator: NameGenerator,
    cpg: Cpg,
    keyPool: IntervalKeyPool
) extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(filesWithMeta.size))) {
  val global: Global = Global()
  private val logger = LoggerFactory.getLogger(getClass)

  override def partIterator: Iterator[String] = {
    filesWithMeta.map { ktFileWithMeta => ktFileWithMeta.f.getVirtualFilePath }.iterator
  }

  override def runOnPart(filename: String): Iterator[DiffGraph] = {
    val fileWithMeta = filesWithMeta
      .filter { ktFileWithMeta =>
        ktFileWithMeta.f.getVirtualFilePath == filename
      }
      .toList
      .headOption
    fileWithMeta match {
      case Some(fm) =>
        val diffGraph =
          new AstCreator(fm, nameGenerator, global).createAst()
        logger.debug("AST created for file at `" + filename + "`.")
        diffGraph
      case None =>
        logger.info("Could not find file at `" + filename + "`.")
        Iterator[DiffGraph]()
    }
  }
}

// Without passing an instance of this class to the CompilerConfiguration for
// KotlinCoreEnviroment, executions throw `IllegalStateExceptions` with the message
// `(no MessageCollector configured)`. The companion object of MessageCollector would have been an alternative,
// unfortunately Scala-Kotlin interop is not good enough to allow to pass it in the `put` method of the config.
class EmptyMessageCollector extends MessageCollector {
  override def report(
      compilerMessageSeverity: CompilerMessageSeverity,
      s: String,
      compilerMessageSourceLocation: CompilerMessageSourceLocation
  ): Unit = {}
  override def hasErrors: Boolean = false
  override def clear(): Unit = {}
}
