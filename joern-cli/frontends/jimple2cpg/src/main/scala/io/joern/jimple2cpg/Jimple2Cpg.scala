package io.joern.jimple2cpg

import io.joern.jimple2cpg.passes.AstCreationPass
import io.joern.jimple2cpg.util.ProgramHandlingUtil
import io.joern.jimple2cpg.util.ProgramHandlingUtil.{extractSourceFilesFromArchive, moveClassFiles}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.shiftleft.x2cpg.SourceFiles
import io.shiftleft.x2cpg.X2Cpg.newEmptyCpg
import org.slf4j.LoggerFactory
import soot.options.Options
import soot.{G, PhaseOptions, Scene, SootClass}

import java.io.{File => JFile}
import java.nio.file.Paths
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.language.postfixOps

object Jimple2Cpg {
  val language = "JAVA"

  /** Formats the file name the way Soot refers to classes within a class path. e.g.
    * /unrelated/paths/class/path/Foo.class => class.path.Foo
    *
    * @param filename
    *   the file name to transform.
    * @return
    *   the correctly formatted class path.
    */
  def getQualifiedClassPath(filename: String): String = {
    val codePath = ProgramHandlingUtil.TEMP_DIR
    val codeDir: String = if (codePath.toFile.isDirectory) {
      codePath.toAbsolutePath.normalize.toString
    } else {
      Paths.get(codePath.toFile.getParentFile.getAbsolutePath).normalize.toString
    }
    filename
      .replace(codeDir + JFile.separator, "")
      .replace(".class", "")
      .replace(JFile.separator, ".")
  }
}

class Jimple2Cpg {

  import Jimple2Cpg._

  private val logger = LoggerFactory.getLogger(classOf[Jimple2Cpg])

  /** Creates a CPG from Jimple.
    *
    * @param rawSourceCodePath
    *   The path to the Jimple code or code that can be transformed into Jimple.
    * @param outputPath
    *   The path to store the CPG. If `outputPath` is `None`, the CPG is created in-memory.
    * @return
    *   The constructed CPG.
    */
  def createCpg(rawSourceCodePath: String, outputPath: Option[String] = None): Cpg = {
    try {
      // Determine if the given path is a file or directory and sanitize accordingly
      val rawSourceCodeFile = new JFile(rawSourceCodePath)
      val sourceTarget      = rawSourceCodeFile.toPath.toAbsolutePath.normalize.toString
      val sourceCodeDir = if (rawSourceCodeFile.isDirectory) {
        sourceTarget
      } else {
        Paths
          .get(new JFile(sourceTarget).getParentFile.getAbsolutePath)
          .normalize
          .toString
      }

      configureSoot()
      val cpg             = newEmptyCpg(outputPath)
      val metaDataKeyPool = new IntervalKeyPool(1, 100)
      val typesKeyPool    = new IntervalKeyPool(100, 1000100)
      val methodKeyPool   = new IntervalKeyPool(first = 1000100, last = Long.MaxValue)

      new MetaDataPass(cpg, language, Some(metaDataKeyPool)).createAndApply()

      val sourceFileExtensions  = Set(".class", ".jimple")
      val archiveFileExtensions = Set(".jar", ".war")
      // Load source files and unpack archives if necessary
      val sourceFileNames = if (sourceTarget == sourceCodeDir) {
        // Load all source files in a directory
        loadSourceFiles(sourceCodeDir, sourceFileExtensions, archiveFileExtensions)
      } else {
        // Load single file that was specified
        loadSourceFiles(sourceTarget, sourceFileExtensions, archiveFileExtensions)
      }

      logger.info(s"Loading ${sourceFileNames.size} program files")
      logger.debug(s"Source files are: $sourceFileNames")

      // Load classes into Soot
      loadClassesIntoSoot(sourceFileNames)
      // Project Soot classes
      val astCreator = new AstCreationPass(sourceCodeDir, sourceFileNames, cpg, methodKeyPool)
      astCreator.createAndApply()
      // Clear classes from Soot
      G.reset()

      new TypeNodePass(astCreator.global.usedTypes.asScala.toList, cpg, Some(typesKeyPool))
        .createAndApply()

      cpg
    } finally {
      clean()
    }
  }

  /** Load all source files from archive and/or source file types.
    */
  private def loadSourceFiles(
    sourceCodePath: String,
    sourceFileExtensions: Set[String],
    archiveFileExtensions: Set[String]
  ): List[String] = {
    (
      extractSourceFilesFromArchive(sourceCodePath, archiveFileExtensions) ++
        moveClassFiles(SourceFiles.determine(Set(sourceCodePath), sourceFileExtensions))
    ).distinct
  }

  private def loadClassesIntoSoot(sourceFileNames: List[String]): Unit = {
    sourceFileNames
      .map(getQualifiedClassPath)
      .foreach { cp =>
        Scene.v().addBasicClass(cp, SootClass.BODIES)
        Scene.v().loadClassAndSupport(cp).setApplicationClass()
      }
    Scene.v().loadNecessaryClasses()
  }

  private def configureSoot(): Unit = {
    // set application mode
    Options.v().set_app(true)
    Options.v().set_whole_program(true)
    // make sure classpath is configured correctly
    Options.v().set_soot_classpath(ProgramHandlingUtil.TEMP_DIR.toString)
    Options.v().set_prepend_classpath(true)
    // keep debugging info
    Options.v().set_keep_line_number(true)
    Options.v().set_keep_offset(true)
    // ignore library code
    Options.v().set_no_bodies_for_excluded(true)
    Options.v().set_allow_phantom_refs(true)
    // keep variable names
    PhaseOptions.v().setPhaseOption("jb", "use-original-names:true")
  }

  private def clean(): Unit = {
    G.reset()
    ProgramHandlingUtil.clean()
  }

}
