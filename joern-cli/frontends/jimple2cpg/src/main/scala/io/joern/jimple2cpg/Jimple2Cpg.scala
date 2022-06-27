package io.joern.jimple2cpg

import io.joern.jimple2cpg.passes.AstCreationPass
import io.joern.jimple2cpg.util.ProgramHandlingUtil
import io.joern.jimple2cpg.util.ProgramHandlingUtil.{extractSourceFilesFromArchive, moveClassFiles}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory
import soot.options.Options
import soot.{G, PhaseOptions, Scene}

import java.io.{File => JFile}
import java.nio.file.Paths
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.language.postfixOps
import scala.util.Try

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
    val codePath = ProgramHandlingUtil.getUnpackingDir

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

  def apply(): Jimple2Cpg = new Jimple2Cpg()
}

class Jimple2Cpg extends X2CpgFrontend[Config] {

  import Jimple2Cpg._

  private val logger = LoggerFactory.getLogger(classOf[Jimple2Cpg])

  def createCpg(config: Config): Try[Cpg] = {
    val ret = withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      val rawSourceCodeFile = new JFile(config.inputPath)
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
      new MetaDataPass(cpg, language, config.inputPath).createAndApply()

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
      val astCreator = new AstCreationPass(sourceFileNames, cpg)
      astCreator.createAndApply()
      // Clear classes from Soot
      G.reset()

      new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg)
        .createAndApply()
    }
    clean()
    ret
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
        moveClassFiles(SourceFiles.determine(sourceCodePath, sourceFileExtensions))
    ).distinct
  }

  private def loadClassesIntoSoot(sourceFileNames: List[String]): Unit = {
    sourceFileNames
      .map(getQualifiedClassPath)
      .foreach { cp =>
        Scene.v().addBasicClass(cp)
        Scene.v().loadClassAndSupport(cp)
      }
    Scene.v().loadNecessaryClasses()
  }

  private def configureSoot(): Unit = {
    // set application mode
    Options.v().set_app(false)
    Options.v().set_whole_program(false)
    // make sure classpath is configured correctly
    Options.v().set_soot_classpath(ProgramHandlingUtil.getUnpackingDir.toString)
    Options.v().set_prepend_classpath(true)
    // keep debugging info
    Options.v().set_keep_line_number(true)
    Options.v().set_keep_offset(true)
    // ignore library code
    Options.v().set_no_bodies_for_excluded(true)
    Options.v().set_allow_phantom_refs(true)
    // keep variable names
    Options.v.setPhaseOption("jb.sils", "enabled:false")
    PhaseOptions.v().setPhaseOption("jb", "use-original-names:true")
  }

  private def clean(): Unit = {
    G.reset()
    ProgramHandlingUtil.clean()
  }

}
