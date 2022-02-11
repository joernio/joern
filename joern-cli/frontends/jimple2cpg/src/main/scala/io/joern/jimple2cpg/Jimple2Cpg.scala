package io.joern.jimple2cpg

import io.joern.jimple2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.shiftleft.x2cpg.SourceFiles
import io.shiftleft.x2cpg.X2Cpg.newEmptyCpg
import org.slf4j.LoggerFactory
import soot.options.Options
import soot.{G, PhaseOptions, Scene, SootClass}

import java.io.{File => JFile}
import java.nio.file.{Files, Paths}
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.{CollectionHasAsScala, EnumerationHasAsScala}
import scala.language.postfixOps
import scala.util.{Failure, Success, Using}

object Jimple2Cpg {
  val language = "JAVA"

  /** Formats the file name the way Soot refers to classes within a class path. e.g.
    * /unrelated/paths/class/path/Foo.class => class.path.Foo
    *
    * @param codePath
    *   the parent directory
    * @param filename
    *   the file name to transform.
    * @return
    *   the correctly formatted class path.
    */
  def getQualifiedClassPath(codePath: String, filename: String): String = {
    val pathFile = new JFile(codePath)
    val codeDir: String = if (pathFile.isDirectory) {
      pathFile.toPath.toAbsolutePath.normalize.toString
    } else {
      Paths.get(pathFile.getParentFile.getAbsolutePath).normalize.toString
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

      configureSoot(sourceCodeDir)
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

      // Load classes into Soot
      loadClassesIntoSoot(sourceCodeDir, sourceFileNames)
      // Project Soot classes
      val astCreator = new AstCreationPass(sourceCodeDir, sourceFileNames, cpg, methodKeyPool)
      astCreator.createAndApply()
      // Clear classes from Soot
      closeSoot()

      new TypeNodePass(astCreator.global.usedTypes.asScala.toList, cpg, Some(typesKeyPool))
        .createAndApply()

      cpg
    } finally {
      closeSoot()
    }
  }

  /** Retrieve parseable files from archive types.
    */
  private def extractSourceFilesFromArchive(sourceCodeDir: String, archiveFileExtensions: Set[String]): List[String] = {
    val archives = if (new JFile(sourceCodeDir).isFile) {
      List(sourceCodeDir)
    } else {
      SourceFiles.determine(Set(sourceCodeDir), archiveFileExtensions)
    }
    archives.flatMap { x =>
      unzipArchive(new ZipFile(x), sourceCodeDir) match {
        case Failure(e) =>
          throw new RuntimeException(s"Error extracting files from archive at $x", e)
        case Success(files) => files
      }
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
        SourceFiles.determine(Set(sourceCodePath), sourceFileExtensions)
    ).distinct
  }

  private def loadClassesIntoSoot(sourceCodePath: String, sourceFileNames: List[String]): Unit = {
    sourceFileNames
      .map { fName =>
        val cp = getQualifiedClassPath(sourceCodePath, fName)
        Scene.v().addBasicClass(cp, SootClass.BODIES)
        cp
      }
      .foreach(Scene.v().loadClassAndSupport(_).setApplicationClass())
    Scene.v().loadNecessaryClasses()
  }

  private def configureSoot(sourceCodePath: String): Unit = {
    // set application mode
    Options.v().set_app(true)
    Options.v().set_whole_program(true)
    // make sure classpath is configured correctly
    Options.v().set_soot_classpath(sourceCodePath)
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

  private def closeSoot(): Unit = G.reset()

  /** Unzips a ZIP file into a sequence of files. All files unpacked are deleted at the end of CPG construction.
    *
    * @param zf
    *   The ZIP file to extract.
    * @param sourceCodePath
    *   The project root path to unpack to.
    */
  private def unzipArchive(zf: ZipFile, sourceCodePath: String) = scala.util.Try {
    Using.resource(zf) { zip: ZipFile =>
      // Copy zipped files across
      zip
        .entries()
        .asScala
        .filter(f => !f.isDirectory && f.getName.contains(".class"))
        .flatMap { entry =>
          val sourceCodePathFile = new JFile(sourceCodePath)
          // Handle the case if the input source code path is an archive itself
          val destFile = if (sourceCodePathFile.isDirectory) {
            new JFile(sourceCodePath + JFile.separator + entry.getName)
          } else {
            new JFile(sourceCodePathFile.getParentFile.getAbsolutePath + JFile.separator + entry.getName)
          }
          // dirName accounts for nested directories as a result of JAR package structure
          val dirName = destFile.getAbsolutePath
            .substring(0, destFile.getAbsolutePath.lastIndexOf(JFile.separator))
          // Create directory path
          new JFile(dirName).mkdirs()
          try {
            if (destFile.exists()) destFile.delete()
            Using.resource(zip.getInputStream(entry)) { input =>
              Files.copy(input, destFile.toPath)
            }
            destFile.deleteOnExit()
            Option(destFile.getAbsolutePath)
          } catch {
            case e: Exception =>
              logger.warn(
                s"Encountered an error while extracting entry ${entry.getName} from archive ${zip.getName}.",
                e
              )
              Option.empty
          }
        }
        .toSeq
    }
  }

}
