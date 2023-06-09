package io.joern.jimple2cpg

import io.joern.jimple2cpg.passes.SootAstCreationPass
import io.joern.jimple2cpg.passes.AstCreationPass
import io.joern.jimple2cpg.util.ProgramHandlingUtil
import io.joern.jimple2cpg.util.ProgramHandlingUtil.{extractSourceFilesFromArchive, moveClassFiles}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory
import soot.options.Options
import soot.{G, PackManager, Scene}

import java.io.{File => JFile}
import java.nio.file.Paths
import org.apache.commons.io.FileUtils
import scala.jdk.CollectionConverters.{EnumerationHasAsScala, SeqHasAsJava}
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

  def sootLoadApk(input: String, framework: Option[String] = None): Unit = {
    Options.v().set_process_dir(List(input).asJava)
    framework match {
      case Some(value) if value.nonEmpty => {
        Options.v().set_src_prec(Options.src_prec_apk)
        Options.v().set_force_android_jar(value)
      }
      case _ => {
        Options.v().set_src_prec(Options.src_prec_apk_c_j)
      }
    }
    Options.v().set_process_multiple_dex(true)
    // workaround for Soot's bug while parsing large apk.
    // see: https://github.com/soot-oss/soot/issues/1256
    Options.v().setPhaseOption("jb", "use-original-names:false")
  }

  def sootLoadClass(inputDir: String): Unit = {
    Options.v().set_process_dir(List(inputDir).asJava)
    Options.v().set_src_prec(Options.src_prec_class)
  }

  def sootLoadSource(input: String, ext: String): Unit = {
    // Soot does not support loading single class/jimple file using path, so we move it to temp dir first
    // NOTE: Soot’s frontend for Java source files is outdated (only partially supports Java version up to 7) and not very robust.
    val src = new JFile(input)
    val dst = new JFile(ProgramHandlingUtil.getUnpackingDir.toString, src.getName)
    val prec = ext match {
      case "jimple" => Options.src_prec_jimple
      case _        => Options.src_prec_class
    }
    FileUtils.copyFile(src, dst)
    Options.v().set_process_dir(List(ProgramHandlingUtil.getUnpackingDir.toString).asJava)
    Options.v().set_src_prec(prec)
  }

  def createCpg(config: Config): Try[Cpg] = {
    val ret = withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      val inputPath = new JFile(config.inputPath)

      configureSoot()
      new MetaDataPass(cpg, language, config.inputPath).createAndApply()
      Options.v().set_dynamic_dir(config.dynamicDirs.toList.asJava)
      Options.v().set_dynamic_package(config.dynamicPkgs.toList.asJava)
      if (config.fullResolver) {
        // full transitive resolution of all references
        Options.v().set_full_resolver(true)
      }
      if (inputPath.isDirectory()) {
        // make sure classpath is configured correctly
        Options.v().set_soot_classpath(ProgramHandlingUtil.getUnpackingDir.toString)
        Options.v().set_prepend_classpath(true)
        val sourceFileExtensions  = Set(".class", ".jimple")
        val archiveFileExtensions = Set(".jar", ".war")
        // Load source files and unpack archives if necessary
        val sourceFileNames = loadSourceFiles(config.inputPath, sourceFileExtensions, archiveFileExtensions)
        logger.info(s"Loading ${sourceFileNames.size} program files")
        logger.debug(s"Source files are: $sourceFileNames")
        loadClassesIntoSoot(sourceFileNames)
        val astCreator = new AstCreationPass(sourceFileNames, cpg)
        astCreator.createAndApply()
        new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg)
          .createAndApply()
      } else {
        val ext = config.inputPath.split("\\.").lastOption.getOrElse("")
        ext match {
          case "jar" | "zip"      => sootLoadClass(config.inputPath)
          case "apk" | "dex"      => sootLoadApk(config.inputPath, config.android)
          case "jimple" | "class" => sootLoadSource(config.inputPath, ext)
          // case "war" => sootLoadClass(unpackPath/WEB-INF/classes)
          case _ => {
            logger.warn(s"Don't know how to handle input: $inputPath")
            throw new RuntimeException(s"Unsupported input at ${config.inputPath}")
          }
        }
        logger.info("Loading classes to soot")
        Scene.v().loadNecessaryClasses()
        logger.info(s"Loaded ${Scene.v().getApplicationClasses().size()} classes")
        val astCreator = new SootAstCreationPass(cpg)
        astCreator.createAndApply()
        new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg)
          .createAndApply()
      }

      // Clear classes from Soot
      G.reset()
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
    // keep debugging info
    Options.v().set_keep_line_number(true)
    Options.v().set_keep_offset(true)
    // ignore library code
    Options.v().set_no_bodies_for_excluded(true)
    Options.v().set_allow_phantom_refs(true)
    // keep variable names
    Options.v().setPhaseOption("jb.sils", "enabled:false")
    Options.v().setPhaseOption("jb", "use-original-names:true")
    // output jimple
    Options.v().set_output_format(Options.output_format_jimple)
    Options.v().set_output_dir(ProgramHandlingUtil.getUnpackingDir.toString)
  }

  private def clean(): Unit = {
    G.reset()
    ProgramHandlingUtil.clean()
  }

}
