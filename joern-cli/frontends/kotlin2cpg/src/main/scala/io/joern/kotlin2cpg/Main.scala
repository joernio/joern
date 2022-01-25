package io.joern.kotlin2cpg

import java.nio.file.{Files, Path, Paths}
import org.slf4j.LoggerFactory
import scopt.OParser

import scala.jdk.CollectionConverters.CollectionHasAsScala
import io.joern.kotlin2cpg.types.{CompilerAPI, DefaultNameGenerator, InferenceSourcesPicker}
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.x2cpg.{IOUtils, SourceFiles, X2Cpg, X2CpgConfig}

case class InferenceJarPath(path: String, isResource: Boolean)

/** Command line configuration parameters
  */
final case class Config(
    inputPaths: Set[String] = Set.empty, // TODO: make this a singular
    outputPath: String = X2CpgConfig.defaultOutputPath
) extends X2CpgConfig[Config] {

  override def withAdditionalInputPath(inputPath: String): Config =
    copy(inputPaths = inputPaths + inputPath)

  override def withOutputPath(x: String): Config = copy(outputPath = x)
}

object SourceFilesPicker {
  private val logger = LoggerFactory.getLogger(getClass)

  def shouldFilter(fileName: String): Boolean = {
    val substringsToFilterFor =
      List(
        ".idea",
        "target",
        "build",
        "integrationTest",
        "integrationtest",
        "androidTest",
        "sharedTest",
        "fixtures",
        "commonTest",
        "jvmTest",
        "test"
      )
    val containsUnwantedSubstring =
      substringsToFilterFor.exists { str =>
        fileName.contains(str)
      }
    val extensionsToFilterFor = List(".kts")
    val hasUnwantedExt = {
      extensionsToFilterFor.exists { ext =>
        fileName.endsWith(ext)
      }
    }

    val isAndroidLayoutXml =
      fileName.endsWith("xml") && (fileName.contains("drawable") || fileName.contains("layout"))
    val containsSrcTest = fileName.contains("src/test")
    val isSettingsXml = fileName.endsWith("strings.xml") // some projects contain many i18n files
    val containsBenchmarks = fileName.contains("benchmarks")
    containsUnwantedSubstring ||
    hasUnwantedExt ||
    isSettingsXml ||
    containsSrcTest ||
    isAndroidLayoutXml ||
    containsBenchmarks
  }

  protected def isConfigFile(fileName: String): Boolean = {
    isXmlFile(fileName) || isGradleFile(fileName)
  }

  protected def isXmlFile(fileName: String): Boolean = {
    val xmlFileExtensions = Seq(".xml")
    xmlFileExtensions.exists(fileName.endsWith)
  }

  protected def isGradleFile(fileName: String): Boolean = {
    val gradleRelatedFiles = Seq("build.gradle", "settings.gradle", "gradle.properties")
    gradleRelatedFiles.exists(fileName.endsWith)
  }

  def configFiles(forDir: String): Seq[String] = {
    val sourceFileExtensions = Set(".xml", ".gradle", ".properties")
    val sourceFileNames = SourceFiles.determine(Set(forDir), sourceFileExtensions)
    sourceFileNames
      .filter(isConfigFile(_))
      .filterNot { fileName =>
        // TODO: add test for this type of filtering
        // TODO: support Windows paths
        val relativized = PathUtils.relativize(forDir, fileName)
        val willFilter = SourceFilesPicker.shouldFilter(relativized)
        if (willFilter) {
          logger.debug("Filtered file at `" + fileName + "`.")
        }
        willFilter
      }
  }
}

object PathUtils {
  def relativize(sourceDir: String, filename: String) = {
    val pathAbsolute = Paths.get(filename).toAbsolutePath
    val pathBase = Paths.get(sourceDir).toAbsolutePath
    pathBase.relativize(pathAbsolute).toString
  }
}

/** Entry point for command line CPG creator
  */
object Main extends App {
  private val logger = LoggerFactory.getLogger(getClass)

  val parsingError = "KT2CPG_PARSING_ERROR"

  private val frontendSpecificOptions = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("kt2cpg"))
  }

  X2Cpg.parseCommandLine(args, frontendSpecificOptions, Config()) match {
    case Some(config) =>
      if (config.inputPaths.size == 1) {
        val sourceDir = config.inputPaths.head
        if (!Files.exists(Paths.get(sourceDir))) {
          println("The input path provided does not exist `" + sourceDir + "`. Exiting.")
          System.exit(1)
        }
        if (!Files.isDirectory(Paths.get(sourceDir))) {
          println("The input path provided is not a directory `" + sourceDir + "`. Exiting.")
          System.exit(1)
        }
        logger.info(s"Starting CPG generation for input directory `$sourceDir`.")

        // TODO: iterate over inferencejars dir and get the paths like so
        val inferenceJarsPaths = Seq(
          InferenceJarPath("inferencejars/android-4.1.1.4.jar", true),
          InferenceJarPath("inferencejars/androidx.activity.activity-ktx-1.2.4.jar", true),
          InferenceJarPath("inferencejars/androidx.annotation-1.1.0.jar", true),
          InferenceJarPath("inferencejars/androidx.compose.foundation-1.0.5.jar", true),
          InferenceJarPath("inferencejars/androidx.compose.foundation-layout-1.0.5.jar", true),
          InferenceJarPath("inferencejars/androidx.constraintlayout-2.1.1.jar", true),
          InferenceJarPath("inferencejars/androidx.core-1.1.0.jar", true),
          InferenceJarPath("inferencejars/androidx.core-ktx-1.6.0.jar", true),
          InferenceJarPath("inferencejars/androidx.fragment-1.4.0.jar", true),
          InferenceJarPath("inferencejars/androidx.fragment-ktx-1.4.0.jar", true),
          InferenceJarPath("inferencejars/androidx.lifecycle-viewmodel-ktx-2.2.0.jar", true),
          InferenceJarPath("inferencejars/androidx.localbroadcastmanager-1.0.0.jar", true),
          InferenceJarPath("inferencejars/androidx.preference-ktx-1.1.1.jar", true),
          InferenceJarPath("inferencejars/androidx.viewpager.viewpager-1.0.0.jar", true),
          InferenceJarPath("inferencejars/androidx.recyclerview.recyclerview-1.0.0.jar", true),
          InferenceJarPath("inferencejars/androidx.webkit-1.4.0.jar", true),
          InferenceJarPath("inferencejars/annotation-1.1.0.jar", true),
          InferenceJarPath("inferencejars/appcompat-1.3.1-classes.jar", true),
          InferenceJarPath("inferencejars/com.appdynamics.appdynamics-runtime-20.7.1.jar", true),
          InferenceJarPath("inferencejars/com.chimerapps.niddler.niddler-1.5.5.jar", true),
          InferenceJarPath("inferencejars/com.chimerapps.niddler.niddler-noop-1.5.5.jar", true),
          InferenceJarPath("inferencejars/com.facebook.stetho.stetho-1.5.0.jar", true),
          InferenceJarPath("inferencejars/com.facebook.stetho.stetho-okhttp3-1.5.0.jar", true),
          InferenceJarPath("inferencejars/com.getkeepsafe.relinker.relinker-1.4.4.jar", true),
          InferenceJarPath("inferencejars/com.github.bumptech.glide.glide-4.9.0.jar", true),
          InferenceJarPath("inferencejars/com.android.support.webkit-28.0.0.jar", true),
          InferenceJarPath("inferencejars/com.google.android.material.material-1.4.0.jar", true),
          InferenceJarPath("inferencejars/com.google.dagger.dagger-2.38.1.jar", true),
          InferenceJarPath("inferencejars/com.google.dagger.dagger-android-2.38.1.jar", true),
          InferenceJarPath(
            "inferencejars/com.google.dagger.dagger-android-support-2.38.1.jar",
            true
          ),
          InferenceJarPath(
            "inferencejars/com.google.firebase.firebase-crashlytics-17.2.1.jar",
            true
          ),
          InferenceJarPath(
            "inferencejars/com.google.guava.guava-27.1-android.jar",
            true
          ),
          InferenceJarPath("inferencejars/com.jakewharton.timber.timber-4.5.1.jar", true),
          InferenceJarPath("inferencejars/commons-io.commons-io-2.6.jar", true),
          InferenceJarPath(
            "inferencejars/com.squareup.okhttp3.logging-interceptor-4.8.0.jar",
            true
          ),
          InferenceJarPath("inferencejars/cryptonode.jncryptor-1.2.0.jar", true),
          InferenceJarPath("inferencejars/dagger-compiler-2.38.1.jar", true),
          InferenceJarPath("inferencejars/google.zixing.core-3.2.0.jar", true),
          InferenceJarPath("inferencejars/gson-2.8.9.jar", true),
          InferenceJarPath("inferencejars/http4k-core-4.14.1.4.jar", true),
          InferenceJarPath("inferencejars/io.reactivex.rxjava2.rxandroid-2.1.0.jar", true),
          InferenceJarPath("inferencejars/javax.servlet-api-4.0.1.jar", true),
          InferenceJarPath("inferencejars/javalin-4.1.1.jar", true),
          InferenceJarPath("inferencejars/jncryptor-1.2.0.jar", true),
          InferenceJarPath("inferencejars/kotlin-android-extensions-runtime-1.6.0-M1.jar", true),
          InferenceJarPath("inferencejars/kotlin-stdlib-1.6.0.jar", true),
          InferenceJarPath("inferencejars/kotlin-stdlib-common-1.6.0.jar", true),
          InferenceJarPath("inferencejars/kotlin-stdlib-jdk8-1.6.0.jar", true),
          InferenceJarPath("inferencejars/org.apache.commons.commons-collections4-4.4.jar", true),
          InferenceJarPath("inferencejars/org.apache.commons.commons-lang3-3.10.jar", true),
          InferenceJarPath("inferencejars/org.apache.commons.commons-text-1.8.jar", true),
          InferenceJarPath(
            "inferencejars/org.jetbrains.kotlin.kotlin-android-extensions-1.6.0.jar",
            true
          ),
          InferenceJarPath(
            "inferencejars/org.jetbrains.kotlinx.kotlinx-coroutines-android-1.3.9.jar",
            true
          ),
          InferenceJarPath("inferencejars/rxjava-2.1.0.jar", true)
        )
        val dirsForSourcesToCompile = InferenceSourcesPicker.dirsForRoot(sourceDir)
        val environment = CompilerAPI.makeEnvironment(dirsForSourcesToCompile, inferenceJarsPaths)
        val ktFiles = environment.getSourceFiles.asScala
        val filesWithMeta =
          ktFiles
            .flatMap { f =>
              try {
                val relPath = PathUtils.relativize(sourceDir, f.getVirtualFilePath)
                Some(f, relPath)
              } catch {
                case _: Throwable => None
              }
            }
            .map { fwp =>
              KtFileWithMeta(
                fwp._1,
                fwp._2,
                fwp._1.getVirtualFilePath
              )
            }
            .filterNot { fwp =>
              // TODO: add test for this type of filtering
              // TODO: support Windows paths
              val willFilter = SourceFilesPicker.shouldFilter(fwp.relativizedPath)
              if (willFilter) {
                logger.debug("Filtered file at `" + fwp.f.getVirtualFilePath + "`.")
              }
              willFilter
            }

        val fileContentsAtPath =
          SourceFilesPicker
            .configFiles(sourceDir)
            .flatMap { fileName =>
              try {
                val relPath = PathUtils.relativize(sourceDir, fileName)
                Some(fileName, relPath)
              } catch {
                case _: Throwable => None
              }
            }
            .map { fnm =>
              val fileContents =
                try {
                  IOUtils.readLinesInFile(Paths.get(fnm._1)).mkString("\n")
                } catch {
                  case t: Throwable => parsingError + "\n" + t.toString
                }
              FileContentAtPath(
                fileContents,
                fnm._2,
                fnm._1
              )
            }

        val nameGenerator = new DefaultNameGenerator(environment)
        val cpg = new Kt2Cpg().createCpg(
          filesWithMeta,
          fileContentsAtPath,
          nameGenerator,
          Some(config.outputPath)
        )
        cpg.close()
      } else {
        println("This frontend requires exactly one input path")
        System.exit(1)
      }
    case None =>
      System.exit(1)
  }
}
