package io.joern.kotlin2cpg

import io.joern.kotlin2cpg.files.SourceFilesPicker
import io.joern.kotlin2cpg.types.ErrorLoggingMessageCollector
import io.joern.kotlin2cpg.types.{CompilerAPI, DefaultNameGenerator, InferenceSourcesPicker}
import io.joern.kotlin2cpg.utils.PathUtils
import io.shiftleft.x2cpg.{IOUtils, X2Cpg, X2CpgConfig}

import java.nio.file.{Files, Paths}
import org.slf4j.LoggerFactory
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scopt.OParser

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
          InferenceJarPath("inferencejars/android-4.1.1.4.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.activity.activity-ktx-1.2.4.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.annotation-1.1.0.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.compose.foundation-1.0.5.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.compose.foundation-layout-1.0.5.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.constraintlayout-2.1.1.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.core-1.1.0.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.core-ktx-1.6.0.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.fragment-1.4.0.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.fragment-ktx-1.4.0.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.lifecycle-viewmodel-ktx-2.2.0.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.localbroadcastmanager-1.0.0.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.preference-ktx-1.1.1.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.viewpager.viewpager-1.0.0.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.recyclerview.recyclerview-1.0.0.jar", isResource = true),
          InferenceJarPath("inferencejars/androidx.webkit-1.4.0.jar", isResource = true),
          InferenceJarPath("inferencejars/annotation-1.1.0.jar", isResource = true),
          InferenceJarPath("inferencejars/appcompat-1.3.1-classes.jar", isResource = true),
          InferenceJarPath("inferencejars/com.appdynamics.appdynamics-runtime-20.7.1.jar", isResource = true),
          InferenceJarPath("inferencejars/com.chimerapps.niddler.niddler-1.5.5.jar", isResource = true),
          InferenceJarPath("inferencejars/com.chimerapps.niddler.niddler-noop-1.5.5.jar", isResource = true),
          InferenceJarPath("inferencejars/com.facebook.stetho.stetho-1.5.0.jar", isResource = true),
          InferenceJarPath("inferencejars/com.facebook.stetho.stetho-okhttp3-1.5.0.jar", isResource = true),
          InferenceJarPath("inferencejars/com.getkeepsafe.relinker.relinker-1.4.4.jar", isResource = true),
          InferenceJarPath("inferencejars/com.github.bumptech.glide.glide-4.9.0.jar", isResource = true),
          InferenceJarPath("inferencejars/com.android.support.webkit-28.0.0.jar", isResource = true),
          InferenceJarPath("inferencejars/com.google.android.material.material-1.4.0.jar", isResource = true),
          InferenceJarPath("inferencejars/com.google.dagger.dagger-2.38.1.jar", isResource = true),
          InferenceJarPath("inferencejars/com.google.dagger.dagger-android-2.38.1.jar", isResource = true),
          InferenceJarPath("inferencejars/com.google.dagger.dagger-android-support-2.38.1.jar", isResource = true),
          InferenceJarPath("inferencejars/com.google.firebase.firebase-crashlytics-17.2.1.jar", isResource = true),
          InferenceJarPath("inferencejars/com.google.guava.guava-27.1-android.jar", isResource = true),
          InferenceJarPath("inferencejars/com.jakewharton.timber.timber-4.5.1.jar", isResource = true),
          InferenceJarPath("inferencejars/commons-io.commons-io-2.6.jar", isResource = true),
          InferenceJarPath("inferencejars/com.squareup.okhttp3.logging-interceptor-4.8.0.jar", isResource = true),
          InferenceJarPath("inferencejars/cryptonode.jncryptor-1.2.0.jar", isResource = true),
          InferenceJarPath("inferencejars/dagger-compiler-2.38.1.jar", isResource = true),
          InferenceJarPath("inferencejars/google.zixing.core-3.2.0.jar", isResource = true),
          InferenceJarPath("inferencejars/gson-2.8.9.jar", isResource = true),
          InferenceJarPath("inferencejars/http4k-core-4.14.1.4.jar", isResource = true),
          InferenceJarPath("inferencejars/io.reactivex.rxjava2.rxandroid-2.1.0.jar", isResource = true),
          InferenceJarPath("inferencejars/javax.servlet-api-4.0.1.jar", isResource = true),
          InferenceJarPath("inferencejars/javalin-4.1.1.jar", isResource = true),
          InferenceJarPath("inferencejars/jncryptor-1.2.0.jar", isResource = true),
          InferenceJarPath("inferencejars/kotlin-android-extensions-runtime-1.6.0-M1.jar", isResource = true),
          InferenceJarPath("inferencejars/kotlin-stdlib-1.6.0.jar", isResource = true),
          InferenceJarPath("inferencejars/kotlin-stdlib-common-1.6.0.jar", isResource = true),
          InferenceJarPath("inferencejars/kotlin-stdlib-jdk8-1.6.0.jar", isResource = true),
          InferenceJarPath("inferencejars/org.apache.commons.commons-collections4-4.4.jar", isResource = true),
          InferenceJarPath("inferencejars/org.apache.commons.commons-lang3-3.10.jar", isResource = true),
          InferenceJarPath("inferencejars/org.apache.commons.commons-text-1.8.jar", isResource = true),
          InferenceJarPath("inferencejars/org.jetbrains.kotlin.kotlin-android-extensions-1.6.0.jar", isResource = true),
          InferenceJarPath(
            "inferencejars/org.jetbrains.kotlinx.kotlinx-coroutines-android-1.3.9.jar",
            isResource = true
          ),
          InferenceJarPath("inferencejars/rxjava-2.1.0.jar", isResource = true)
        )
        val dirsForSourcesToCompile = InferenceSourcesPicker.dirsForRoot(sourceDir)
        val messageCollector        = new ErrorLoggingMessageCollector
        val environment =
          CompilerAPI.makeEnvironment(dirsForSourcesToCompile, inferenceJarsPaths, Seq(), messageCollector)
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
              KtFileWithMeta(fwp._1, fwp._2, fwp._1.getVirtualFilePath)
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
              FileContentAtPath(fileContents, fnm._2, fnm._1)
            }

        val nameGenerator = new DefaultNameGenerator(environment)
        val cpg = new Kt2Cpg().createCpg(filesWithMeta, fileContentsAtPath, nameGenerator, Some(config.outputPath))
        cpg.close()
      } else {
        println("This frontend requires exactly one input path")
        System.exit(1)
      }
    case None =>
      System.exit(1)
  }
}
