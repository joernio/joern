package io.joern.kotlin2cpg.files

import io.joern.kotlin2cpg.utils.PathUtils
import io.joern.x2cpg.SourceFiles

import org.slf4j.LoggerFactory

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
    val extensionsToFilterFor = List()
    val hasUnwantedExt = {
      extensionsToFilterFor.exists { ext =>
        fileName.endsWith(ext)
      }
    }

    val isAndroidLayoutXml =
      fileName.endsWith("xml") && (fileName.contains("drawable") || fileName.contains("layout"))
    val containsSrcTest    = fileName.contains("src/test")
    val isSettingsXml      = fileName.endsWith("strings.xml") // some projects contain many i18n files
    val containsBenchmarks = fileName.contains("benchmarks")
    val isBuildGradle      = fileName.endsWith("build.gradle") || fileName.endsWith("build.gradle.kts")

    (containsUnwantedSubstring && !isBuildGradle) ||
    hasUnwantedExt ||
    isSettingsXml ||
    containsSrcTest ||
    isAndroidLayoutXml ||
    containsBenchmarks
  }

  protected def isConfigFile(fileName: String): Boolean = {
    isXmlFile(fileName) || isGradleFile(fileName) || isKotlinScript(fileName)
  }

  protected def isXmlFile(fileName: String): Boolean = {
    val xmlFileExtensions = Seq(".xml")
    xmlFileExtensions.exists(fileName.endsWith)
  }

  protected def isGradleFile(fileName: String): Boolean = {
    val gradleRelatedFiles = Seq("build.gradle", "settings.gradle", "gradle.properties", "build.gradle.kts")
    gradleRelatedFiles.exists(fileName.endsWith)
  }

  protected def isKotlinScript(fileName: String): Boolean = {
    val ktsExtensions = Seq(".kts")
    ktsExtensions.exists(fileName.endsWith)
  }

  def configFiles(forDir: String): Seq[String] = {
    val sourceFileExtensions = Set(".xml", ".gradle", ".properties", ".kts")
    val sourceFileNames      = SourceFiles.determine(forDir, sourceFileExtensions)
    sourceFileNames
      .filter(isConfigFile)
      .filterNot { fileName =>
        // TODO: add test for this type of filtering
        // TODO: support Windows paths
        val relativized = PathUtils.relativize(forDir, fileName)
        val willFilter  = SourceFilesPicker.shouldFilter(relativized)
        if (willFilter) {
          logger.debug(s"Filtered file at `$fileName`.")
        }
        willFilter
      }
  }
}

class SourceFilesPicker {}
