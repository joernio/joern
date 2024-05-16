package io.joern.kotlin2cpg.files

import io.joern.x2cpg.SourceFiles

import org.slf4j.LoggerFactory

object SourceFilesPicker {
  private val logger = LoggerFactory.getLogger(getClass)

  private val substringsToFilterFor = List(
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

  def shouldFilter(fileName: String): Boolean = {
    val containsUnwantedSubstring = substringsToFilterFor.exists(fileName.contains)

    val isAndroidLayoutXml = fileName.endsWith("xml") && (fileName.contains("drawable") || fileName.contains("layout"))
    val containsSrcTest    = fileName.contains("src/test")
    val isSettingsXml      = fileName.endsWith("strings.xml") // some projects contain many i18n files
    val containsBenchmarks = fileName.contains("benchmarks")
    val isBuildGradle      = fileName.endsWith("build.gradle") || fileName.endsWith("build.gradle.kts")

    (containsUnwantedSubstring && !isBuildGradle) ||
    isSettingsXml ||
    containsSrcTest ||
    isAndroidLayoutXml ||
    containsBenchmarks
  }

  private def isConfigFile(fileName: String): Boolean = {
    isXmlFile(fileName) || isGradleFile(fileName) || isKotlinScript(fileName)
  }

  private def isXmlFile(fileName: String): Boolean = {
    val xmlFileExtensions = Seq(".xml")
    xmlFileExtensions.exists(fileName.endsWith)
  }

  private def isGradleFile(fileName: String): Boolean = {
    val gradleRelatedFiles = Seq("build.gradle", "settings.gradle", "gradle.properties", "build.gradle.kts")
    gradleRelatedFiles.exists(fileName.endsWith)
  }

  private def isKotlinScript(fileName: String): Boolean = {
    val ktsExtensions = Seq(".kts")
    ktsExtensions.exists(fileName.endsWith)
  }

  def configFiles(forDir: String): Seq[String] = {
    val sourceFileExtensions = Set(".xml", ".gradle", ".properties", ".kts")
    val sourceFileNames      = SourceFiles.determine(forDir, sourceFileExtensions)
    sourceFileNames
      .filter(isConfigFile)
      .filterNot { fileName =>
        val relativized = SourceFiles.toRelativePath(fileName, forDir)
        val willFilter  = SourceFilesPicker.shouldFilter(relativized)
        if (willFilter) {
          logger.debug(s"Filtered file at `$fileName`.")
        }
        willFilter
      }
  }
}
