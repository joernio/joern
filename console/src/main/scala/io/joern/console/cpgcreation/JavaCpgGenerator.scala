package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.shiftleft.semanticcpg.utils.ExternalCommand

import java.nio.file.Path
import scala.sys.process.*
import scala.util.{Failure, Try}

/** Language frontend for Java archives (JAR files). Translates Java archives into code property graphs.
  */
case class JavaCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {

    if (commercialAvailable) {
      generateCommercial(inputPath, outputPath)
    } else if (ossAvailable) {
      generateOss(inputPath, outputPath)
    } else {
      Failure(new AssertionError("No Java language frontend present"))
    }
  }

  private def generateCommercial(inputPath: String, outputPath: String): Try[String] = {
    if (inputPath.endsWith(".apk")) {
      println("found .apk ending - will first transform it to a jar using dex2jar.sh")

      val dex2jar = rootPath.resolve("dex2jar.sh").toString

      ExternalCommand.run(Seq(dex2jar, inputPath), isShellCommand = true)
      val jarPath = s"$inputPath.jar"
      generateCommercial(jarPath, outputPath)
    } else {
      var command = rootPath.resolve("java2cpg.sh").toString
      var arguments =
        Seq(inputPath, "-o", outputPath) ++ jvmLanguages ++ config.cmdLineParams
      if (System.getProperty("os.name").startsWith("Windows")) {
        command = "powershell"
        arguments = Seq(rootPath.resolve("java2cpg.ps1").toString) ++ arguments
      }
      runShellCommand(command, arguments).map(_ => outputPath)
    }
  }

  private def generateOss(inputPath: String, outputPath: String): Try[String] = {
    val command   = if (isWin) rootPath.resolve("jimple2cpg.bat") else rootPath.resolve("jimple2cpg")
    val arguments = config.cmdLineParams.toSeq ++ Seq(inputPath, "--output", outputPath)
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  private def jvmLanguages: List[String] = {
    if (JavaCpgGenerator.experimentalLanguages.nonEmpty) {
      List("--experimental-langs", JavaCpgGenerator.experimentalLanguages.mkString(","))
    } else Nil
  }

  override def isAvailable: Boolean = {
    commercialAvailable || ossAvailable
  }

  private def commercialAvailable: Boolean = rootPath.resolve("java2cpg.sh").toFile.exists()
  private def ossAvailable: Boolean        = rootPath.resolve("jimple2cpg").toFile.exists()

  override def isJvmBased = true
}

object JavaCpgGenerator {
  private final val experimentalLanguages: List[String] = List("scala")
}
