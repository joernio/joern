package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.dataflowengineoss.slicing.SliceBasedTypeInferencePass
import io.joern.jssrc2cpg.{Config, Frontend, JsSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.Cpg

import java.nio.file.Path
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import scala.util.Try

case class JsSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("jssrc2cpg.bat") else rootPath.resolve("jssrc2cpg.sh")
  private var jsConfig: Option[Config] = None

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    val arguments = Seq(inputPath, "--output", outputPath) ++ config.cmdLineParams
    jsConfig = X2Cpg.parseCommandLine(arguments.toArray, Frontend.cmdLineParser, Config())
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  private def nanoToMinutesAndSeconds(nanoTime: Long): (Long, Long) = {
    val min = TimeUnit.MINUTES.convert(nanoTime, TimeUnit.NANOSECONDS)
    val sec =
      TimeUnit.SECONDS.convert(nanoTime - TimeUnit.NANOSECONDS.convert(min, TimeUnit.MINUTES), TimeUnit.NANOSECONDS)
    (min, sec)
  }

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    val postProcessingStart = new AtomicLong(0)
    val slicingStart        = new AtomicLong(0)
    JsSrc2Cpg.postProcessingPasses(cpg, jsConfig).foreach {
      case x: SliceBasedTypeInferencePass =>
        val start = System.nanoTime()
        x.createAndApply()
        slicingStart.addAndGet(System.nanoTime() - start)
      case x =>
        val start = System.nanoTime()
        x.createAndApply()
        postProcessingStart.addAndGet(System.nanoTime() - start)
    }

    val (postMinutes, postSeconds)   = nanoToMinutesAndSeconds(postProcessingStart.get())
    val (sliceMinutes, sliceSeconds) = nanoToMinutesAndSeconds(slicingStart.get())

    println(s"Post-processing passes (excl. JoernTI slicing) ${postMinutes}m and ${postSeconds}s")
    println(s"JoernTI slicing & type inference ran for ${sliceMinutes}m and ${sliceSeconds}s")
    cpg
  }

  override def isJvmBased = true
}
