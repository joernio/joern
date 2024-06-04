package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.utils.Report
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.utils.IOUtils

import scala.util.matching.Regex

class PrivateKeyFilePass(cpg: Cpg, config: Config, report: Report = new Report())
    extends ConfigPass(cpg, config, report) {

  private val PrivateKeyRegex: Regex = """.*RSA\sPRIVATE\sKEY.*""".r

  override val allExtensions: Set[String]      = Set(".key")
  override val selectedExtensions: Set[String] = Set(".key")

  override def fileContent(file: File): Seq[String] =
    Seq("Content omitted for security reasons.")

  override def generateParts(): Array[File] =
    configFiles(config, selectedExtensions).toArray.filter(p =>
      IOUtils.readLinesInFile(p.path).exists(PrivateKeyRegex.matches)
    )

}
