package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.utils.Report
import io.joern.jssrc2cpg.Config
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.utils.IOUtils

import scala.util.matching.Regex

class PrivateKeyFilePass(cpg: Cpg, files: Seq[File], config: Config, report: Report = new Report())
    extends ConfigPass(cpg, files, config, report) {

  private val PRIVATE_KEY: Regex = """.*RSA\sPRIVATE\sKEY.*""".r

  override def fileContent(file: File): Seq[String] =
    Seq("Content omitted for security reasons.")

  override def generateParts(): Array[File] =
    super.generateParts().filter(p => IOUtils.readLinesInFile(p.path).exists(PRIVATE_KEY.matches))

}
