package io.joern.pysrc2cpg

import org.rogach.scallop._

import java.nio.file.Paths

class ParsedArguments(arguments: Seq[String]) extends ScallopConf(arguments) {
  val output: ScallopOption[String] =
    opt[String](default = Some("out.cpg"), short = 'o', descr = "Output file name. Defaults to out.cpg")

  val input: ScallopOption[String] = trailArg[String](descr = "Input directory name")

  val venvDir: ScallopOption[String] =
    opt[String](default = Some(".venv"), noshort = true, descr = "Virtual environment directory. Defaults to .venv.")
  val ignoreVenvDir: ScallopOption[Boolean] = opt[Boolean](
    default = Some(true),
    noshort = true,
    descr = "Specifies whether venv-dir is ignored. Default to true."
  )
  verify()
}

object Main {
  def main(args: Array[String]) = {
    val parsedArguments = new ParsedArguments(args.toSeq)

    val ignoreVenvDir =
      if (parsedArguments.ignoreVenvDir.toOption.get) {
        parsedArguments.venvDir.toOption
      } else {
        None
      }

    val py2CpgConfig =
      Py2CpgOnFileSystemConfig(
        Paths.get(parsedArguments.output.toOption.get),
        Paths.get(parsedArguments.input.toOption.get),
        ignoreVenvDir.map(Paths.get(_))
      )

    val cpg = Py2CpgOnFileSystem.buildCpg(py2CpgConfig)
    cpg.close()
  }
}
