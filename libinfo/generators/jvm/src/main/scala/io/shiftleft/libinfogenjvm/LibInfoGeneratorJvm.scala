package io.shiftleft.libinfogenjvm

import io.shiftleft.libinfo.LibInfoWriter
import org.objectweb.asm.ClassReader
import scopt.OParser

import java.io.OutputStream
import java.nio.file.{Files, Path}
import scala.util.Using

object LibInfoGeneratorJvm {
  def main(argv: Array[String]): Unit = {
    val options = parseCmdLine(argv).getOrElse(sys.exit(1))

    val outStream  = Files.newOutputStream(Path.of(options.outputFile))
    val classFiles = options.inputClassFiles.get.map(Path.of(_))

    convert(classFiles, outStream)
  }

  def convert(classFiles: collection.Seq[Path], libInfoOutStream: OutputStream): Unit = {
    Using.resource(LibInfoWriter(libInfoOutStream)) { writer =>
      classFiles.foreach { classFile =>
        Using.resource(Files.newInputStream(classFile)) { classFileInStream =>
          val reader  = new ClassReader(classFileInStream)
          val visitor = new ToLibInfoVisitor(writer)

          reader.accept(visitor, ClassReader.SKIP_CODE)
        }
      }
    }
  }

  case class CmdLineOptions(
    outputFile: String = "libInfo.ion",
    inputClassFiles: Option[Seq[String]] = None,
    inputJarFiles: Option[Seq[String]] = None
  )

  private def parseCmdLine(argv: Array[String]): Option[CmdLineOptions] = {
    val cmdLineParser: OParser[Unit, CmdLineOptions] = {
      val builder = OParser.builder[CmdLineOptions]
      import builder.programName
      import builder.opt
      OParser.sequence(
        programName(getClass.getSimpleName),
        opt[String]("output")
          .abbr("o")
          .text(s"Output ion file to store the library information in. Default ${CmdLineOptions().outputFile}")
          .action((file, c) => c.copy(outputFile = file)),
        opt[Seq[String]]("input-classes")
          .abbr("c")
          .optional()
          .text("input class files")
          .action((files, c) => c.copy(inputClassFiles = Some(files))),
        opt[Seq[String]]("input-jars")
          .abbr("j")
          .optional()
          .text("input jar files")
          .action((files, c) => c.copy(inputJarFiles = Some(files)))
      )
    }

    OParser.parse(cmdLineParser, argv, CmdLineOptions())
  }

}
