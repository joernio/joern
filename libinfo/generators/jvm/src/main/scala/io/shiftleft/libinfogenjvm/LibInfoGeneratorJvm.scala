package io.shiftleft.libinfogenjvm

import io.shiftleft.libinfo.LibInfoWriter
import org.objectweb.asm.ClassReader
import scopt.OParser

import java.io.{InputStream, OutputStream}
import java.nio.file.{Files, Path}
import java.util.jar.JarFile
import scala.util.Using
import scala.jdk.CollectionConverters.*

object LibInfoGeneratorJvm {
  def main(argv: Array[String]): Unit = {
    val options = parseCmdLine(argv).getOrElse(sys.exit(1))

    val outStream  = Files.newOutputStream(Path.of(options.outputFile))
    val classFiles = options.inputClassFiles.getOrElse(Nil)
    val jarFiles   = options.inputJarFiles.getOrElse(Nil)

    convert(classFiles, jarFiles, outStream)
  }

  def convert(
    classFiles: collection.Seq[String],
    jarFiles: collection.Seq[String],
    libInfoOutStream: OutputStream
  ): Unit = {
    Using.resource(LibInfoWriter(libInfoOutStream)) { writer =>
      classFiles.foreach { classFile =>
        println(s"Handling $classFile")
        Using.resource(Files.newInputStream(Path.of(classFile))) { classFileInStream =>
          convertInputStream(writer, classFileInStream)
        }
      }

      jarFiles.foreach { jarFile =>
        Using.resource(JarFile(jarFile)) { jar =>
          jar.entries().asIterator().asScala.foreach { entry =>
            if (entry.getName.endsWith(".class")) {
              println(s"Handling ${entry.getName}")
              Using.resource(jar.getInputStream(entry)) { classFileInStream =>
                convertInputStream(writer, classFileInStream)
              }
            }
          }
        }
      }
    }
  }

  private def convertInputStream(writer: LibInfoWriter, classFileInStream: InputStream): Unit = {
    val reader  = new ClassReader(classFileInStream)
    val visitor = new ToLibInfoVisitor(writer)

    reader.accept(visitor, ClassReader.SKIP_CODE)
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
      import builder.checkConfig
      import builder.failure
      import builder.success
      OParser.sequence(
        programName(getClass.getSimpleName),
        opt[String]("output")
          .abbr("o")
          .text(s"Output ion file to store the library information in. Default: '${CmdLineOptions().outputFile}''")
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
          .action((files, c) => c.copy(inputJarFiles = Some(files))),
        checkConfig { c =>
          if (c.inputClassFiles.isEmpty && c.inputJarFiles.isEmpty) {
            failure("At least one input class or jar file needs to be given.")
          } else {
            success
          }
        }
      )
    }

    OParser.parse(cmdLineParser, argv, CmdLineOptions())
  }

}
