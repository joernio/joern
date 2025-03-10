package io.shiftleft.libinfogen.jvm

import io.shiftleft.libinfo.LibInfoWriter
import org.objectweb.asm.ClassReader

import java.io.{InputStream, OutputStream}
import java.nio.file.{Files, Path}
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*
import scala.util.Using

class LibInfoGenJvm {
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
}
