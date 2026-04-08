package io.joern.javasrc2cpg.typesolvers

import javassist.ClassPath
import javassist.bytecode.ClassFile
import org.slf4j.LoggerFactory

import java.io.{DataInputStream, InputStream}
import java.net.{URI, URL}
import java.nio.file.Paths
import java.util.jar.{JarEntry, JarFile}
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

object BytecodeIndexedClassPath {

  /** Read the declared class name from an open `.class` file stream using javassist. */
  def readClassNameFrom(inputStream: InputStream): String =
    new ClassFile(new DataInputStream(inputStream)).getName
}

/** A ClassPath implementation that resolves classes by their actual package declaration in bytecode rather than by
  * their path within the archive. This handles non-standard archive structures (e.g., fat JARs, repackaged JARs, JMODs)
  * where the entry path may not match the class's declared package.
  */
class BytecodeIndexedClassPath(archivePath: String) extends ClassPath {

  private val logger     = LoggerFactory.getLogger(this.getClass)
  private val jarFile    = new JarFile(archivePath)
  private val jarFileURL = Paths.get(archivePath).toUri.toURL.toString
  private val urlScheme  = if (archivePath.endsWith(".jmod")) "jmod" else "jar"

  private val classNameToEntry: Map[String, JarEntry] = buildIndex()

  val knownClassNames: Set[String] = classNameToEntry.keySet

  private def buildIndex(): Map[String, JarEntry] = {
    jarFile
      .entries()
      .asScala
      .filter(entry => !entry.isDirectory && entry.getName.endsWith(".class"))
      .flatMap { entry =>
        readClassName(entry) match {
          case Some(className) => Some(className -> entry)
          case None =>
            logger.debug(s"Could not read class name from entry ${entry.getName} in $archivePath")
            None
        }
      }
      .toMap
  }

  private def readClassName(entry: JarEntry): Option[String] =
    Try(Using.resource(jarFile.getInputStream(entry))(BytecodeIndexedClassPath.readClassNameFrom)).toOption

  override def find(classname: String): URL = {
    classNameToEntry
      .get(classname)
      .flatMap { entry =>
        Try(new URI(s"$urlScheme:$jarFileURL!/${entry.getName}").toURL).toOption
      }
      .orNull
  }

  override def openClassfile(classname: String): InputStream = {
    classNameToEntry.get(classname).map(jarFile.getInputStream).orNull
  }
}
