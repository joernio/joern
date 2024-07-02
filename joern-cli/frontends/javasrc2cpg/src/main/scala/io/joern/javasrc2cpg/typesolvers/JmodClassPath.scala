package io.joern.javasrc2cpg.typesolvers

import better.files.File
import io.joern.javasrc2cpg.typesolvers.JmodClassPath.*
import javassist.ClassPath

import scala.jdk.CollectionConverters.*
import scala.util.Try
import java.io.InputStream
import java.net.URL
import java.util.jar.{JarEntry, JarFile}

class JmodClassPath(jmodPath: String) extends ClassPath {
  private val jarfile    = new JarFile(jmodPath)
  private val jarfileURL = File(jmodPath).url.toString
  private val entries    = getEntriesMap(jarfile)

  private def entryToClassName(entry: JarEntry): String = {
    entry.getName.stripPrefix(JmodClassesPrefix).stripSuffix(".class").replace('/', '.')
  }

  private def getEntriesMap(jarfile: JarFile): Map[String, JarEntry] = {
    jarfile
      .entries()
      .asScala
      .filter(_.getName.startsWith(JmodClassesPrefix))
      .filter(_.getName.endsWith(".class"))
      .map { entry => entryToClassName(entry) -> entry }
      .toMap
  }

  override def find(classname: String): URL = {
    val jarname = classname.replace('.', '/') + ".class"

    if (entries.contains(classname)) {
      Try(new URL(s"jmod:${jarfileURL}!/${jarname}")).getOrElse(null)
    } else { null }
  }

  override def openClassfile(classname: String): InputStream = {
    entries.get(classname) match {
      case None => null

      case Some(entry) => jarfile.getInputStream(entry)
    }
  }
}

object JmodClassPath {
  val JmodClassesPrefix: String = "classes/"
}
