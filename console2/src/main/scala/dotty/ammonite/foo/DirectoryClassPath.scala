package dotty.ammonite.foo

import java.io.{File => JFile}
import dotty.tools.dotc.classpath
import dotty.tools.io.{AbstractFile, PlainFile, ClassPath, ClassRepresentation, EfficientClassPath}
import classpath.FileUtils._

case class DirectoryClassPath(dir: JFile)
    extends classpath.JFileDirectoryLookup[classpath.ClassFileEntryImpl]
    with classpath.NoSourcePaths {
  override def findClass(className: String): Option[ClassRepresentation] =
    findClassFile(className) map classpath.ClassFileEntryImpl.apply

  def findClassFile(className: String): Option[AbstractFile] = {
    val relativePath = classpath.FileUtils.dirPath(className)
    val classFile = new JFile(dir, relativePath + ".class")
    if (classFile.exists) {
      val wrappedClassFile = new dotty.tools.io.File(classFile.toPath)
      val abstractClassFile = new PlainFile(wrappedClassFile)
      Some(abstractClassFile)
    }
    else None
  }

  protected def createFileEntry(file: AbstractFile): classpath.ClassFileEntryImpl =
    classpath.ClassFileEntryImpl(file)
  protected def isMatchingFile(f: JFile): Boolean =
    f.isClass

  private[dotty] def classes(inPackage: classpath.PackageName): Seq[classpath.ClassFileEntry] =
    files(inPackage)
}
