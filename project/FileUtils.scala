import java.io.File
import java.nio.file.{Files, FileSystems}
import scala.collection.JavaConverters._

object FileUtils {

  def listFilesRecursively(roots: File*): Seq[File] = {
    roots.flatMap { root =>
      Files.walk(root.toPath).iterator.asScala.map(_.toFile).filter(!_.isDirectory)
    }
  }

  def deleteRecursively(root: File): Unit = {
    if (root.exists) {
      Files.walk(root.toPath).iterator.asScala.map(_.toFile).collect {
        case file if file.isDirectory => deleteRecursively(file)
        case file                     => file.delete()
      }
    }
  }

  /** checks if the given jar contains the given entry in the root directory
    */
  def jarContainsEntryInRoot(jar: File, entry: String): Boolean = {
    val zipFs = FileSystems.newFileSystem(jar.toPath)
    val result = zipFs.getRootDirectories.asScala.exists { zipRootDir =>
      Files.list(zipRootDir).iterator.asScala.exists(_.getFileName.toString == entry)
    }
    zipFs.close()
    result
  }

  /** removes the given entry from the given jar, only at root level */
  def removeJarEntryFromRoot(jar: File, entry: String): Unit = {
    val zipFs = FileSystems.newFileSystem(jar.toPath)
    zipFs.getRootDirectories.forEach { zipRootDir =>
      Files.list(zipRootDir).filter(_.getFileName.toString == entry).forEach(Files.delete(_))
    }
    zipFs.close()
  }


}
