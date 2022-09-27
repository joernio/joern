/** originally from amm/util/src/main/scala/ammonite/util/Classpath.scala */
package ammonite.foo

import io.github.retronym.java9rtexport.Export
import java.io.File
import java.net.URL
import java.nio.file.{Path, Paths}
import java.util.zip.{ZipFile, ZipInputStream}
import scala.util.control.NonFatal

object Classpath {
  val traceClasspathIssues =
    sys.props
       .get("ammonite.trace-classpath")
       .exists(_.toLowerCase == "true")

  /**
   * In memory cache of all the jars used in the compiler. This takes up some
   * memory but is better than reaching all over the filesystem every time we
   * want to do something.
   */
  def classpath(
    classLoader: ClassLoader,
    rtCacheDir: Option[Path]
  ): Vector[URL] = {
    lazy val actualRTCacheDir = rtCacheDir.filter { dir =>
      // no need to cache if the storage is in tmpdir
      // because it is temporary
      !dir.startsWith(Paths.get(System.getProperty("java.io.tmpdir")))
    }

    var current = classLoader
    val files = collection.mutable.Buffer.empty[java.net.URL]
    val seenClassLoaders = collection.mutable.Buffer.empty[ClassLoader]
    while(current != null){
      seenClassLoaders.append(current)
      current match{
        case t: java.net.URLClassLoader =>
          files.appendAll(t.getURLs)
        case _ =>
      }
      current = current.getParent
    }


    val sunBoot = System.getProperty("sun.boot.class.path")
    if (sunBoot != null) {
      files.appendAll(
        sunBoot
          .split(java.io.File.pathSeparator)
          .map(new java.io.File(_))
          .filter(_.exists())
          .map(_.toURI.toURL)
      )
    } else {
      if (seenClassLoaders.contains(ClassLoader.getSystemClassLoader)) {
        for (p <- System.getProperty("java.class.path")
          .split(File.pathSeparatorChar) if !p.endsWith("sbt-launch.jar")) {
          val f = new File(p)
          if (f.exists())
            files.append(f.toURI.toURL)
        }
        try {
          new java.net.URLClassLoader(files.map(_.toURI.toURL).toArray, null)
            .loadClass("javax.script.ScriptEngineManager")
        } catch {
          case _: ClassNotFoundException =>
            actualRTCacheDir match {
              case Some(path) => files.append(Export.rtAt(path.toFile).toURI.toURL)
              case _ => files.append(Export.rt().toURI.toURL)
            }
        }
      }
    }
    files.toVector
  }

  def canBeOpenedAsJar(url: URL): Boolean = {
    var zis: ZipInputStream = null
    try {
      if (url.getProtocol == "file") {
        // this ignores any preamble in particular, unlike ZipInputStream
        val zf = new ZipFile(new File(url.toURI))
        zf.close()
        true
      } else {
        zis = new ZipInputStream(url.openStream())
        zis.getNextEntry != null
      }
    } catch {
      case NonFatal(e) =>
        traceClasspathProblem(
          s"Classpath element '$url' "+
            s"could not be opened as jar file because of $e"
        )
        false
    } finally {
      if (zis != null)
        zis.close()
    }
  }
  def traceClasspathProblem(msg: String): Unit =
    if (traceClasspathIssues) println(msg)
}
