import java.io.{BufferedReader, File, InputStreamReader}
import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object ExternalCommandUtil {
  case class ExternalCommandOutput(exitCode: Int, stdOut: Seq[String], stdErr: Seq[String]) {
    def toTry: Try[ExternalCommandOutput] = {
      exitCode match {
        case 0 => Success(this)
        case _ => Failure(new RuntimeException(stdErr.mkString("\n")))
      }
    }
  }

  def run(cmd: String, cwd: Option[File] = None, isShellCmd: Boolean = true): ExternalCommandOutput = run(cmd.split(" ").toIndexedSeq, cwd, isShellCmd)

  def run(cmd: Seq[String], cwd: Option[File], isShellCmd: Boolean): ExternalCommandOutput = {
    val externalCmd = if (isShellCmd) {
      Seq("sh", "-c") ++ cmd
    } else {
      cmd
    }

    val processBuilder = cwd match {
     case Some(directory) => new ProcessBuilder(externalCmd*).directory(directory).redirectErrorStream(true)
     case None => new ProcessBuilder(externalCmd*).redirectErrorStream(true)
    }

    val process = processBuilder.start();

    val out = scala.collection.mutable.ListBuffer[String]()
    val err = scala.collection.mutable.ListBuffer[String]()

    val stdOutReader = new BufferedReader(new InputStreamReader(process.getInputStream))
    val stdErrReader = new BufferedReader(new InputStreamReader(process.getInputStream))

    var line: String = null

    while ({
      line = stdOutReader.readLine();
      line != null
    }) out += line
    while ({
      line = stdErrReader.readLine();
      line != null
    }) err += line

    val exitCode = process.waitFor();

    stdOutReader.close()
    stdErrReader.close()

    if (err.isEmpty) {
      ExternalCommandOutput(exitCode, out, out)
    } else {
      ExternalCommandOutput(exitCode, out, err)
    }
  }
}
