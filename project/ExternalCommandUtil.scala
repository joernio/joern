import java.io.{BufferedReader, File, InputStreamReader}
import scala.language.implicitConversions

object ExternalCommandUtil {
  case class ExternalCommandOutput(exitCode: Int, stdOut: Seq[String]) {}

  def run(cmd: Seq[String], cwd: Option[File] = None, isShellCmd: Boolean = true): Option[ExternalCommandOutput] = {
    val shellExeCommand = if (scala.util.Properties.isWin) {
      Seq("cmd.exe", "/C")
    } else {
      Seq("sh", "-c")
    }

    val externalCmd = if (isShellCmd) {
      shellExeCommand ++ cmd
    } else {
      cmd
    }

    val processBuilder = cwd match {
     case Some(directory) => new ProcessBuilder(externalCmd*).directory(directory).redirectErrorStream(true)
     case None => new ProcessBuilder(externalCmd*).redirectErrorStream(true)
    }

    val process = processBuilder.start();

    val out = scala.collection.mutable.ListBuffer[String]()

    val stdOutReader = new BufferedReader(new InputStreamReader(process.getInputStream))

    var line: String = null

    while ({
      line = stdOutReader.readLine();
      line != null
    }) out += line

    val exitCode = process.waitFor();

    stdOutReader.close()

    if (exitCode == 0) {
      Some(ExternalCommandOutput(exitCode, out))
    } else {
      None
    }
  }
}
