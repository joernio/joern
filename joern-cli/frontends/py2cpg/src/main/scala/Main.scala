import io.shiftleft.pythonparser.PyParser

import java.io.FileInputStream
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

object Main extends App {
  val parser = new PyParser()

  val inputDirOrFile = args(0)

  val files = Files.walk(Paths.get(inputDirOrFile)).collect(Collectors.toList[Path]).asScala

  files.foreach { file =>
    if (!Files.isDirectory(file) && file.toString.endsWith("py")) {
      println(s"Processing: $file")
      val module = parser.parse(new FileInputStream(file.toString))
      val errors = parser.errors
      if (errors.nonEmpty) {
        errors.foreach(println)
        System.exit(1)
      }
    }
  }

}
