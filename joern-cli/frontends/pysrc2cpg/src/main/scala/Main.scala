import io.joern.pythonparser.PyParser

import java.io.FileInputStream
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

object Main extends App {
  val parser = new PyParser()

  val inputDirOrFile = args(0)

  val files = Files.walk(Paths.get(inputDirOrFile)).collect(Collectors.toList[Path]).asScala

  var filesProcessed = 0
  var timeAccu       = 0L
  files.foreach { file =>
    if (!Files.isDirectory(file) && file.toString.endsWith(".py")) {
      println(s"Processing: $file")
      val start  = System.currentTimeMillis()
      val module = parser.parse(new FileInputStream(file.toString))
      val stop   = System.currentTimeMillis()
      filesProcessed += 1
      timeAccu += (stop - start)
      val errors = parser.errors
      if (errors.nonEmpty) {
        errors.foreach(println)
        System.exit(1)
      }
    }
  }

  println(s"Files processed: $filesProcessed in $timeAccu msec")
  println(s"Mean time to process: ${timeAccu.asInstanceOf[Float] / filesProcessed}")

}
