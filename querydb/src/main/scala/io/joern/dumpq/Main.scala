package io.joern.dumpq

import io.joern.console.{DefaultArgumentProvider, QueryDatabase}
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.semanticsloader.NoSemantics
import org.json4s.{Formats, NoTypeHints}
import org.json4s.native.Serialization

import java.nio.file.{Files, Paths}
import scala.util.Properties

object Main {

  def main(args: Array[String]): Unit = {
    dumpQueries()
  }

  def dumpQueries(): Unit = {
    implicit val engineContext: EngineContext = EngineContext(NoSemantics)
    implicit val formats: Formats             = Serialization.formats(NoTypeHints)
    val queryDb                               = new QueryDatabase(new JoernDefaultArgumentProvider(0))
    // TODO allow specifying file from the outside
    val outFileName = Paths.get(Properties.tmpDir, "querydb.json")
    Files.writeString(outFileName, Serialization.write(queryDb.allQueries))
    println(s"Queries written to: ${outFileName.toAbsolutePath.toString}")
  }

  class JoernDefaultArgumentProvider(maxCallDepth: Int)(implicit context: EngineContext)
      extends DefaultArgumentProvider {

    override def typeSpecificDefaultArg(argTypeFullName: String): Option[Any] = {
      if (argTypeFullName.endsWith("EngineContext")) {
        Some(context.copy(config = EngineConfig(maxCallDepth = maxCallDepth)))
      } else {
        None
      }
    }
  }

}
