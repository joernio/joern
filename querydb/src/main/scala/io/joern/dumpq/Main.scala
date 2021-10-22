package io.joern.dumpq

import io.shiftleft.console.{DefaultArgumentProvider, QueryDatabase}
import io.shiftleft.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.dataflowengineoss.semanticsloader.Semantics
import org.json4s.{Formats, NoTypeHints}
import org.json4s.native.Serialization

import scala.reflect.runtime.universe._

object Main extends App {

  dumpQueries()

  def dumpQueries(): Unit = {
    implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
    implicit val formats: AnyRef with Formats =
      Serialization.formats(NoTypeHints)
    val queryDb = new QueryDatabase(new JoernDefaultArgumentProvider(0))
    // TODO allow specifying file from the outside and make this portable
    val outFileName = "/tmp/querydb.json"
    better.files
      .File(outFileName)
      .write(
        Serialization.write(queryDb.allQueries)
      )
    println(s"Queries written to: $outFileName")
  }

  class JoernDefaultArgumentProvider(maxCallDepth: Int)(
      implicit context: EngineContext)
      extends DefaultArgumentProvider {

    override def defaultArgument(method: MethodSymbol,
                                 im: InstanceMirror,
                                 x: Symbol,
                                 i: Int): Option[Any] = {
      if (x.typeSignature.toString.endsWith("EngineContext")) {
        Some(context.copy(config = EngineConfig(maxCallDepth = maxCallDepth)))
      } else {
        super.defaultArgument(method, im, x, i)
      }
    }
  }

}
