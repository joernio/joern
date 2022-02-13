package io.joern.suites

import io.joern.console.DefaultArgumentProvider
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}

import java.nio.file.Paths

class QDBArgumentProvider(maxCallDepth: Int) extends DefaultArgumentProvider {
  private val semanticsFilename     = getClass.getClassLoader.getResource("default.semantics").toURI
  def testSemanticsFilename: String = Paths.get(semanticsFilename).toString

  override def typeSpecificDefaultArg(argTypeFullName: String): Option[Any] = {
    if (argTypeFullName.endsWith("EngineContext")) {
      val newsemantics  = Semantics.fromList(new Parser().parseFile(testSemanticsFilename))
      val engineContext = EngineContext(newsemantics)
      engineContext.config.maxCallDepth = maxCallDepth
      Some(engineContext)
    } else {
      None
    }
  }
}
