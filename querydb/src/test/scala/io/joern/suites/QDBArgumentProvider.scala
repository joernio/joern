package io.joern.suites

import io.joern.console.DefaultArgumentProvider
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{FullNameSemanticsParser, Semantics}

import java.nio.file.Paths

class QDBArgumentProvider(maxCallDepth: Int) extends DefaultArgumentProvider {

  override def typeSpecificDefaultArg(argTypeFullName: String): Option[Any] = {
    if (argTypeFullName.endsWith("EngineContext")) {
      val engineContext = EngineContext()
      engineContext.config.maxCallDepth = maxCallDepth
      Some(engineContext)
    } else {
      None
    }
  }
}
