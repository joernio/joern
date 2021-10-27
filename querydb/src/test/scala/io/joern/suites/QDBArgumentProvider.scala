package io.joern.suites

import io.shiftleft.console.DefaultArgumentProvider
import io.shiftleft.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.dataflowengineoss.semanticsloader.{Parser, Semantics}

import scala.reflect.runtime.universe._

class QDBArgumentProvider(maxCallDepth: Int)  extends DefaultArgumentProvider {
  def testSemanticsFilename = "src/test/resources/default.semantics"

  override def defaultArgument(method: MethodSymbol, im: InstanceMirror, x: Symbol, i: Int): Option[Any] = {
    if (x.typeSignature.toString.endsWith("EngineContext")) {
      val newsemantics = Semantics.fromList(new Parser().parseFile(testSemanticsFilename))
      val engineContext = EngineContext(newsemantics)
      engineContext.config.maxCallDepth = maxCallDepth
      Some(engineContext)
    } else {
      super.defaultArgument(method, im, x, i)
    }
  }
}
