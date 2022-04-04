# Data Flow Engine

A taint-tracking system based on whole-program data-dependence 
representation. External library calls can be defined by 
semantic models (see `src/test/resources/default.semantics`)

Basic usage:

```scala
// If using Joern shell, imports and engine context will be pre-configured and available already
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language.toExtendedCfgNode

def sink = cpg.call.argument.code(".*malicious_input.*")
def source = cpg.call(".*println.*")

// Traverses data flow in the backwards direction
sink.reachableBy(source)
```

## Configuration

To begin using the data flow engine on the CPG, we need the following:

```scala
  // (1) Imports to extend CFG nodes
  import io.joern.dataflowengineoss.language.toExtendedCfgNode
  import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics, FlowSemantic}
  import io.joern.dataflowengineoss.queryengine.{EngineContext, EngineConfig}

  import scala.util.{Failure, Success, Try}
  import scala.io.{BufferedSource, Source}

  // (2) Parse in semantic models, if none given then external methods will be overtainted by default
  val semanticsParser = new Parser()
  val defaultSemantics: Try[BufferedSource] = Try(
    Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("default.semantics"))
  )
  val semantics: List[FlowSemantic] = defaultSemantics match {
    case Failure(_)         => List()
    case Success(semantics) => semanticsParser.parse(semantics.getLines().mkString("\n"))
  }

  // (3) Optional: Configure the engine
  val engineConfig = EngineConfig(maxCallDepth = 2, initialTable = None, disableCacheUse = false)

  // (4) Create execution context for the engine
  implicit var context: EngineContext = EngineContext(Semantics.fromList(semantics), engineConfig)
```