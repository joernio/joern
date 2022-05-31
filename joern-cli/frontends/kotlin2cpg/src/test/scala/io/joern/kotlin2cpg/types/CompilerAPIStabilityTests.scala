package io.joern.kotlin2cpg.types

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CompilerAPIStabilityTests extends AnyFreeSpec with Matchers {
  "TypeInfoProvioder execution on codebase containing KotlinScript gradle file" - {
    "should return a non-null binding context" in {
      val sourceDir               = "joern-cli/frontends/kotlin2cpg/src/test/resources/code/with_kotlin_script/"
      val dirsForSourcesToCompile = ContentSourcesPicker.dirsForRoot(sourceDir)
      val environment =
        CompilerAPI.makeEnvironment(dirsForSourcesToCompile, Seq(), Seq(), new ErrorLoggingMessageCollector)
      environment.getSourceFiles should not be List()

      val nameGenerator = new DefaultTypeInfoProvider(environment)
      nameGenerator.bindingContext should not be null
    }
  }
}
