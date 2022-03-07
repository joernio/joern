package io.joern.kotlin2cpg.types

import io.joern.kotlin2cpg.types.ErrorLoggingMessageCollector
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CompilerAPIStabilityTests extends AnyFreeSpec with Matchers {
  "TypeInfoProvioder execution on codebase containing KotlinScript gradle file" - {
    "should return a non-null binding context" in {
      val sourceDir               = "src/test/resources/code/with_kotlin_script/"
      val dirsForSourcesToCompile = InferenceSourcesPicker.dirsForRoot(sourceDir)
      val environment =
        CompilerAPI.makeEnvironment(dirsForSourcesToCompile, Seq(), Seq(), new ErrorLoggingMessageCollector)
      environment.getSourceFiles should not be List()

      val nameGenerator = new DefaultNameGenerator(environment)
      nameGenerator.bindingContext should not be null
    }
  }
}
