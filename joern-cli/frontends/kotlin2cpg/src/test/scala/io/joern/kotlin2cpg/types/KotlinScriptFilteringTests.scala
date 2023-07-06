package io.joern.kotlin2cpg.types

import io.joern.kotlin2cpg.compiler.{CompilerAPI, ErrorLoggingMessageCollector}
import org.jetbrains.kotlin.resolve.BindingContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Ignore

@Ignore // uncomment as soon as the sourceDir path is correct
class KotlinScriptFilteringTests extends AnyFreeSpec with Matchers {
  "Running `CompilerAPI.makeEnvironment` on external project with lots of KotlinScript sources" - {
    "should return an empty binding context" in {
      val sourceDir = "src/test/resources/external_projects/kotlin-dsl"
      val environment =
        CompilerAPI.makeEnvironment(Seq(sourceDir), Seq(), Seq(), new ErrorLoggingMessageCollector)
      environment.getSourceFiles should not be List()

      val nameGenerator = new DefaultTypeInfoProvider(environment)
      nameGenerator.bindingContext should not be null
      nameGenerator.bindingContext shouldBe BindingContext.EMPTY
    }

    "should not return an empty binding context" in {
      val sourceDir               = "src/test/resources/external_projects/kotlin-dsl"
      val dirsForSourcesToCompile = ContentSourcesPicker.dirsForRoot(sourceDir)
      val environment =
        CompilerAPI.makeEnvironment(dirsForSourcesToCompile, Seq(), Seq(), new ErrorLoggingMessageCollector)
      environment.getSourceFiles should not be List()

      val nameGenerator = new DefaultTypeInfoProvider(environment)
      nameGenerator.bindingContext should not be null
      nameGenerator.bindingContext should not be BindingContext.EMPTY
    }
  }
}
