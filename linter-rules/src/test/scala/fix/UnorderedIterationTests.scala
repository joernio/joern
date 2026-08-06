package fix

import scalafix.testkit.AbstractSemanticRuleSuite

// The test cases live in linter-rules/input/src/main/scala/fix/UnorderedIterationTestCases.scala.
// That file is compiled with the build's Scala version and this suite runs the rule against its
// SemanticDB output (wired via the scalafix-testkit.properties generated in linter-rules/build.sbt).
// Each `/* assert: UnorderedIteration.UnorderedIterationRule */` comment there pins an expected
// diagnostic at that call site.
class UnorderedIterationTests extends AbstractSemanticRuleSuite {
  runAllTests()
}
