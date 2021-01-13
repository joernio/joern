import mill._
import scalalib._

object py2cpg extends SbtModule {
  def scalaVersion = "2.13.1"

  // We only have one module in this build. Thus we dont need
  // the usual directory level introduced by the module name.
  // So our source will be in src/main/scala and not
  // py2cpg/src/main/scala
  override def millSourcePath = millOuterCtx.millSourcePath

  override def ivyDeps = Agg(
    ivy"org.eclipse.platform:org.eclipse.core.resources:3.13.900",
    ivy"org.eclipse.platform:org.eclipse.text:3.10.400",
    ivy"commons-io:commons-io:2.8.0",
  )

  object test extends Tests {
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.2")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}
