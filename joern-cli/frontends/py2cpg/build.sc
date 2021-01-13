import coursier.MavenRepository
import mill._
import mill.scalalib.scalafmt.ScalafmtModule
import scalalib._

object py2cpg extends SbtModule with ScalafmtModule {
  def scalaVersion = "2.13.1"
  def jfrogUri: String = "https://shiftleft.jfrog.io/shiftleft/libs-release-local"

  // We only have one module in this build. Thus we dont need
  // the usual directory level introduced by the module name.
  // So our source will be in src/main/scala and not
  // py2cpg/src/main/scala
  override def millSourcePath = millOuterCtx.millSourcePath

  def jfrogUser = T.input {
    T.ctx.env.getOrElse("JFROG_USER",
      throw new RuntimeException("Environment variable JFROG_USER not defined."))
  }

  def jfrogPassword = T.input {
    T.ctx.env.getOrElse("JFROG_PASSWORD",
      throw new RuntimeException("Environment variable JFROG_PASSWORD not defined."))
  }

  override def repositoriesTask = T.task {
    super.repositoriesTask() ++
      Seq(MavenRepository(jfrogUri,
        authentication = Some(coursier.core.Authentication(jfrogUser(), jfrogPassword()))))
  }

  override def ivyDeps = Agg(
    ivy"org.python.pydev:parser:8.1.0",
    ivy"org.python.pydev:core:8.1.0",
    ivy"org.python.pydev:shared_core:8.1.0",
    ivy"org.eclipse.platform:org.eclipse.core.resources:3.13.900",
    ivy"org.eclipse.platform:org.eclipse.text:3.10.400",
    ivy"commons-io:commons-io:2.8.0",
  )

  object test extends Tests {
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.2")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}
