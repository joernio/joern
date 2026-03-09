package io.joern.swiftsrc2cpg.passes

import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.codepropertygraph.generated.nodes.NewDependency
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.Logger
import org.slf4j.LoggerFactory

/** Creation of DEPENDENCY nodes from "Package.swift" files.
  */
class DependenciesPass(cpg: Cpg) extends CpgPass(cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private val VersionIdSpecifier = Set("from", "branch", "revision", "exact")

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    cpg.file("Package.swift").ast.isCall.nameExact("package").foreach { call =>
      call.argument.argumentIndexGt(0).l match {
        case Nil => // Do nothing
        case (lit: Literal) :: Nil if lit.argumentLabel.contains("path") =>
          val name = X2Cpg.stripQuotes(lit.code)
          val dep  = NewDependency().name(name)
          diffGraph.addNode(dep)
        case (lit1: Literal) :: (lit2: Literal) :: Nil
            if lit1.argumentLabel.contains("name") && lit2.argumentLabel.contains("path") =>
          val name = X2Cpg.stripQuotes(lit1.code)
          val path = X2Cpg.stripQuotes(lit2.code)
          val dep  = NewDependency().name(name).dependencyGroupId(path)
          diffGraph.addNode(dep)
        case (lit1: Literal) :: (lit2: Literal) :: Nil
            if lit1.argumentLabel.contains("url") && lit2.argumentLabel.exists(VersionIdSpecifier.contains) =>
          val name    = X2Cpg.stripQuotes(lit1.code)
          val version = X2Cpg.stripQuotes(lit2.code)
          val dep     = NewDependency().name(name).version(version)
          diffGraph.addNode(dep)
        case (lit1: Literal) :: (versionRange: Call) :: Nil if lit1.argumentLabel.contains("url") =>
          val name    = X2Cpg.stripQuotes(lit1.code)
          val version = versionRange.code.replaceAll("[\"']", "")
          val dep     = NewDependency().name(name).version(version)
          diffGraph.addNode(dep)
        case args =>
          logger.warn(s"Unknown dependency specification: '${args.mkString(", ")}'")
      }
    }
  }

}
