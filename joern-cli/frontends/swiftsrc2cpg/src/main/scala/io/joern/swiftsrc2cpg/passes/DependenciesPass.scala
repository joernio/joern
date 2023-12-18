package io.joern.swiftsrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.NewDependency
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.Logger
import org.slf4j.LoggerFactory

/** Creation of DEPENDENCY nodes from "Package.swift" files.
  */
class DependenciesPass(cpg: Cpg) extends CpgPass(cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private val versionIds = Set("from", "branch", "revision", "exact")

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    cpg.file("Package.swift").ast.isCall.nameExact("package").foreach { call =>
      call.argument.l match
        case Nil => // Do nothing
        case _ :: (pathArg: Call) :: Nil if pathArg.argument(1).code == "path" =>
          val name = pathArg.argument(2).code
          val dep  = NewDependency().name(name)
          diffGraph.addNode(dep)
        case _ :: (nameArg: Call) :: (pathArg: Call) :: Nil
            if nameArg.argument(1).code == "name" && pathArg.argument(1).code == "path" =>
          val name = nameArg.argument(2).code
          val path = pathArg.argument(2).code
          val dep  = NewDependency().name(name).dependencyGroupId(path)
          diffGraph.addNode(dep)
        case _ :: (urlArg: Call) :: (versionArg: Call) :: Nil
            if urlArg.argument(1).code == "url" && versionIds.contains(versionArg.argument(1).code) =>
          val name    = urlArg.argument(2).code
          val version = versionArg.argument(2).code
          val dep     = NewDependency().name(name).version(version)
          diffGraph.addNode(dep)
        case _ :: (urlArg: Call) :: (versionRange: Call) :: Nil if urlArg.argument(1).code == "url" =>
          val name    = urlArg.argument(2).code
          val version = versionRange.code
          val dep     = NewDependency().name(name).version(version)
          diffGraph.addNode(dep)
        case call =>
          logger.warn(s"Unknown dependency specification: '${call.mkString(", ")}'")
    }
  }

}
