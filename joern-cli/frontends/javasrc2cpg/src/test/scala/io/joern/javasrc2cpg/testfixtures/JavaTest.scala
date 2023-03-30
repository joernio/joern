package io.joern.javasrc2cpg

import io.joern.dataflowengineoss.language._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, StoredNode}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import java.util.Calendar
import scala.collection.mutable.ListBuffer

object JavaTest extends App {
  val cpgconfig = Config(inputPath = "/Users/rahul/IdeaProjects/privado-accounts-ap", fetchDependencies = false)
  val javasrc = JavaSrc2Cpg()
  val cpg = javasrc.createCpg(cpgconfig).map { cpg =>
    applyDefaultOverlays(cpg)
    cpg
  }


  }
