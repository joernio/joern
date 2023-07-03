package io.joern.rubysrc2cpg.passes

import better.files.File
import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.utils.{PackageContext, PackageTable}
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import better.files.File
import org.jruby.Ruby
import org.jruby.ast.{Colon2Node, DefnNode, Node, NodeType, ReturnNode}

import scala.collection.mutable.ListBuffer
import scala.util.Try

class AstPackagePass(cpg: Cpg, tempExtDir: String, global: Global, packageTable: PackageTable, inputPath: String)
    extends ConcurrentWriterCpgPass[String](cpg) {

  private val rubyInstance = Ruby.getGlobalRuntime()
  override def generateParts(): Array[String] =
    getRubyDependenciesFile(inputPath) ++ getRubyDependenciesFile(tempExtDir)

  override def runOnPart(diffGraph: DiffGraphBuilder, filePath: String): Unit = {
    val moduleName = resolveModuleNameFromPath(filePath)
    try {
      processRubyDependencyFile(filePath, moduleName)
    } catch {
      case ex: Exception =>
        println(s"Error while parsing $moduleName module File ($filePath): ${ex.getMessage}")
    }
  }

  private def fetchMethodInfoFromNode(node: Node, currentNameSpace: ListBuffer[String], moduleName: String): Unit = {
    if (node != null) {
      println(node.getNodeType)
      node.getNodeType match {
        case NodeType.CLASSNODE | NodeType.MODULENODE =>


          node.childNodes().forEach(child => {

          })



          println("ppp")
          val classOrModuleName = node.childNodes().get(0).asInstanceOf[Colon2Node].getName.toString
          currentNameSpace.addOne(classOrModuleName)
        case NodeType.DEFNNODE =>





          val methodName = node.asInstanceOf[DefnNode].getName.toString
          println(methodName)
          val classPath = currentNameSpace.mkString(".")
          println("   " + classPath + "." + methodName)
          // val returnType = node.asInstanceOf[DefnNode].getReturnType.map(_.toString).getOrElse("")
          packageTable.addPackageMethod(moduleName, methodName, classPath, "<extMod>")
        case _ =>
      }
      node.childNodes().forEach(childNode => fetchMethodInfoFromNode(childNode, currentNameSpace, moduleName))
    }
  }

  private def extractReturnInfo(node: Node): String = {
    val returnTypeNode = node.childNodes().forEach(childNode => {
      childNode.getNodeType match {
        case ReturnNode =>

        case _ => ""
      }
    })
    returnTypeNode.map (_.getChildNodes.get (0).toString).getOrElse ("")
  }

  private def processRubyDependencyFile(inputPath: String, moduleName: String): Unit = {
    val currentFile = File(inputPath)
    if (currentFile.exists) {
      println(moduleName)
      val rubyFile = new java.io.File(inputPath)
      val rubyCode = scala.io.Source.fromFile(rubyFile).mkString
      val rootNode = rubyInstance.parseEval(rubyCode, inputPath, null, 0)
      fetchMethodInfoFromNode(rootNode, ListBuffer.empty, moduleName)
    }
  }

  private def getRubyDependenciesFile(inputPath: String): Array[String] = {
    val currentDir = File(inputPath)
    if (currentDir.exists) {
      currentDir.listRecursively.filter(_.extension.exists(_ == ".rb")).map(_.path.toString).toArray
    } else {
      Array.empty
    }
  }

  private def resolveModuleNameFromPath(path: String): String = {
    if (path.contains(tempExtDir)) {
      val moduleNameRegex = Seq("gems", "([^", "]+)", "lib", ".*").mkString(java.io.File.separator).r
      moduleNameRegex
        .findFirstMatchIn(path)
        .map(_.group(1))
        .getOrElse("")
        .split(java.io.File.separator)
        .last
        .split("-")
        .head
    } else {
      path
    }
  }
}
