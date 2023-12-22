package io.joern.csharpsrc2cpg

import io.joern.csharpsrc2cpg.astcreation.AstCreatorHelper
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.{
  ClassDeclaration,
  FieldDeclaration,
  MethodDeclaration,
  NamespaceDeclaration
}
import io.joern.csharpsrc2cpg.parser.{DotNetJsonAst, DotNetJsonParser, DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.astgen.AstGenRunner.AstGenRunnerResult
import io.joern.x2cpg.datastructures.Stack.Stack
import io.shiftleft.codepropertygraph.generated.nodes.NewNode

import java.nio.file.Paths
import scala.collection.mutable

class TypeMap(astGenResult: AstGenRunnerResult) {

  private val namespaceToType: Map[String, Set[CSharpType]] = astGenResult.parsedFiles
    .map { file =>
      val parserResult    = DotNetJsonParser.readFile(Paths.get(file))
      val compilationUnit = AstCreatorHelper.createDotNetNodeInfo(parserResult.json(ParserKeys.AstRoot))
      () => parseCompilationUnit(compilationUnit)
    }
    .map(task => task()) // TODO: To be parallelized with https://github.com/joernio/joern/pull/4009
    .foldLeft(Map.empty[String, Set[CSharpType]])((a, b) => {
      val accumulator = mutable.HashMap.from(a)
      val allKeys     = accumulator.keySet ++ b.keySet

      allKeys.foreach(k =>
        accumulator.updateWith(k) {
          case Some(existing) => b.get(k).map(x => x ++ existing)
          case None           => b.get(k)
        }
      )
      accumulator.toMap
    })

  /** For the given namespace, returns the declared classes.
    */
  def classesIn(namespace: String): Set[CSharpType] = namespaceToType.getOrElse(namespace, Set.empty)

  /** Parses a compilation unit and returns a mapping from all the contained namespaces and the immediate children
    * types.
    */
  private def parseCompilationUnit(cu: DotNetNodeInfo): Map[String, Set[CSharpType]] = {
    cu.json(ParserKeys.Members)
      .arr
      .map(AstCreatorHelper.createDotNetNodeInfo(_))
      .filter { x =>
        x.node match
          case NamespaceDeclaration => true
          case _                    => false
      }
      .map(parseNamespace)
      .toMap
  }

  private def parseNamespace(namespace: DotNetNodeInfo): (String, Set[CSharpType]) = {
    val namespaceName = AstCreatorHelper.nameFromNode(namespace)
    val classes = namespace
      .json(ParserKeys.Members)
      .arr
      .map(AstCreatorHelper.createDotNetNodeInfo(_))
      .filter { x =>
        x.node match
          case ClassDeclaration => true
          case _                => false
      }
      .map(parseClassDeclaration)
      .toSet
    namespaceName -> classes
  }

  private def parseClassDeclaration(classDecl: DotNetNodeInfo): CSharpType = {
    val className = AstCreatorHelper.nameFromNode(classDecl)
    val members = classDecl
      .json(ParserKeys.Members)
      .arr
      .map(AstCreatorHelper.createDotNetNodeInfo(_))
      .flatMap { x =>
        x.node match
          case MethodDeclaration => parseMethodDeclaration(x)
          case FieldDeclaration  => parseFieldDeclaration(x)
          case _                 => List.empty
      }
      .toList
    CSharpType(className, members)
  }

  private def parseMethodDeclaration(methodDecl: DotNetNodeInfo): List[CSharpMethod] = {
    List(CSharpMethod(AstCreatorHelper.nameFromNode(methodDecl)))
  }

  private def parseFieldDeclaration(fieldDecl: DotNetNodeInfo): List[CSharpField] = {
    val declarationNode = AstCreatorHelper.createDotNetNodeInfo(fieldDecl.json(ParserKeys.Declaration))
    declarationNode
      .json(ParserKeys.Variables)
      .arr
      .map(AstCreatorHelper.createDotNetNodeInfo(_))
      .map(AstCreatorHelper.nameFromNode)
      .map(CSharpField.apply)
      .toList
  }

}

sealed trait CSharpMember {
  def name: String
}

case class CSharpField(name: String) extends CSharpMember

case class CSharpMethod(name: String) extends CSharpMember

case class CSharpType(name: String, members: List[CSharpMember]) extends CSharpMember
