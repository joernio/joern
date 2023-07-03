package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.BlockStmt
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.NodeBuilders.newMethodReturnNode
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import ujson.Value

trait AstForFunctionsCreator { this: AstCreator =>
  def astForFuncDecl(funcDecl: ParserNodeInfo): Seq[Ast] = {

    val filename       = "fileName"
    val returnType     = "returnType"
    val name           = funcDecl.json(ParserKeys.Name).obj(ParserKeys.Name).str
    val fullname       = "fullname"
    val templateParams = "templateParams"

    val signature =
      s"$returnType $fullname$templateParams {parameterListSignature(funcDef)}"

    val code        = "code"
    val methodNode_ = methodNode(funcDecl, name, code, fullname, Some(signature), filename)

    // TODO Stub for parameter nodes, revisit when we focus on Method nodes
    val parameterNodes = Seq[NewMethodParameterIn]()

    // TODO Add relevant information to stack and cache

    val astForMethod = methodAst(
      methodNode_,
      parameterNodes.map(Ast(_)),
      astForMethodBody(funcDecl.json(ParserKeys.Body)).head,
      newMethodReturnNode(returnType, None, line(funcDecl), column(funcDecl))
    )
    // TODO register type above
    Seq(astForMethod)
  }

  def astForMethodBody(body: Value): Seq[Ast] = {

    val nodeInfo = createParserNodeInfo(body)
    nodeInfo.node match {
      case BlockStmt => astForBlockStatement(nodeInfo)
      case _         => Seq()
    }
  }

}
