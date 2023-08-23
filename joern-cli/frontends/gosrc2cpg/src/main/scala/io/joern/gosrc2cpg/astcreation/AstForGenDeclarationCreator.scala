package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}
import ujson.Value
import io.joern.x2cpg.datastructures.Stack.StackWrapper

import scala.util.Try

trait AstForGenDeclarationCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>
  def astForGenDecl(genDecl: ParserNodeInfo): Seq[Ast] = {
    genDecl.json(ParserKeys.Tok).str match {
      case "import" => astForImport(genDecl)
      case "const"  => List[Ast]()
      case "type"   => astForTypeSpec(genDecl)
      case "var"    => astForValueSpec(genDecl)

    }
  }

  def astForTypeSpec(genDecl: ParserNodeInfo): Seq[Ast] = {
    genDecl
      .json(ParserKeys.Specs)
      .arr
      .map(createParserNodeInfo)
      .flatMap { typeSpec =>
        typeSpec.node match {
          // TODO: Add support for member variables and methods
          case TypeSpec =>
            val nameNode          = typeSpec.json(ParserKeys.Name)
            val typeNode          = typeSpec.json(ParserKeys.Type)
            val astParentType     = methodAstParentStack.head.label
            val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString
            val typeDeclNode_ =
              typeDeclNode(
                typeSpec,
                nameNode(ParserKeys.Name).str,
                x2cpg.Defines.DynamicCallUnknownFullName, // TODO: Fill in fullName
                parserResult.filename,
                typeSpec.code,
                astParentType,
                astParentFullName
              )

            addModifier(typeDeclNode_, nameNode(ParserKeys.Name).str)

            // TODO: Fill in typeFullName
            val typeRefNode_ =
              typeRefNode(typeSpec, s"type ${nameNode(ParserKeys.Name).str} struct", Defines.anyTypeName)

            diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)
            Seq(Ast(typeRefNode_))
          case _ =>
            Seq(Ast())
        }
      }

    Seq(Ast())
  }

  def astForImport(imports: ParserNodeInfo): Seq[Ast] = {
    imports
      .json(ParserKeys.Specs)
      .arr
      .map(createParserNodeInfo)
      .map { nodeInfo =>
        nodeInfo.node match {
          case ImportSpec =>
            val basicLit       = createParserNodeInfo(nodeInfo.json(ParserKeys.Path))
            val importedEntity = nodeInfo.json(ParserKeys.Path).obj(ParserKeys.Value).str.replaceAll("\"", "")
            val importedAs =
              Try(nodeInfo.json(ParserKeys.Name).obj(ParserKeys.Name).str).toOption
                .getOrElse(importedEntity.split("/").last)
            aliasToNameSpaceMapping.put(importedAs, importedEntity)
            val importedAsReplacement = if (importedEntity.equals(importedAs)) "" else s"$importedAs "
            // This may be better way to add code for import node
            Ast(newImportNode(s"import $importedAsReplacement$importedEntity", importedEntity, importedAs, basicLit))
        }
      }
      .toList
  }

  def astForValueSpec(genDecl: ParserNodeInfo): Seq[Ast] = {
    genDecl
      .json(ParserKeys.Specs)
      .arr
      .map(createParserNodeInfo)
      .flatMap { valueSpec =>
        valueSpec.node match {
          case ValueSpec =>
            // If global array is initialized, type info is present in values > type
            val typeJson = Try(valueSpec.json(ParserKeys.Type))
              .getOrElse(valueSpec.json(ParserKeys.Values).arr.head)
            val typeInfoNode = createParserNodeInfo(typeJson)

            val arrayInitializerNode: Seq[Ast] = typeInfoNode.node match
              case ArrayType =>
                Seq(astForEmptyArrayInitializer(typeInfoNode))
              case _ =>
                Seq.empty

            val localNodes = valueSpec.json(ParserKeys.Names).arr.map { parserNode =>
              val localParserNode = createParserNodeInfo(parserNode)

              val name                                               = parserNode(ParserKeys.Name).str
              val (typeFullName, typeFullNameForcode, isVariadic, _) = processTypeInfo(typeJson)
              val node = localNode(localParserNode, name, localParserNode.code, typeFullName)
              scope.addToScope(name, (node, typeFullName))
              Ast(node)
            }

            if (!valueSpec.json(ParserKeys.Values).isNull) {
              val callNodes =
                (valueSpec.json(ParserKeys.Names).arr.toList zip valueSpec.json(ParserKeys.Values).arr.toList)
                  .map { case (lhs, rhs) => (createParserNodeInfo(lhs), createParserNodeInfo(rhs)) }
                  .map { case (lhsParserNode, rhsParserNode) =>
                    val cNode = callNode(
                      rhsParserNode,
                      lhsParserNode.code + rhsParserNode.code,
                      Operators.assignment,
                      Operators.assignment,
                      DispatchTypes.STATIC_DISPATCH
                    )
                    val arguments = astForNode(lhsParserNode.json) ++: astForNode(rhsParserNode.json)
                    callAst(cNode, arguments)
                  }
              localNodes.toList ::: callNodes ::: arrayInitializerNode.toList
            } else
              localNodes.toList ++ arrayInitializerNode.toList
          case _ =>
            Seq.empty
        }
      }
      .toList

  }
}
