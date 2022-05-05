package io.joern.solidity2cpg.util
import io.joern.solidity2cpg.domain.SuryaObject._
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies, NodeTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{NewAnnotation, NewAnnotationLiteral, NewAnnotationParameter, NewAnnotationParameterAssign, NewArrayInitializer, NewBinding, NewBlock, NewCall, NewClosureBinding, NewControlStructure, NewFieldIdentifier, NewFile, NewIdentifier, NewJumpTarget, NewLiteral, NewLocal, NewMember, NewMethod, NewMethodParameterIn, NewMethodRef, NewMethodReturn, NewModifier, NewNamespaceBlock, NewNode, NewReturn, NewTypeDecl, NewTypeRef, NewUnknown}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.joern.x2cpg.datastructures.{Global, Scope}

class TypeInfoProvider(global: Global, scope: Scope) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def registerType(typeName:String): String = {
    if (typeName != "ANY") {
      global.usedTypes.putIfAbsent(typeName, true)
    }
    typeName
  }



}
