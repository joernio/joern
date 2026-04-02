package io.joern.abap2cpg.passes

import io.shiftleft.codepropertygraph.generated.{Cpg, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Local, Method, MethodParameterIn, MethodReturn, StoredNode}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._

/** Type inference pass for ABAP.
  *
  * Propagates type information through the CPG:
  * 1. Identifiers get types from their declarations (LOCAL, METHOD_PARAMETER_IN)
  * 2. Calls get types from method return types
  * 3. Assignments propagate types from RHS to LHS
  *
  * After this pass, TypeEvalPass can create EVAL_TYPE edges from nodes to TYPE nodes.
  */
class AbapTypeInferencePass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  override def generateParts(): Array[Method] = {
    cpg.method.toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, method: Method): Unit = {
    // Build symbol table for this method
    val symbolTable = buildSymbolTable(method)

    // Propagate types to identifiers
    method.ast.isIdentifier.foreach { identifier =>
      symbolTable.get(identifier.name).foreach { declaredType =>
        if (identifier.typeFullName == "ANY" && declaredType != "ANY") {
          builder.setNodeProperty(identifier, PropertyNames.TypeFullName, declaredType)
        }
      }
    }

    // Propagate types to calls
    method.ast.isCall.foreach { call =>
      // Try to resolve return type
      val returnType = resolveCallReturnType(call)
      if (call.typeFullName == "ANY" && returnType != "ANY") {
        builder.setNodeProperty(call, PropertyNames.TypeFullName, returnType)
      }
    }
  }

  /** Build a symbol table mapping variable names to their types */
  private def buildSymbolTable(method: Method): Map[String, String] = {
    val locals = method.local.map(l => l.name -> l.typeFullName).toMap
    val params = method.parameter.map(p => p.name -> p.typeFullName).toMap
    locals ++ params
  }

  /** Resolve the return type of a method call */
  private def resolveCallReturnType(call: Call): String = {
    // Try to find the called method in the CPG
    val calledMethods = cpg.method.fullNameExact(call.methodFullName).toList

    calledMethods.headOption match {
      case Some(targetMethod) =>
        targetMethod.methodReturn.typeFullName
      case None =>
        // Method not found in CPG - it might be external
        // Check if it's an operator
        if (call.name.startsWith("<operator>")) {
          inferOperatorReturnType(call)
        } else {
          "ANY"
        }
    }
  }

  /** Infer return type for operators based on operands */
  private def inferOperatorReturnType(call: Call): String = {
    call.name match {
      case "<operator>.assignment" =>
        // Assignment returns the type of RHS
        call.argument.argumentIndex(2).headOption
          .flatMap(_.propertyOption(PropertyNames.TypeFullName))
          .getOrElse("ANY")

      case "<operator>.fieldAccess" | "<operator>.indirectFieldAccess" =>
        // Field access - we don't know field types without class analysis
        "ANY"

      case "<operator>.addition" | "<operator>.subtraction" | "<operator>.multiplication" | "<operator>.division" =>
        // Arithmetic operators - try to infer from operands
        val operandTypes = call.argument.property(PropertyNames.TypeFullName).toSet
        if (operandTypes.contains("NUMBER") || operandTypes.contains("i")) {
          "NUMBER"
        } else {
          "ANY"
        }

      case _ => "ANY"
    }
  }
}
