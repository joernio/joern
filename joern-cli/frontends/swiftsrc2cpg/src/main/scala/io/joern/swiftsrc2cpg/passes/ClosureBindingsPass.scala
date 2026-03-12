package io.joern.swiftsrc2cpg.passes

import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.passes.base.{MethodStubCreator, TypeDeclStubCreator}
import io.shiftleft.codepropertygraph.generated.nodes.{Method, NewBinding, NewMethod, NewTypeDecl, TypeDecl}
import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, EdgeTypes}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

/** Adds `BINDS`/`REF` bindings from the synthetic function type to generated closure functions.
  *
  * Each closure function is exposed through the canonical apply name so dispatch can resolve closure calls via bindings
  * like regular function-type invocations.
  *
  * @param cpg
  *   graph to enrich with closure bindings
  */
class ClosureBindingsPass(cpg: Cpg) extends CpgPass(cpg) {

  private val seenTypeDecls    = scala.collection.mutable.HashMap.empty[String, NewTypeDecl]
  private val seenBoundMethods = scala.collection.mutable.HashMap.empty[String, NewMethod]

  private def stubTypeDeclIfNeeded(diffGraph: DiffGraphBuilder, fullName: String): TypeDecl | NewTypeDecl = {
    if (cpg.typeDecl.fullNameExact(fullName).isEmpty) {
      seenTypeDecls.getOrElseUpdate(
        fullName, {
          val typeDeclStub = TypeDeclStubCreator.createTypeDeclStub("Function", fullName)
          diffGraph.addNode(typeDeclStub)
          typeDeclStub
        }
      )
    } else {
      cpg.typeDecl.fullNameExact(fullName).loneElement
    }
  }

  private def stubBoundMethodIfNeeded(
    diffGraph: DiffGraphBuilder,
    closureMethodFullName: String,
    closureMethod: Method
  ): Method | NewMethod = {
    val methodFullName = s"$closureMethodFullName.${Defines.ClosureApplyMethodName}:${closureMethod.signature}"
    val numArgs        = closureMethod.parameter.size
    if (cpg.method.fullNameExact(methodFullName).isEmpty) {
      seenBoundMethods.getOrElseUpdate(
        methodFullName, {
          MethodStubCreator.createMethodStub(
            Defines.ClosureApplyMethodName,
            methodFullName,
            closureMethod.signature,
            DispatchTypes.DYNAMIC_DISPATCH,
            numArgs,
            diffGraph
          )
        }
      )
    } else {
      cpg.method.fullNameExact(methodFullName).loneElement
    }
  }

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    for {
      closureMethod            <- cpg.method.isLambda
      closureMethodTypeDecl    <- closureMethod.bindingTypeDecl
      inheritsFromTypeFullName <- closureMethodTypeDecl.inheritsFromTypeFullName.loneElementOption
      closureTypeDecl = stubTypeDeclIfNeeded(diffGraph, inheritsFromTypeFullName)
      boundMethod     = stubBoundMethodIfNeeded(diffGraph, inheritsFromTypeFullName, closureMethod)
    } {
      val functionBinding = NewBinding()
        .name(Defines.ClosureApplyMethodName)
        .methodFullName(boundMethod.fullName)
        .signature(boundMethod.signature)
      diffGraph.addNode(functionBinding)
      diffGraph.addEdge(closureTypeDecl, functionBinding, EdgeTypes.BINDS)
      diffGraph.addEdge(functionBinding, boundMethod, EdgeTypes.REF)
    }
  }

}
