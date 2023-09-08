package io.joern.javasrc2cpg.scope

import io.joern.x2cpg.Ast

import scala.collection.mutable

trait TypeDeclContainer {
  private val typeDeclsToAdd = mutable.ListBuffer[Ast]()
  private val lambdaMethods  = mutable.ListBuffer[Ast]()

  def registerTypeDecl(typeDecl: Ast) = {
    typeDeclsToAdd.append(typeDecl)
  }

  def registeredTypeDecls: List[Ast] = typeDeclsToAdd.toList

  // TODO: Refactor and remove this
  def registerLambdaMethod(lambdaMethod: Ast) = {
    lambdaMethods.append(lambdaMethod)
  }

  def registeredLambdaMethods: List[Ast] = lambdaMethods.toList
}
