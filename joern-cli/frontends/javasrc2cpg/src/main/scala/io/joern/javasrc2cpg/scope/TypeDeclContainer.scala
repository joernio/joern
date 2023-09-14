package io.joern.javasrc2cpg.scope

import io.joern.javasrc2cpg.util.MultiBindingTableAdapterForJavaparser.JavaparserBindingDeclType
import io.joern.x2cpg.Ast

import scala.collection.mutable

trait TypeDeclContainer {
  private val typeDeclsToAdd         = mutable.ListBuffer[Ast]()
  private val lambdaMethods          = mutable.ListBuffer[Ast]()
  private val localClassBindingTypes = mutable.Map[String, JavaparserBindingDeclType]()

  def registerTypeDecl(typeDecl: Ast) = {
    typeDeclsToAdd.append(typeDecl)
  }

  def registeredTypeDecls: List[Ast] = typeDeclsToAdd.toList

  // TODO: Refactor and remove this
  def registerLambdaMethod(lambdaMethod: Ast) = {
    lambdaMethods.append(lambdaMethod)
  }

  def registeredLambdaMethods: List[Ast] = lambdaMethods.toList

  def addDeclBinding(name: String, declBindingInfo: JavaparserBindingDeclType): Unit = {
    localClassBindingTypes.put(name, declBindingInfo)
  }

  def getDeclBinding(name: String): Option[JavaparserBindingDeclType] = {
    localClassBindingTypes.get(name)
  }
}
