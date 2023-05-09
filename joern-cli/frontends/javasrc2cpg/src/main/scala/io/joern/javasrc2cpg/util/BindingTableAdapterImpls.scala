package io.joern.javasrc2cpg.util

import com.github.javaparser.resolution.declarations.{ResolvedMethodDeclaration, ResolvedReferenceTypeDeclaration}
import com.github.javaparser.resolution.types.ResolvedReferenceType
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import com.github.javaparser.symbolsolver.javaparsermodel.declarations.JavaParserAnnotationDeclaration
import com.github.javaparser.symbolsolver.javassistmodel.JavassistAnnotationDeclaration
import com.github.javaparser.symbolsolver.reflectionmodel.ReflectionAnnotationDeclaration
import io.joern.javasrc2cpg.util.Util.{composeMethodFullName, getAllParents, safeGetAncestors}
import io.shiftleft.codepropertygraph.generated.nodes.NewBinding

import scala.jdk.OptionConverters.RichOptional
import scala.jdk.CollectionConverters._

object Shared {
  def getDeclaredMethods(typeDecl: ResolvedReferenceTypeDeclaration): Iterable[ResolvedMethodDeclaration] = {
    typeDecl match {
      // Attempting to get declared methods for annotations throws an UnsupportedOperationException.
      // See https://github.com/javaparser/javaparser/issues/1838 for details.
      case _: JavaParserAnnotationDeclaration => Set.empty
      case _: ReflectionAnnotationDeclaration => Set.empty
      case _: JavassistAnnotationDeclaration  => Set.empty

      case _ => typeDecl.getDeclaredMethods.asScala
    }
  }
}

class BindingTableAdapterForJavaparser(methodSignatureImpl: (ResolvedMethodDeclaration, ResolvedTypeParametersMap) => String)
  extends BindingTableAdapter[
    ResolvedReferenceTypeDeclaration,
    ResolvedReferenceTypeDeclaration,
    ResolvedMethodDeclaration,
  ResolvedTypeParametersMap] {

  override def directParents(
                              typeDecl: ResolvedReferenceTypeDeclaration
                            ): collection.Seq[ResolvedReferenceTypeDeclaration] = {
    safeGetAncestors(typeDecl).map(_.getTypeDeclaration.get)
  }

  override def allParentsWithTypeMap(
                                      typeDecl: ResolvedReferenceTypeDeclaration
                                    ): collection.Seq[(ResolvedReferenceTypeDeclaration, ResolvedTypeParametersMap)] = {
    getAllParents(typeDecl).map { parentType =>
      (parentType.getTypeDeclaration.get, parentType.typeParametersMap())
    }
  }

  override def directBindingTableEntries(
                                          typeDeclFullName: String,
                                          typeDecl: ResolvedReferenceTypeDeclaration
                                        ): collection.Seq[BindingTableEntry] = {
    getDeclaredMethods(typeDecl)
      .filter { case (_, methodDecl) => !methodDecl.isStatic}
      .map { case (_, methodDecl) =>
        val signature = getMethodSignature(methodDecl, ResolvedTypeParametersMap.empty())
        BindingTableEntry.apply(
          methodDecl.getName,
          signature,
          composeMethodFullName(typeDeclFullName, methodDecl.getName, signature)
        )
      }
      .toBuffer
  }

  override def getDeclaredMethods(typeDecl: ResolvedReferenceTypeDeclaration): Iterable[(String, ResolvedMethodDeclaration)] = {
    Shared.getDeclaredMethods(typeDecl).map(method => (method.getName, method))
  }

  override def getMethodSignature(methodDecl: ResolvedMethodDeclaration, typeMap: ResolvedTypeParametersMap): String = {
    methodSignatureImpl(methodDecl, typeMap)
  }

  override def getMethodSignatureForEmptyTypeMap(methodDecl: ResolvedMethodDeclaration): String = {
    methodSignatureImpl(methodDecl, ResolvedTypeParametersMap.empty())
  }

  override def typeDeclEquals(astTypeDecl: ResolvedReferenceTypeDeclaration, inputTypeDecl: ResolvedReferenceTypeDeclaration): Boolean = {
     astTypeDecl.getQualifiedName == inputTypeDecl.getQualifiedName
  }
}

case class LambdaBindingInfo(fullName: String,
                             implementedType: Option[ResolvedReferenceType],
                             directBinding: Option[NewBinding])


class BindingTableAdapterForLambdas(methodSignatureImpl: (ResolvedMethodDeclaration, ResolvedTypeParametersMap) => String)
  extends BindingTableAdapter[
    LambdaBindingInfo,
    ResolvedReferenceTypeDeclaration,
    ResolvedMethodDeclaration,
    ResolvedTypeParametersMap] {

  override def directParents(lambdaBindingInfo: LambdaBindingInfo): collection.Seq[ResolvedReferenceTypeDeclaration] = {
    lambdaBindingInfo.implementedType.flatMap(_.getTypeDeclaration.toScala).toList
  }

  override def allParentsWithTypeMap(
                                      lambdaBindingInfo: LambdaBindingInfo
                                    ): collection.Seq[(ResolvedReferenceTypeDeclaration, ResolvedTypeParametersMap)] = {
    val nonDirectParents =
      lambdaBindingInfo.implementedType.flatMap(_.getTypeDeclaration.toScala).toList.flatMap(getAllParents)
    (lambdaBindingInfo.implementedType.toList ++ nonDirectParents).map { typ =>
      (typ.getTypeDeclaration.get, typ.typeParametersMap())
    }
  }

  override def directBindingTableEntries(
                                          typeDeclFullName: String,
                                          lambdaBindingInfo: LambdaBindingInfo
                                        ): collection.Seq[BindingTableEntry] = {
    lambdaBindingInfo.directBinding.map { binding =>
      BindingTableEntry(binding.name, binding.signature, binding.methodFullName)
    }.toList
  }

  override def getDeclaredMethods(typeDecl: ResolvedReferenceTypeDeclaration): Iterable[(String, ResolvedMethodDeclaration)] = {
    Shared.getDeclaredMethods(typeDecl).map(method => (method.getName, method))
  }

  override def getMethodSignature(methodDecl: ResolvedMethodDeclaration, typeMap: ResolvedTypeParametersMap): String = {
    methodSignatureImpl(methodDecl, typeMap)
  }

  override def getMethodSignatureForEmptyTypeMap(methodDecl: ResolvedMethodDeclaration): String = {
    methodSignatureImpl(methodDecl, ResolvedTypeParametersMap.empty())
  }

  override def typeDeclEquals(astTypeDecl: ResolvedReferenceTypeDeclaration, inputTypeDecl: LambdaBindingInfo): Boolean = {
    astTypeDecl.getQualifiedName == inputTypeDecl.fullName
  }
}

