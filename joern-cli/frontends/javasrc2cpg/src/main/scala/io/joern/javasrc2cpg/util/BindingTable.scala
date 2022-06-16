package io.joern.javasrc2cpg.util

import com.github.javaparser.resolution.declarations.{ResolvedMethodDeclaration, ResolvedReferenceTypeDeclaration}
import com.github.javaparser.resolution.types.ResolvedReferenceType
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import io.joern.javasrc2cpg.util.Util.{composeMethodFullName, getAllParents}
import io.shiftleft.codepropertygraph.generated.nodes.{Binding, NewBinding, NewTypeDecl, TypeDecl}

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.util.Try

case class BindingTableEntry(name: String, signature: String, implementingMethodFullName: String)

class BindingTable() {
  private val entries = mutable.Map.empty[String, BindingTableEntry]

  def add(entry: BindingTableEntry): Unit = {
    entries.put(entry.name + entry.signature, entry)
  }

  def getEntries: Iterable[BindingTableEntry] = {
    entries.values
  }
}

trait BindingTableAdapter[T] {
  def directParents(typeDecl: T): collection.Seq[ResolvedReferenceTypeDeclaration]

  def allParentsWithTypeMap(typeDecl: T): collection.Seq[(ResolvedReferenceTypeDeclaration, ResolvedTypeParametersMap)]

  def directBindingTableEntries(typeDeclFullName: String, typeDecl: T): collection.Seq[BindingTableEntry]
}

class BindingTableAdapterForJavaparser(
  methodSignature: (ResolvedMethodDeclaration, ResolvedTypeParametersMap) => String
) extends BindingTableAdapter[ResolvedReferenceTypeDeclaration] {
  override def directParents(
    typeDecl: ResolvedReferenceTypeDeclaration
  ): collection.Seq[ResolvedReferenceTypeDeclaration] = {
    typeDecl.getAncestors(true).asScala.map(_.getTypeDeclaration.get)
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

    typeDecl.getDeclaredMethods.asScala.iterator
      .filter(methodDecl => !methodDecl.isStatic)
      .map { methodDecl =>
        val signature = methodSignature(methodDecl, ResolvedTypeParametersMap.empty())
        BindingTableEntry.apply(
          methodDecl.getName,
          signature,
          composeMethodFullName(typeDeclFullName, methodDecl.getName, signature)
        )
      }
      .toBuffer
  }
}

case class LambdaBindingInfo(
  fullName: String,
  implementedType: Option[ResolvedReferenceType],
  directBinding: Option[NewBinding]
)

class BindingTableAdapterForLambdas(
) extends BindingTableAdapter[LambdaBindingInfo] {

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
}

object BindingTable {
  def createBindingTable[T](
    typeDeclFullName: String,
    typeDecl: T,
    getBindingTable: ResolvedReferenceTypeDeclaration => BindingTable,
    methodSignature: (ResolvedMethodDeclaration, ResolvedTypeParametersMap) => String,
    adapter: BindingTableAdapter[T]
  ): BindingTable = {
    val bindingTable = new BindingTable()

    // Take over all binding table entries for parent class/interface binding tables.
    adapter.directParents(typeDecl).foreach { parentTypeDecl =>
      val parentBindingTable = getBindingTable(parentTypeDecl)
      parentBindingTable.getEntries.foreach { entry =>
        bindingTable.add(entry)
      }
    }

    // Create table entries for all methods declared in type declaration.
    val directTableEntries = adapter.directBindingTableEntries(typeDeclFullName, typeDecl)

    // Add all table entries for method of type declaration to binding table.
    // It is important that this happens after adding the inherited entries
    // because later entries for the same slot (same name and signature)
    // override previously added entries.
    directTableEntries.foreach(bindingTable.add)

    // Override the bindings for generic base class methods if they are overriden.
    // To do so we need to traverse all methods in all parent type and calculate
    // their signature in the derived type declarations context, meaning with the
    // concrete values for the generic type parameters. If this signature together
    // with the name matches a direct table entry we have an override and replace
    // the binding table entry for the erased! parent method signature.
    // This become necessary because calls in the JVM executed via erased signatures.
    adapter.allParentsWithTypeMap(typeDecl).foreach { case (parentTypeDecl, typeParameterInDerivedContext) =>
      directTableEntries.foreach { directTableEntry =>
        val parentMethods = parentTypeDecl.getDeclaredMethods.asScala
        parentMethods.foreach { parentMethodDecl =>
          if (directTableEntry.name == parentMethodDecl.getName) {
            val parentSigInDerivedContext = methodSignature(parentMethodDecl, typeParameterInDerivedContext)
            if (directTableEntry.signature == parentSigInDerivedContext) {
              val erasedParentMethodSig = methodSignature(parentMethodDecl, ResolvedTypeParametersMap.empty())
              val tableEntry = BindingTableEntry
                .apply(directTableEntry.name, erasedParentMethodSig, directTableEntry.implementingMethodFullName)
              bindingTable.add(tableEntry)
            }
          }
        }
      }
    }

    bindingTable
  }
}
