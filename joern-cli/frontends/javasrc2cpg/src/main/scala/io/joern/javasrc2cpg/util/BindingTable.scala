package io.joern.javasrc2cpg.util

import com.github.javaparser.resolution.declarations.{ResolvedMethodDeclaration, ResolvedReferenceTypeDeclaration}
import com.github.javaparser.resolution.types.ResolvedReferenceType
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap

import scala.collection.mutable
import scala.jdk.CollectionConverters._

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

object BindingTable {
  def createBindingTable(
    typeDeclFullName: String,
    typeDecl: ResolvedReferenceTypeDeclaration,
    getBindingTable: ResolvedReferenceTypeDeclaration => BindingTable,
    methodSignature: (ResolvedMethodDeclaration, ResolvedTypeParametersMap) => String
  ): BindingTable = {
    val bindingTable = new BindingTable()

    // Take over all binding table entries for parent class/interface binding tables.
    val ancestors = typeDecl.getAncestors(true).asScala
    ancestors.foreach { parentType =>
      val parentTypeDecl     = parentType.getTypeDeclaration.get
      val parentBindingTable = getBindingTable(parentTypeDecl)
      parentBindingTable.getEntries.foreach { entry =>
        bindingTable.add(entry)
      }
    }

    // Create table entries for all methods declared in type declaration.
    val directTableEntries =
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
    val allParents = getAllParents(typeDecl)
    allParents.foreach { parentType =>
      val typeParameterInDerivedContext = parentType.typeParametersMap()
      directTableEntries.foreach { directTableEntry =>
        val parentMethods = parentType.getTypeDeclaration.get.getDeclaredMethods.asScala
        parentMethods.foreach { parentMethodDecl =>
          if (directTableEntry.name == parentMethodDecl.getName) {
            val parentSigInDerivedContext = methodSignature(parentMethodDecl, typeParameterInDerivedContext)
            if (directTableEntry.signature == parentSigInDerivedContext) {
              val erasedParentMethodSig = methodSignature(parentMethodDecl, ResolvedTypeParametersMap.empty())
              val tableEntry = BindingTableEntry.apply(
                directTableEntry.name,
                erasedParentMethodSig,
                directTableEntry.implementingMethodFullName
              )
              bindingTable.add(tableEntry)
            }
          }
        }
      }
    }

    bindingTable
  }

  private def getAllParents(typeDecl: ResolvedReferenceTypeDeclaration): mutable.ArrayBuffer[ResolvedReferenceType] = {
    val result = mutable.ArrayBuffer.empty[ResolvedReferenceType]

    if (!typeDecl.isJavaLangObject) {
      typeDecl.getAncestors(true).asScala.foreach { ancestor =>
        result.append(ancestor)
        getAllParents(ancestor, result)
      }
    }

    result
  }

  private def getAllParents(typ: ResolvedReferenceType, result: mutable.ArrayBuffer[ResolvedReferenceType]): Unit = {
    if (typ.isJavaLangObject) {
      Iterable.empty
    } else {
      typ.getDirectAncestors.asScala.foreach { ancestor =>
        result.append(ancestor)
        getAllParents(ancestor, result)
      }
    }
  }

  private def composeMethodFullName(typeDeclFullName: String, name: String, signature: String): String = {
    s"$typeDeclFullName.$name:$signature"
  }
}
