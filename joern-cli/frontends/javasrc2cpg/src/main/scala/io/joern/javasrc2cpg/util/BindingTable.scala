package io.joern.javasrc2cpg.util

import scala.collection.mutable
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.joern.x2cpg.utils.NodeBuilders.newBindingNode
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import io.shiftleft.codepropertygraph.generated.EdgeTypes

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

trait BindingTableAdapter[InputTypeDecl, AstTypeDecl, AstMethodDecl, TypeMap] {
  def directParents(typeDecl: InputTypeDecl): collection.Seq[AstTypeDecl]

  def allParentsWithTypeMap(typeDecl: InputTypeDecl): collection.Seq[(AstTypeDecl, TypeMap)]

  def directBindingTableEntries(typeDeclFullName: String, typeDecl: InputTypeDecl): collection.Seq[BindingTableEntry]

  def getDeclaredMethods(typeDecl: AstTypeDecl): Iterable[(String, AstMethodDecl)]

  def getMethodSignature(methodDecl: AstMethodDecl, typeMap: TypeMap): String

  def getMethodSignatureForEmptyTypeMap(methodDecl: AstMethodDecl): String

  def typeDeclEquals(astTypeDecl: AstTypeDecl, inputTypeDecl: InputTypeDecl): Boolean
}

object BindingTable {
  def createBindingNodes(diffGraph: DiffGraphBuilder, typeDeclNode: NewTypeDecl, bindingTable: BindingTable): Unit = {
    // We only sort to get stable output.
    val sortedEntries =
      bindingTable.getEntries.toBuffer.sortBy((entry: BindingTableEntry) => s"${entry.name}${entry.signature}")

    sortedEntries.foreach { entry =>
      val bindingNode = newBindingNode(entry.name, entry.signature, entry.implementingMethodFullName)

      diffGraph.addNode(bindingNode)
      diffGraph.addEdge(typeDeclNode, bindingNode, EdgeTypes.BINDS)
    }
  }

  def createBindingTable[InputTypeDecl, AstTypeDecl, AstMethodDecl, TypeMap](
    typeDeclFullName: String,
    typeDecl: InputTypeDecl,
    getBindingTable: AstTypeDecl => BindingTable,
    adapter: BindingTableAdapter[InputTypeDecl, AstTypeDecl, AstMethodDecl, TypeMap]
  ): BindingTable = {
    val bindingTable = new BindingTable()

    // Take over all binding table entries for parent class/interface binding tables.
    adapter.directParents(typeDecl).filterNot(adapter.typeDeclEquals(_, typeDecl)).foreach { parentTypeDecl =>
      val parentBindingTable =
        try {
          getBindingTable(parentTypeDecl)
        } catch {
          case e: StackOverflowError =>
            throw new RuntimeException(s"SOE getting binding table for $typeDeclFullName")
        }
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
        val parentMethods = adapter.getDeclaredMethods(parentTypeDecl)
        parentMethods.foreach { case (parentName, parentMethodDecl) =>
          if (directTableEntry.name == parentName) {
            val parentSigInDerivedContext = adapter.getMethodSignature(parentMethodDecl, typeParameterInDerivedContext)
            if (directTableEntry.signature == parentSigInDerivedContext) {
              val erasedParentMethodSig = adapter.getMethodSignatureForEmptyTypeMap(parentMethodDecl)
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
