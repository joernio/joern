package io.joern.joerncli

import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes._
import overflowdb.Edge

package object slicing {

  /** A trait for all objects that represent a 1:1 relationship between the CPG and all the slices extracted.
    */
  sealed trait ProgramSlice

  /** A data-flow slice vector for a given backwards intraprocedural path.
    *
    * @param nodes
    *   the nodes in the slice.
    * @param edges
    *   a map linking nodes with their edges.
    */
  case class DataFlowSlice(nodes: List[CfgNode], edges: Map[CfgNode, List[Edge]])

  /** The data-flow slices for the program grouped by procedure.
    *
    * @param dataFlowSlices
    *   the mapped slices.
    */
  case class ProgramDataFlowSlice(dataFlowSlices: Map[String, Set[DataFlowSlice]]) extends ProgramSlice

  /** A usage slice of an object at the start of its definition until its final usage.
    *
    * @param targetObj
    *   the name and type of the focus object.
    * @param definedBy
    *   the name of the call, identifier, or literal that defined the target object, if available.
    * @param invokedCalls
    *   calls this object is observed to call.
    * @param argToCalls
    *   the calls this object is observed to be an argument of.
    */
  case class ObjectUsageSlice(
    targetObj: DefComponent,
    definedBy: Option[DefComponent],
    invokedCalls: List[ObservedCall],
    argToCalls: List[(ObservedCall, Int)]
  ) {
    override def toString: String =
      s"{tgt: $targetObj${definedBy.map(p => s" = $p").getOrElse("")}, " +
        s"inv: [${invokedCalls.mkString(",")}], " +
        s"argsTo: [${argToCalls.map { case (callArg: ObservedCall, idx: Int) => s"$callArg@$idx" }.mkString(",")}]" +
        s"}"
  }

  /** Represents a component that carries data. This could be an identifier of a variable or method and supplementary
    * type information, if available.
    *
    * @param name
    *   the name of the object or method call.
    * @param typeFullName
    *   the type full name.
    * @param literal
    *   if this object represents a literal or not.
    */
  case class DefComponent(name: String, typeFullName: String, literal: Boolean = false) {
    override def toString: String = s"$name" +
      (if (typeFullName.nonEmpty) s": $typeFullName" else "") +
      (if (literal) " [LITERAL]" else "")
  }

  object DefComponent {

    /** Attempts to generate an [[DefComponent]] from the given CPG node.
      *
      * @param node
      *   the CPG node.
      * @return
      *   an ID type pair with default values "UNKNOWN" if the respective properties for [[DefComponent]] could not be
      *   extracted.
      */
    def fromNode(node: StoredNode): DefComponent = {
      val name = node match {
        case x: TypeDecl          => x.name
        case x: MethodParameterIn => x.name
        case x: Call              => x.code.takeWhile(_ != '(')
        case x: Identifier        => x.name
        case x: Member            => x.name
        case x: AstNode           => x.code
        case _                    => "UNKNOWN"
      }
      val typs = node.property(PropertyNames.TYPE_FULL_NAME, "ANY") +: node.property(
        PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME,
        Seq.empty[String]
      )
      DefComponent(
        name,
        typs.filterNot(_.matches("(ANY|UNKNOWN)")).headOption.getOrElse("ANY"),
        node.label.equals(Literal.Label)
      )
    }
  }

  /** Details related to an observed call.
    *
    * @param callName
    *   the name of the call.
    * @param paramTypes
    *   the observed parameter types.
    * @param returnType
    *   the observed return type.
    */
  case class ObservedCall(callName: String, paramTypes: List[String], returnType: String) {
    override def toString: String =
      s"$callName(${paramTypes.mkString(",")}):$returnType"
  }

  /** Describes types defined within the application.
    *
    * @param name
    *   name of the type.
    * @param fields
    *   the static or object fields.
    * @param procedures
    *   defined, named procedures within the type.
    */
  case class UserDefinedType(name: String, fields: List[DefComponent], procedures: List[ObservedCall])

  /** The program usage slices and UDTs.
    *
    * @param objectSlices
    *   the object slices under each procedure
    * @param userDefinedTypes
    *   the UDTs.
    */
  case class ProgramUsageSlice(
    objectSlices: Map[String, Set[ObjectUsageSlice]],
    userDefinedTypes: List[UserDefinedType]
  ) extends ProgramSlice

}
