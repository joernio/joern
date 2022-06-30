package io.shiftleft.semanticcpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.traversal.NodeTraversalImplicits
import io.shiftleft.semanticcpg.language.bindingextension.{
  MethodTraversal => BindingMethodTraversal,
  TypeDeclTraversal => BindingTypeDeclTraversal
}
import io.shiftleft.semanticcpg.language.callgraphextension.{CallTraversal, MethodTraversal}
import io.shiftleft.semanticcpg.language.dotextension.{AstNodeDot, CfgNodeDot, InterproceduralNodeDot}
import io.shiftleft.semanticcpg.language.nodemethods._
import io.shiftleft.semanticcpg.language.types.expressions.generalizations.{
  AstNodeTraversal,
  CfgNodeTraversal,
  ExpressionTraversal
}
import io.shiftleft.semanticcpg.language.types.expressions.{CallTraversal => OriginalCall, _}
import io.shiftleft.semanticcpg.language.types.propertyaccessors._
import io.shiftleft.semanticcpg.language.types.structure.{MethodTraversal => OriginalMethod, _}
import overflowdb.traversal._
import overflowdb.NodeOrDetachedNode

/** Language for traversing the code property graph
  *
  * Implicit conversions to specific steps, based on the node at hand. Automatically in scope when using anything in the
  * `steps` package, e.g. `Steps`
  */
package object language extends operatorextension.Implicits with LowPrioImplicits with NodeTraversalImplicits {
  // Implicit conversions from generated node types. We use these to add methods
  // to generated node types.

  implicit def cfgNodeToAsNode(node: CfgNode): AstNodeMethods                 = new AstNodeMethods(node)
  implicit def toExtendedNode(node: NodeOrDetachedNode): NodeMethods          = new NodeMethods(node)
  implicit def toExtendedStoredNode(node: StoredNode): StoredNodeMethods      = new StoredNodeMethods(node)
  implicit def toAstNodeMethods(node: AstNode): AstNodeMethods                = new AstNodeMethods(node)
  implicit def toCfgNodeMethods(node: CfgNode): CfgNodeMethods                = new CfgNodeMethods(node)
  implicit def toExpressionMethods(node: Expression): ExpressionMethods       = new ExpressionMethods(node)
  implicit def toMethodMethods(node: Method): MethodMethods                   = new MethodMethods(node)
  implicit def toMethodReturnMethods(node: MethodReturn): MethodReturnMethods = new MethodReturnMethods(node)
  implicit def toCallMethods(node: Call): CallMethods                         = new CallMethods(node)
  implicit def toMethodParamInMethods(node: MethodParameterIn): MethodParameterInMethods =
    new MethodParameterInMethods(node)
  implicit def toMethodParamOutMethods(node: MethodParameterOut): MethodParameterOutMethods =
    new MethodParameterOutMethods(node)
  implicit def toIdentifierMethods(node: Identifier): IdentifierMethods = new IdentifierMethods(node)
  implicit def toLiteralMethods(node: Literal): LiteralMethods          = new LiteralMethods(node)
  implicit def toLocalMethods(node: Local): LocalMethods                = new LocalMethods(node)
  implicit def toMethodRefMethods(node: MethodRef): MethodRefMethods    = new MethodRefMethods(node)

  // Implicit conversions from Step[NodeType, Label] to corresponding Step classes.
  // If you introduce a new Step-type, that is, one that inherits from `Steps[NodeType]`,
  // then you need to add an implicit conversion from `Steps[NodeType]` to your type
  // here.

  implicit def singleToTypeTrav[A <: Type](a: A): TypeTraversal =
    new TypeTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToTypeTrav[A <: Type](a: IterableOnce[A]): TypeTraversal =
    new TypeTraversal(iterableToTraversal(a))

  implicit def singleToTypeDeclTrav[A <: TypeDecl](a: A): TypeDeclTraversal =
    new TypeDeclTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToTypeDeclTrav[A <: TypeDecl](a: IterableOnce[A]): TypeDeclTraversal =
    new TypeDeclTraversal(iterableToTraversal(a))

  implicit def iterOnceToOriginalCallTrav[A <: Call](a: IterableOnce[A]): OriginalCall =
    new OriginalCall(iterableToTraversal(a))

  implicit def singleToControlStructureTrav[A <: ControlStructure](a: A): ControlStructureTraversal =
    new ControlStructureTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToControlStructureTrav[A <: ControlStructure](a: IterableOnce[A]): ControlStructureTraversal =
    new ControlStructureTraversal(iterableToTraversal(a))

  implicit def singleToIdentifierTrav[A <: Identifier](a: A): IdentifierTraversal =
    new IdentifierTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToIdentifierTrav[A <: Identifier](a: IterableOnce[A]): IdentifierTraversal =
    new IdentifierTraversal(iterableToTraversal(a))

  implicit def singleToAnnotationTrav[A <: Annotation](a: A): AnnotationTraversal =
    new AnnotationTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToAnnotationTrav[A <: Annotation](a: IterableOnce[A]): AnnotationTraversal =
    new AnnotationTraversal(iterableToTraversal(a))

  implicit def singleToAnnotationParameterAssignTrav[A <: AnnotationParameterAssign](
    a: A
  ): AnnotationParameterAssignTraversal =
    new AnnotationParameterAssignTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToAnnotationParameterAssignTrav[A <: AnnotationParameterAssign](
    a: IterableOnce[A]
  ): AnnotationParameterAssignTraversal =
    new AnnotationParameterAssignTraversal(iterableToTraversal(a))

  implicit def toMember(traversal: IterableOnce[Member]): MemberTraversal = new MemberTraversal(traversal)
  implicit def toLocal(traversal: IterableOnce[Local]): LocalTraversal    = new LocalTraversal(traversal)
  implicit def toMethod(traversal: IterableOnce[Method]): OriginalMethod  = new OriginalMethod(traversal)

  implicit def singleToMethodParameterInTrav[A <: MethodParameterIn](a: A): MethodParameterTraversal =
    new MethodParameterTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToMethodParameterInTrav[A <: MethodParameterIn](a: IterableOnce[A]): MethodParameterTraversal =
    new MethodParameterTraversal(iterableToTraversal(a))

  implicit def singleToMethodParameterOutTrav[A <: MethodParameterOut](a: A): MethodParameterOutTraversal =
    new MethodParameterOutTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToMethodParameterOutTrav[A <: MethodParameterOut](
    a: IterableOnce[A]
  ): MethodParameterOutTraversal =
    new MethodParameterOutTraversal(iterableToTraversal(a))

  implicit def iterOnceToMethodReturnTrav[A <: MethodReturn](a: IterableOnce[A]): MethodReturnTraversal =
    new MethodReturnTraversal(iterableToTraversal(a))

  implicit def singleToNamespaceTrav[A <: Namespace](a: A): NamespaceTraversal =
    new NamespaceTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToNamespaceTrav[A <: Namespace](a: IterableOnce[A]): NamespaceTraversal =
    new NamespaceTraversal(iterableToTraversal(a))

  implicit def singleToNamespaceBlockTrav[A <: NamespaceBlock](a: A): NamespaceBlockTraversal =
    new NamespaceBlockTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToNamespaceBlockTrav[A <: NamespaceBlock](a: IterableOnce[A]): NamespaceBlockTraversal =
    new NamespaceBlockTraversal(iterableToTraversal(a))

  implicit def singleToFileTrav[A <: File](a: A): FileTraversal =
    new FileTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToFileTrav[A <: File](a: IterableOnce[A]): FileTraversal =
    new FileTraversal(iterableToTraversal(a))

  implicit def singleToImportTrav[A <: Import](a: A): ImportTraversal =
    new ImportTraversal(Traversal.fromSingle(a))

  implicit def iterToImportTrav[A <: Import](a: IterableOnce[A]): ImportTraversal =
    new ImportTraversal(iterableToTraversal(a))

  // Call graph extension
  implicit def singleToMethodTravCallGraphExt[A <: Method](a: A): MethodTraversal =
    new MethodTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToMethodTravCallGraphExt[A <: Method](a: IterableOnce[A]): MethodTraversal =
    new MethodTraversal(iterableToTraversal(a))
  implicit def singleToCallTrav[A <: Call](a: A): CallTraversal =
    new CallTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToCallTrav[A <: Call](a: IterableOnce[A]): CallTraversal =
    new CallTraversal(iterableToTraversal(a))
  // / Call graph extension

  // Binding extensions
  implicit def singleToBindingMethodTrav[A <: Method](a: A): BindingMethodTraversal =
    new BindingMethodTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToBindingMethodTrav[A <: Method](a: IterableOnce[A]): BindingMethodTraversal =
    new BindingMethodTraversal(iterableToTraversal(a))

  implicit def singleToBindingTypeDeclTrav[A <: TypeDecl](a: A): BindingTypeDeclTraversal =
    new BindingTypeDeclTraversal(Traversal.fromSingle(a))
  implicit def iterOnceToBindingTypeDeclTrav[A <: TypeDecl](a: IterableOnce[A]): BindingTypeDeclTraversal =
    new BindingTypeDeclTraversal(iterableToTraversal(a))

  implicit def singleToAstNodeDot[A <: AstNode](a: A): AstNodeDot[A] =
    new AstNodeDot(Traversal.fromSingle(a))
  implicit def iterOnceToAstNodeDot[A <: AstNode](a: IterableOnce[A]): AstNodeDot[A] =
    new AstNodeDot(iterableToTraversal(a))

  implicit def singleToCfgNodeDot[A <: Method](a: A): CfgNodeDot =
    new CfgNodeDot(Traversal.fromSingle(a))
  implicit def iterOnceToCfgNodeDot[A <: Method](a: IterableOnce[A]): CfgNodeDot =
    new CfgNodeDot(iterableToTraversal(a))

  implicit def graphToInterproceduralDot(cpg: Cpg): InterproceduralNodeDot =
    new InterproceduralNodeDot(cpg)

  implicit def toTraversal[NodeType <: StoredNode](node: NodeType): Traversal[NodeType] =
    Traversal.fromSingle(node)

  implicit def toSteps[A](trav: Traversal[A]): Steps[A] = new Steps(trav)
  implicit def iterOnceToNodeSteps[A <: StoredNode](a: IterableOnce[A]): NodeSteps[A] =
    new NodeSteps[A](iterableToTraversal(a))

  implicit def toNewNodeTrav[NodeType <: NewNode](trav: Traversal[NodeType]): NewNodeSteps[NodeType] =
    new NewNodeSteps[NodeType](trav)

  implicit def toNodeTypeStarters(cpg: Cpg): NodeTypeStarters     = new NodeTypeStarters(cpg)
  implicit def toTagTraversal(trav: Traversal[Tag]): TagTraversal = new TagTraversal(trav)

  // ~ EvalType accessors
  implicit def singleToEvalTypeAccessorsLocal[A <: Local](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Traversal.fromSingle(a))
  implicit def iterOnceToEvalTypeAccessorsLocal[A <: Local](a: IterableOnce[A]): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](iterableToTraversal(a))

  implicit def singleToEvalTypeAccessorsMember[A <: Member](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Traversal.fromSingle(a))
  implicit def iterOnceToEvalTypeAccessorsMember[A <: Member](a: IterableOnce[A]): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](iterableToTraversal(a))

  implicit def singleToEvalTypeAccessorsMethod[A <: Method](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Traversal.fromSingle(a))
  implicit def iterOnceToEvalTypeAccessorsMethod[A <: Method](a: IterableOnce[A]): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](iterableToTraversal(a))

  implicit def singleToEvalTypeAccessorsParameterIn[A <: MethodParameterIn](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Traversal.fromSingle(a))
  implicit def iterOnceToEvalTypeAccessorsParameterIn[A <: MethodParameterIn](
    a: IterableOnce[A]
  ): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](iterableToTraversal(a))

  implicit def singleToEvalTypeAccessorsParameterOut[A <: MethodParameterOut](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Traversal.fromSingle(a))
  implicit def iterOnceToEvalTypeAccessorsParameterOut[A <: MethodParameterOut](
    a: IterableOnce[A]
  ): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](iterableToTraversal(a))

  implicit def singleToEvalTypeAccessorsMethodReturn[A <: MethodReturn](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Traversal.fromSingle(a))
  implicit def iterOnceToEvalTypeAccessorsMethodReturn[A <: MethodReturn](a: IterableOnce[A]): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](iterableToTraversal(a))

  implicit def singleToEvalTypeAccessorsExpression[A <: Expression](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Traversal.fromSingle(a))
  implicit def iterOnceToEvalTypeAccessorsExpression[A <: Expression](a: IterableOnce[A]): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](iterableToTraversal(a))

  // EvalType accessors ~

  // ~ Modifier accessors
  implicit def singleToModifierAccessorsMember[A <: Member](a: A): ModifierAccessors[A] =
    new ModifierAccessors[A](Traversal.fromSingle(a))
  implicit def iterOnceToModifierAccessorsMember[A <: Member](a: IterableOnce[A]): ModifierAccessors[A] =
    new ModifierAccessors[A](iterableToTraversal(a))

  implicit def singleToModifierAccessorsMethod[A <: Method](a: A): ModifierAccessors[A] =
    new ModifierAccessors[A](Traversal.fromSingle(a))
  implicit def iterOnceToModifierAccessorsMethod[A <: Method](a: IterableOnce[A]): ModifierAccessors[A] =
    new ModifierAccessors[A](iterableToTraversal(a))

  implicit def singleToModifierAccessorsTypeDecl[A <: TypeDecl](a: A): ModifierAccessors[A] =
    new ModifierAccessors[A](Traversal.fromSingle(a))
  implicit def iterOnceToModifierAccessorsTypeDecl[A <: TypeDecl](a: IterableOnce[A]): ModifierAccessors[A] =
    new ModifierAccessors[A](iterableToTraversal(a))
  // Modifier accessors ~

  implicit class NewNodeTypeDeco[NodeType <: NewNode](val node: NodeType) extends AnyVal {

    /** Start a new traversal from this node
      */
    def start: Traversal[NodeType] =
      Traversal.fromSingle(node)
  }

  implicit def toExpression[A <: Expression](a: IterableOnce[A]): ExpressionTraversal[A] =
    new ExpressionTraversal[A](iterableToTraversal(a))
}

trait LowPrioImplicits {
  implicit def singleToCfgNodeTraversal[A <: CfgNode](a: A): CfgNodeTraversal[A] =
    new CfgNodeTraversal[A](Traversal.fromSingle(a))
  implicit def iterOnceToCfgNodeTraversal[A <: CfgNode](a: IterableOnce[A]): CfgNodeTraversal[A] =
    new CfgNodeTraversal[A](iterableToTraversal(a))

  implicit def singleToAstNodeTraversal[A <: AstNode](a: A): AstNodeTraversal[A] =
    new AstNodeTraversal[A](Traversal.fromSingle(a))
  implicit def iterOnceToAstNodeTraversal[A <: AstNode](a: IterableOnce[A]): AstNodeTraversal[A] =
    new AstNodeTraversal[A](iterableToTraversal(a))
}
