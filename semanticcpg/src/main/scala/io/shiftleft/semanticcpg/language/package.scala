package io.shiftleft.semanticcpg

import flatgraph.help.DocSearchPackages
import io.shiftleft.codepropertygraph.generated
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.SarifExtension
import io.shiftleft.semanticcpg.language.bindingextension.{
  MethodTraversal as BindingMethodTraversal,
  TypeDeclTraversal as BindingTypeDeclTraversal
}
import io.shiftleft.semanticcpg.language.callgraphextension.{CallTraversal, MethodTraversal}
import io.shiftleft.semanticcpg.language.dotextension.{AstNodeDot, CfgNodeDot, InterproceduralNodeDot}
import io.shiftleft.semanticcpg.language.nodemethods.*
import io.shiftleft.semanticcpg.language.types.expressions.generalizations.*
import io.shiftleft.semanticcpg.language.types.expressions.{CallTraversal as OriginalCall, *}
import io.shiftleft.semanticcpg.language.types.propertyaccessors.*
import io.shiftleft.semanticcpg.language.types.structure.{MethodTraversal as OriginalMethod, *}
import io.shiftleft.semanticcpg.language.types.structure.*

/** Language for traversing the code property graph
  *
  * Implicit conversions to specific steps, based on the node at hand. Automatically in scope when using anything in the
  * `steps` package, e.g. `Steps`
  */
package object language
    extends generated.language
    with operatorextension.Implicits
    with modulevariable.Implicits
    with importresolver.Implicits
    with LowPrioImplicits {
  // Implicit conversions from generated node types. We use these to add methods
  // to generated node types.
  implicit def cfgNodeToAstNode(node: CfgNode): AstNodeMethods           = new AstNodeMethods(node)
  implicit def toExtendedNode(node: AbstractNode): NodeMethods           = new NodeMethods(node)
  implicit def toExtendedStoredNode(node: StoredNode): StoredNodeMethods = new StoredNodeMethods(node)
  implicit def toAstNodeMethods(node: AstNode): AstNodeMethods           = new AstNodeMethods(node)
  implicit def toExpressionMethods(node: Expression): ExpressionMethods  = new ExpressionMethods(node)

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
    new TypeTraversal(Iterator.single(a))
  implicit def iterOnceToTypeTrav[A <: Type](a: IterableOnce[A]): TypeTraversal =
    new TypeTraversal(a.iterator)

  implicit def singleToTypeDeclTrav[A <: TypeDecl](a: A): TypeDeclTraversal =
    new TypeDeclTraversal(Iterator.single(a))
  implicit def iterOnceToTypeDeclTrav[A <: TypeDecl](a: IterableOnce[A]): TypeDeclTraversal =
    new TypeDeclTraversal(a.iterator)

  implicit def iterOnceToOriginalCallTrav(traversal: IterableOnce[Call]): OriginalCall = new OriginalCall(traversal)

  implicit def singleToControlStructureTrav[A <: ControlStructure](a: A): ControlStructureTraversal =
    new ControlStructureTraversal(Iterator.single(a))
  implicit def iterOnceToControlStructureTrav[A <: ControlStructure](a: IterableOnce[A]): ControlStructureTraversal =
    new ControlStructureTraversal(a.iterator)

  implicit def singleToIdentifierTrav[A <: Identifier](a: A): IdentifierTraversal =
    new IdentifierTraversal(Iterator.single(a))
  implicit def iterOnceToIdentifierTrav[A <: Identifier](a: IterableOnce[A]): IdentifierTraversal =
    new IdentifierTraversal(a.iterator)

  implicit def singleToAnnotationTrav[A <: Annotation](a: A): AnnotationTraversal =
    new AnnotationTraversal(Iterator.single(a))
  implicit def iterOnceToAnnotationTrav[A <: Annotation](a: IterableOnce[A]): AnnotationTraversal =
    new AnnotationTraversal(a.iterator)

  implicit def singleToDependencyTrav[A <: Dependency](a: A): DependencyTraversal =
    new DependencyTraversal(Iterator.single(a))

  implicit def iterToDependencyTrav[A <: Dependency](a: IterableOnce[A]): DependencyTraversal =
    new DependencyTraversal(a.iterator)

  implicit def singleToAnnotationParameterAssignTrav[A <: AnnotationParameterAssign](
    a: A
  ): AnnotationParameterAssignTraversal =
    new AnnotationParameterAssignTraversal(Iterator.single(a))
  implicit def iterOnceToAnnotationParameterAssignTrav[A <: AnnotationParameterAssign](
    a: IterableOnce[A]
  ): AnnotationParameterAssignTraversal =
    new AnnotationParameterAssignTraversal(a.iterator)

  implicit def toMember(traversal: IterableOnce[Member]): MemberTraversal = new MemberTraversal(traversal.iterator)
  implicit def toLocal(traversal: IterableOnce[Local]): LocalTraversal    = new LocalTraversal(traversal.iterator)
  implicit def toMethod(traversal: IterableOnce[Method]): OriginalMethod  = new OriginalMethod(traversal.iterator)

  implicit def singleToMethodParameterInTrav[A <: MethodParameterIn](a: A): MethodParameterTraversal =
    new MethodParameterTraversal(Iterator.single(a))
  implicit def iterOnceToMethodParameterInTrav[A <: MethodParameterIn](a: IterableOnce[A]): MethodParameterTraversal =
    new MethodParameterTraversal(a.iterator)

  implicit def iterOnceToMethodParameterOutTrav[A <: MethodParameterOut](
    a: IterableOnce[A]
  ): MethodParameterOutTraversal =
    new MethodParameterOutTraversal(a.iterator)

  implicit def iterOnceToMethodReturnTrav[A <: MethodReturn](a: IterableOnce[A]): MethodReturnTraversal =
    new MethodReturnTraversal(a.iterator)

  implicit def singleToNamespaceTrav[A <: Namespace](a: A): NamespaceTraversal =
    new NamespaceTraversal(Iterator.single(a))
  implicit def iterOnceToNamespaceTrav[A <: Namespace](a: IterableOnce[A]): NamespaceTraversal =
    new NamespaceTraversal(a.iterator)

  implicit def singleToNamespaceBlockTrav[A <: NamespaceBlock](a: A): NamespaceBlockTraversal =
    new NamespaceBlockTraversal(Iterator.single(a))
  implicit def iterOnceToNamespaceBlockTrav[A <: NamespaceBlock](a: IterableOnce[A]): NamespaceBlockTraversal =
    new NamespaceBlockTraversal(a.iterator)

  implicit def singleToFileTrav[A <: File](a: A): FileTraversal =
    new FileTraversal(Iterator.single(a))
  implicit def iterOnceToFileTrav[A <: File](a: IterableOnce[A]): FileTraversal =
    new FileTraversal(a.iterator)

  implicit def singleToImportTrav[A <: Import](a: A): ImportTraversal =
    new ImportTraversal(Iterator.single(a))

  implicit def iterToImportTrav[A <: Import](a: IterableOnce[A]): ImportTraversal =
    new ImportTraversal(a.iterator)

  // Call graph extension
  implicit def singleToMethodTravCallGraphExt[A <: Method](a: A): MethodTraversal =
    new MethodTraversal(Iterator.single(a))
  implicit def iterOnceToMethodTravCallGraphExt[A <: Method](a: IterableOnce[A]): MethodTraversal =
    new MethodTraversal(a.iterator)
  implicit def singleToCallTrav[A <: Call](a: A): CallTraversal =
    new CallTraversal(Iterator.single(a))
  implicit def iterOnceToCallTrav[A <: Call](a: IterableOnce[A]): CallTraversal =
    new CallTraversal(a.iterator)
  // / Call graph extension

  // Binding extensions
  implicit def singleToBindingMethodTrav[A <: Method](a: A): BindingMethodTraversal =
    new BindingMethodTraversal(Iterator.single(a))
  implicit def iterOnceToBindingMethodTrav[A <: Method](a: IterableOnce[A]): BindingMethodTraversal =
    new BindingMethodTraversal(a.iterator)

  implicit def singleToBindingTypeDeclTrav[A <: TypeDecl](a: A): BindingTypeDeclTraversal =
    new BindingTypeDeclTraversal(Iterator.single(a))
  implicit def iterOnceToBindingTypeDeclTrav[A <: TypeDecl](a: IterableOnce[A]): BindingTypeDeclTraversal =
    new BindingTypeDeclTraversal(a.iterator)

  implicit def singleToCfgNodeDot[A <: Method](a: A): CfgNodeDot =
    new CfgNodeDot(Iterator.single(a))
  implicit def iterOnceToCfgNodeDot[A <: Method](a: IterableOnce[A]): CfgNodeDot =
    new CfgNodeDot(a.iterator)

  implicit def graphToInterproceduralDot(cpg: Cpg): InterproceduralNodeDot =
    new InterproceduralNodeDot(cpg)

  /** Warning: implicitly lifting `Node -> Traversal` opens a broad space with a lot of accidental complexity and is
    * considered a historical accident. We only keep it around because we want to preserve `reachableBy(Node*)`, which
    * unfortunately (due to type erasure) can't be an overload of `reachableBy(Traversal*)`.
    *
    * In most places you should explicitly call `Iterator.single` instead of relying on this implicit.
    */
  implicit def toTraversal[NodeType <: StoredNode](node: NodeType): Iterator[NodeType] =
    Iterator.single(node)

  implicit def iterableOnceToSteps[A](iterableOnce: IterableOnce[A]): Steps[A] =
    new Steps(iterableOnce.iterator)

  implicit def traversalToSteps[A](trav: Iterator[A]): Steps[A] =
    new Steps(trav)
  implicit def iterOnceToNodeSteps[A <: StoredNode](a: IterableOnce[A]): NodeSteps[A] =
    new NodeSteps[A](a.iterator)

  implicit def toNewNodeTrav[NodeType <: NewNode](trav: Iterator[NodeType]): NewNodeSteps[NodeType] =
    new NewNodeSteps[NodeType](trav)

  implicit def toNodeTypeStarters(cpg: Cpg): NodeTypeStarters    = new NodeTypeStarters(cpg)
  implicit def toTagTraversal(trav: Iterator[Tag]): TagTraversal = new TagTraversal(trav)

  // ~ EvalType accessors
  implicit def singleToEvalTypeAccessorsLocal[A <: Local](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Iterator.single(a))
  implicit def iterOnceToEvalTypeAccessorsLocal[A <: Local](a: IterableOnce[A]): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](a.iterator)

  implicit def singleToEvalTypeAccessorsMember[A <: Member](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Iterator.single(a))
  implicit def iterOnceToEvalTypeAccessorsMember[A <: Member](a: IterableOnce[A]): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](a.iterator)

  implicit def singleToEvalTypeAccessorsMethod[A <: Method](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Iterator.single(a))
  implicit def iterOnceToEvalTypeAccessorsMethod[A <: Method](a: IterableOnce[A]): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](a.iterator)

  implicit def singleToEvalTypeAccessorsParameterIn[A <: MethodParameterIn](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Iterator.single(a))
  implicit def iterOnceToEvalTypeAccessorsParameterIn[A <: MethodParameterIn](
    a: IterableOnce[A]
  ): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](a.iterator)

  implicit def singleToEvalTypeAccessorsParameterOut[A <: MethodParameterOut](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Iterator.single(a))
  implicit def iterOnceToEvalTypeAccessorsParameterOut[A <: MethodParameterOut](
    a: IterableOnce[A]
  ): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](a.iterator)

  implicit def singleToEvalTypeAccessorsMethodReturn[A <: MethodReturn](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Iterator.single(a))
  implicit def iterOnceToEvalTypeAccessorsMethodReturn[A <: MethodReturn](a: IterableOnce[A]): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](a.iterator)

  implicit def singleToEvalTypeAccessorsExpression[A <: Expression](a: A): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](Iterator.single(a))
  implicit def iterOnceToEvalTypeAccessorsExpression[A <: Expression](a: IterableOnce[A]): EvalTypeAccessors[A] =
    new EvalTypeAccessors[A](a.iterator)

  // EvalType accessors ~

  // ~ Modifier accessors
  implicit def singleToModifierAccessorsMember[A <: Member](a: A): ModifierAccessors[A] =
    new ModifierAccessors[A](Iterator.single(a))
  implicit def iterOnceToModifierAccessorsMember[A <: Member](a: IterableOnce[A]): ModifierAccessors[A] =
    new ModifierAccessors[A](a.iterator)

  implicit def singleToModifierAccessorsMethod[A <: Method](a: A): ModifierAccessors[A] =
    new ModifierAccessors[A](Iterator.single(a))
  implicit def iterOnceToModifierAccessorsMethod[A <: Method](a: IterableOnce[A]): ModifierAccessors[A] =
    new ModifierAccessors[A](a.iterator)

  implicit def singleToModifierAccessorsTypeDecl[A <: TypeDecl](a: A): ModifierAccessors[A] =
    new ModifierAccessors[A](Iterator.single(a))
  implicit def iterOnceToModifierAccessorsTypeDecl[A <: TypeDecl](a: IterableOnce[A]): ModifierAccessors[A] =
    new ModifierAccessors[A](a.iterator)
  // Modifier accessors ~

  implicit class NewNodeTypeDeco[NodeType <: NewNode](val node: NodeType) extends AnyVal {

    /** Start a new traversal from this node
      */
    def start: Iterator[NodeType] =
      Iterator.single(node)
  }

  implicit def toExpression[A <: Expression](a: IterableOnce[A]): ExpressionTraversal[A] =
    new ExpressionTraversal[A](a.iterator)

  object NonStandardImplicits {

    // note: this causes problems because MethodParameterOut has an `index` property and the `MethodParameterOutTraversal` defines an `index` step...
    implicit def singleToMethodParameterOutTrav[A <: MethodParameterOut](a: A): MethodParameterOutTraversal =
      new MethodParameterOutTraversal(Iterator.single(a))

  }

  implicit def singleToSarifTraversal[A <: Finding](a: A): SarifExtension = new SarifExtension(Iterator.single(a))
  implicit def iterOnceToSarifTraversal[A <: Finding](a: IterableOnce[A]): SarifExtension = new SarifExtension(a)
}

trait LowPrioImplicits {
  implicit val docSearchPackages: DocSearchPackages =
    Cpg.defaultDocSearchPackage
      .withAdditionalPackage("io.joern")
      .withAdditionalPackage("io.shiftleft")

  implicit def singleToAstNodeDot[A <: AstNode](a: A): AstNodeDot[A] =
    new AstNodeDot(Iterator.single(a))
  implicit def iterOnceToAstNodeDot[A <: AstNode](a: IterableOnce[A]): AstNodeDot[A] =
    new AstNodeDot(a.iterator)

  implicit def toCfgNodeMethods(node: CfgNode): CfgNodeMethods = new CfgNodeMethods(node)

  implicit def iterOnceToCfgNodeTraversal[A <: CfgNode](a: IterableOnce[A]): CfgNodeTraversal[A] =
    new CfgNodeTraversal[A](a.iterator)

  implicit def singleToAstNodeTraversal[A <: AstNode](a: A): AstNodeTraversal[A] =
    new AstNodeTraversal[A](Iterator.single(a))
  implicit def iterOnceToAstNodeTraversal[A <: AstNode](a: IterableOnce[A]): AstNodeTraversal[A] =
    new AstNodeTraversal[A](a.iterator)

  implicit def singleToDeclarationNodeTraversal[A <: Declaration](a: A): DeclarationTraversal[A] =
    new DeclarationTraversal[A](Iterator.single(a))
  implicit def iterOnceToDeclarationNodeTraversal[A <: Declaration](a: IterableOnce[A]): DeclarationTraversal[A] =
    new DeclarationTraversal[A](a.iterator)

}
