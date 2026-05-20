package io.joern.rust2cpg.astcreation

import io.joern.rust2cpg.parser.RustNodeSyntax
import io.joern.rust2cpg.parser.RustNodeSyntax.RustNode
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewNamespaceBlock}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

// Computes rust-style full names, e.g. `crate::module::item`.
// rust_ast_gen-provided methodFullName/typeFullName are always preferred.
// The fallback is to rely on the current enclosing scope.
trait RustFullNames { this: AstCreator =>

  private val PathSep = "::"

  protected def combineRustFullName(parent: String, child: String): String = {
    s"$parent$PathSep$child"
  }

  private def crateName: String = {
    parseResult.crateName.getOrElse(Defines.UnresolvedNamespace)
  }

  protected def rustNamespaceFullName: String = parseResult.modulePath match {
    case Some(path) => combineRustFullName(crateName, path)
    case None       => crateName
  }

  protected def composeRustFullName(name: String): String = {
    combineRustFullName(rustParentFullName, name)
  }

  private def rustParentFullName: String = {
    // We don't want to have the "global method"'s fullname propagated since that
    // would not match rust_ast_gen's full names.
    val parent = methodAstParentStack.find {
      case method: NewMethod if method.name == NamespaceTraversal.globalNamespaceName => false
      case _                                                                          => true
    }.get
    // When the parent is a namespace_block, we don't want its fullName since it's
    // file-prefix in order to be unique, but rather its name. For other nodes,
    // we do want the fullName.
    parent match {
      case ns: NewNamespaceBlock => ns.name
      case other                 => other.properties(PropertyNames.FullName).toString
    }
  }

  private def rustAstGenMethodFullName(node: RustNode): Option[String] = {
    // TODO: move this key into a constant in RustNodeSyntax, or provide the accessor there.
    node.json.obj.get("methodFullName").flatMap(_.strOpt)
  }

  private def rustAstGenTypeFullName(node: RustNode): Option[String] = {
    // TODO: move this key into a constant in RustNodeSyntax, or provide the accessor there.
    node.json.obj.get("typeFullName").flatMap(_.strOpt)
  }

  protected def methodFullNameForCallExpr(
    callExpr: RustNodeSyntax.CallExpr,
    nameRefs: Seq[RustNodeSyntax.NameRef]
  ): String = {
    rustAstGenMethodFullName(callExpr).getOrElse {
      // TODO: take into account `use` (imports). We are incorrectly assuming here that an
      //  unresolved call e.g. `a::b::c()` has fullName `a::b::c`.
      nameRefs match {
        case name :: Nil => combineRustFullName(Defines.UnresolvedNamespace, code(name))
        case names       => names.map(code).mkString(PathSep)
      }
    }
  }

  protected def methodFullNameForMethodCallExpr(methodCallExpr: RustNodeSyntax.MethodCallExpr): String = {
    rustAstGenMethodFullName(methodCallExpr).getOrElse(Defines.DynamicCallUnknownFullName)
  }

  // TODO
  protected def typeFullNameForType(typ: RustNodeSyntax.Type): String = {
    text(typ).getOrElse(Defines.Any)
  }

  // TODO
  protected def typeFullNameForNameRef(nameRef: RustNodeSyntax.NameRef): String = {
    Defines.Any
  }

  protected def typeFullNameForPath(path: RustNodeSyntax.Path): String = {
    rustAstGenTypeFullName(path)
      .orElse(path.pathSegment.nameRef.flatMap(rustAstGenTypeFullName))
      .getOrElse(Defines.Any)
  }

  protected def typeFullNameForExpr(expr: RustNodeSyntax.Expr): String = {
    rustAstGenTypeFullName(expr).getOrElse(Defines.Any)
  }

  protected def typeFullNameForTupleExpr(tupleExpr: RustNodeSyntax.TupleExpr): String = {
    rustAstGenTypeFullName(tupleExpr).getOrElse {
      val childTypes = tupleExpr.expr.map(typeFullNameForExpr)
      s"(${childTypes.mkString(", ")})"
    }
  }

  protected def typeFullNameForLiteral(lit: RustNodeSyntax.Literal): String = {
    rustAstGenTypeFullName(lit).orElse(lit.value.map(typeFullNameForLiteralToken)).getOrElse(Defines.Any)
  }

  protected def typeFullNameForIdentPat(identPat: RustNodeSyntax.IdentPat): String = {
    rustAstGenTypeFullName(identPat).getOrElse(Defines.Any)
  }

  protected def typeFullNameForLiteralToken(tok: RustNodeSyntax.RustToken): String = tok match {
    case _: RustNodeSyntax.IntNumberToken   => "i32"
    case _: RustNodeSyntax.FloatNumberToken => "f64"
    case _: RustNodeSyntax.StringToken      => "&str"
    case _: RustNodeSyntax.ByteStringToken  => "&[u8]"
    case _: RustNodeSyntax.CStringToken     => "&CStr"
    case _: RustNodeSyntax.CharToken        => "char"
    case _: RustNodeSyntax.ByteToken        => "u8"
    case _: RustNodeSyntax.TrueKwToken      => "bool"
    case _: RustNodeSyntax.FalseKwToken     => "bool"
    case _                                  => Defines.Any
  }

}
