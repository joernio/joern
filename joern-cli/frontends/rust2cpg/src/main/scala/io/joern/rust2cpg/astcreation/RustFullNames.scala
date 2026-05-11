package io.joern.rust2cpg.astcreation

import io.joern.rust2cpg.parser.RustNodeSyntax
import io.joern.rust2cpg.parser.RustNodeSyntax.RustNode
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.PropertyNames

// Computes rust-style full names, e.g. `crate::module::item`.
// ast_gen provided methodFullName/typeFullName are always preferred.
// The fallback is to rely on the current enclosing scope.
trait RustFullNames { this: AstCreator =>

  protected def crateName: String = parseResult.crateName.getOrElse(Defines.Unknown)

  // `crate::module` when it's a submodule, or `crate` when the file is the crate root.
  protected def namespaceFullName: String = parseResult.modulePath match {
    case Some(path) if path.nonEmpty => s"$crateName::$path"
    case _                           => crateName
  }

  protected def composeFullName(name: String): String = {
    val parentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString
    s"$parentFullName::$name"
  }

  private def rustAstGenTypeFullName(node: RustNode): Option[String] = {
    node.json.obj.get("typeFullName").flatMap(_.strOpt)
  }

  private def rustAstGenMethodFullName(node: RustNode): Option[String] = {
    node.json.obj.get("methodFullName").flatMap(_.strOpt)
  }

  protected def typeFullNameForPath(path: RustNodeSyntax.Path): String = {
    rustAstGenTypeFullName(path)
      .orElse(path.pathSegment.nameRef.flatMap(rustAstGenTypeFullName))
      .getOrElse(Defines.Any)
  }

  protected def typeFullNameForExpr(expr: RustNodeSyntax.Expr): String = {
    rustAstGenTypeFullName(expr).getOrElse(Defines.Any)
  }

  protected def typeFullNameForLiteral(lit: RustNodeSyntax.Literal): String =
    rustAstGenTypeFullName(lit)
      .orElse(lit.value.map(typeFullNameForLiteralToken))
      .getOrElse(Defines.Any)

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

  protected def methodFullNameForCallExpr(
    callExpr: RustNodeSyntax.CallExpr,
    nameRefs: Seq[RustNodeSyntax.NameRef]
  ): String =
    rustAstGenMethodFullName(callExpr).getOrElse {
      nameRefs.map(code) match {
        case Nil   => Defines.Unknown
        case names => names.mkString("::")
      }
    }

  protected def methodFullNameForMethodCallExpr(methodCallExpr: RustNodeSyntax.MethodCallExpr): String = {
    rustAstGenMethodFullName(methodCallExpr).getOrElse(Defines.DynamicCallUnknownFullName)
  }
}
