package io.joern.rust2cpg.astcreation

import io.joern.rust2cpg.parser.RustNodeSyntax
import io.joern.rust2cpg.parser.RustNodeSyntax.RustNode
import io.joern.rust2cpg.parser.RustNodeSyntaxExtensions.value
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewNamespaceBlock}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

// Computes rust-style full names, e.g. `crate::module::item`.
// rust_ast_gen-provided methodFullName/typeFullName are always preferred.
// The fallback is to rely on the current enclosing scope.
trait RustFullNames { this: AstCreator =>
  import RustFullNames.PathSep

  protected def combineRustFullName(parent: String, child: String): String = {
    s"$parent$PathSep$child"
  }

  protected def traitImplFullName(implTypeFullName: String, traitTypeFullName: String): String = {
    s"<$implTypeFullName as $traitTypeFullName>"
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

  protected def methodFullNameForCallExpr(
    callExpr: RustNodeSyntax.CallExpr,
    nameRefs: Seq[RustNodeSyntax.NameRef],
    name: String
  ): String = {
    callExpr.methodFullName.getOrElse {
      // TODO: take into account `use` (imports). We are incorrectly assuming here that an
      //  unresolved call e.g. `a::b::c()` has fullName `a::b::c`.
      if (nameRefs.size > 1) {
        nameRefs.map(code).reduce(combineRustFullName)
      } else {
        combineRustFullName(Defines.UnresolvedNamespace, name)
      }
    }
  }

  protected def methodFullNameForMethodCallExpr(methodCallExpr: RustNodeSyntax.MethodCallExpr): String = {
    methodCallExpr.methodFullName.getOrElse(
      combineRustFullName(Defines.UnresolvedNamespace, code(methodCallExpr.nameRef))
    )
  }

  protected def typeFullNameForType(typ: RustNodeSyntax.Type): String = {
    typ match {
      case pathType: RustNodeSyntax.PathType =>
        typeFullNameForPath(pathType.path)
      case refType: RustNodeSyntax.RefType =>
        val mut = Option.when(refType.mutKwToken.isDefined)("mut ").getOrElse("")
        s"&$mut${typeFullNameForType(refType.typ)}"
      case ptrType: RustNodeSyntax.PtrType =>
        val qualifier = if (ptrType.constKwToken.isDefined) "const " else "mut "
        s"*$qualifier${typeFullNameForType(ptrType.typ)}"
      case sliceType: RustNodeSyntax.SliceType =>
        s"[${typeFullNameForType(sliceType.typ)}]"
      case arrayType: RustNodeSyntax.ArrayType =>
        s"[${typeFullNameForType(arrayType.typ)}; ${text(arrayType.constArg).getOrElse("")}]"
      case tupleType: RustNodeSyntax.TupleType =>
        s"(${tupleType.typ.map(typeFullNameForType).mkString(", ")})"
      case parenType: RustNodeSyntax.ParenType =>
        typeFullNameForType(parenType.typ)
      case _: RustNodeSyntax.NeverType =>
        "!"
      case macroType: RustNodeSyntax.MacroType =>
        macroType.macroCall.macroExpansion match {
          case Some(expanded: RustNodeSyntax.Type) => typeFullNameForType(expanded)
          case _                                   => text(typ).getOrElse(Defines.Any)
        }

      // TODO: the following are not handled yet.
      case io.joern.rust2cpg.parser.RustNodeSyntax.DynTraitType(_) =>
        text(typ).getOrElse(Defines.Any)
      case io.joern.rust2cpg.parser.RustNodeSyntax.FnPtrType(_) =>
        text(typ).getOrElse(Defines.Any)
      case io.joern.rust2cpg.parser.RustNodeSyntax.ForType(_) =>
        text(typ).getOrElse(Defines.Any)
      case io.joern.rust2cpg.parser.RustNodeSyntax.ImplTraitType(_) =>
        text(typ).getOrElse(Defines.Any)
      case io.joern.rust2cpg.parser.RustNodeSyntax.InferType(_) =>
        // TODO(rust_ast_gen): is this typeFullName missing on purpose or by accident?
        //  This corresponds to `_` in something like `let x: Vec<_>`. We currently don't have a
        //  typeFullName for it, but we have for `x`.
        text(typ).getOrElse(Defines.Any)
    }
  }

  protected def typeFullNameForNameRef(nameRef: RustNodeSyntax.NameRef): String = {
    nameRef.typeFullName.getOrElse(Defines.Any)
  }

  protected def typeFullNameForPath(path: RustNodeSyntax.Path): String = {
    // In a path, only the leaf (NameRef) has typeFullName set (by rust_ast_gen).
    path.pathSegment.nameRef.flatMap(_.typeFullName).getOrElse(Defines.Any)
  }

  protected def typeFullNameForExpr(expr: RustNodeSyntax.Expr): String = {
    expr.typeFullName.getOrElse(Defines.Any)
  }

  protected def typeFullNameForSelfParam(selfParam: RustNodeSyntax.SelfParam): String = {
    selfParam.typeFullName.getOrElse {
      val enclosingType = enclosingTypeDeclFullName.getOrElse(Defines.Any)
      selfParam.typ match {
        case Some(typ) => typeFullNameForType(typ)
        case None if selfParam.ampToken.isDefined =>
          val mut = Option.when(selfParam.mutKwToken.isDefined)("mut ").getOrElse("")
          s"&$mut$enclosingType"
        case None => enclosingType
      }
    }
  }

  protected def typeFullNameForTupleExpr(tupleExpr: RustNodeSyntax.TupleExpr): String = {
    tupleExpr.typeFullName.getOrElse {
      val childTypes = tupleExpr.expr.map(typeFullNameForExpr)
      s"(${childTypes.mkString(", ")})"
    }
  }

  protected def typeFullNameForLiteral(lit: RustNodeSyntax.Literal): String = {
    lit.typeFullName.orElse(lit.value.map(typeFullNameForLiteralToken)).getOrElse(Defines.Any)
  }

  protected def typeFullNameForIdentPat(identPat: RustNodeSyntax.IdentPat): String = {
    identPat.typeFullName.getOrElse(Defines.Any)
  }

  protected def typeFullNameForPat(pat: RustNodeSyntax.Pat): String = {
    // TODO(rust_ast_gen): add typeFullName to patterns.
    pat.typeFullName.getOrElse {
      pat match {
        case tuplePat: RustNodeSyntax.TuplePat =>
          val childTypes = tuplePat.pat.map(typeFullNameForPat)
          s"(${childTypes.mkString(", ")})"
        case _ => Defines.Any
      }
    }
  }

  private def typeFullNameForLiteralToken(tok: RustNodeSyntax.RustToken): String = tok match {
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

object RustFullNames {
  val PathSep = "::"
}
