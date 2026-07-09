package io.joern.rust2cpg.parser

import io.joern.rust2cpg.parser.RustNodeSyntax.*

// This mainly exists because rust_ast_gen doesn't codegen accessors for labelled sub-items.
// So we implement them as needed here.
object RustNodeSyntaxExtensions {

  extension (rangeExpr: RangeExpr) {
    def isInclusive: Boolean  = rangeExpr.dot2eqToken.isDefined
    def op: Option[RustToken] = rangeExpr.dot2Token.orElse(rangeExpr.dot2eqToken)
    def start: Option[Expr]   = operand(_ < _)
    def end: Option[Expr]     = operand(_ > _)
    private def operand(compareWithOpOffset: (Int, Int) => Boolean): Option[Expr] = {
      op.flatMap(_.startOffset).flatMap { opOffset =>
        rangeExpr.expr.find(_.startOffset.exists(compareWithOpOffset(_, opOffset)))
      }
    }
  }

  extension (lit: Literal) {
    def value: Option[RustToken] =
      lit.intNumberToken
        .orElse(lit.floatNumberToken)
        .orElse(lit.stringToken)
        .orElse(lit.byteStringToken)
        .orElse(lit.cStringToken)
        .orElse(lit.charToken)
        .orElse(lit.byteToken)
        .orElse(lit.trueKwToken)
        .orElse(lit.falseKwToken)
  }

  extension (nameRef: NameRef) {
    def token: Option[RustToken] =
      nameRef.identToken
        .orElse(nameRef.intNumberToken)
        .orElse(nameRef.selfKwToken)
        .orElse(nameRef.superKwToken)
        .orElse(nameRef.crateKwToken)
        .orElse(nameRef.selfTypeKwToken)
  }

  extension (binExpr: BinExpr) {
    def op: Option[RustToken] =
      binExpr.pipe2Token
        .orElse(binExpr.amp2Token)
        .orElse(binExpr.eq2Token)
        .orElse(binExpr.neqToken)
        .orElse(binExpr.lteqToken)
        .orElse(binExpr.gteqToken)
        .orElse(binExpr.lAngleToken)
        .orElse(binExpr.rAngleToken)
        .orElse(binExpr.plusToken)
        .orElse(binExpr.starToken)
        .orElse(binExpr.minusToken)
        .orElse(binExpr.slashToken)
        .orElse(binExpr.percentToken)
        .orElse(binExpr.shlToken)
        .orElse(binExpr.shrToken)
        .orElse(binExpr.caretToken)
        .orElse(binExpr.pipeToken)
        .orElse(binExpr.ampToken)
        .orElse(binExpr.eqToken)
        .orElse(binExpr.pluseqToken)
        .orElse(binExpr.slasheqToken)
        .orElse(binExpr.stareqToken)
        .orElse(binExpr.percenteqToken)
        .orElse(binExpr.shreqToken)
        .orElse(binExpr.shleqToken)
        .orElse(binExpr.minuseqToken)
        .orElse(binExpr.pipeeqToken)
        .orElse(binExpr.ampeqToken)
        .orElse(binExpr.careteqToken)
  }

  extension (prefixExpr: PrefixExpr) {
    def op: Option[RustToken] =
      prefixExpr.minusToken
        .orElse(prefixExpr.bangToken)
        .orElse(prefixExpr.starToken)
  }

  extension (ifExpr: IfExpr) {
    def thenBranch: BlockExpr = {
      ifExpr.blockExpr.head
    }

    def elseBranch: Option[IfExpr | BlockExpr] = {
      if (ifExpr.ifExpr.isDefined) {
        ifExpr.ifExpr
      } else if (ifExpr.blockExpr.sizeIs > 1) {
        Some(ifExpr.blockExpr.last)
      } else
        None
    }
  }

  extension (indexExpr: IndexExpr) {
    def base: Expr  = indexExpr.expr.head
    def index: Expr = indexExpr.expr.last
  }

}
