package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines

import scala.annotation.unused

trait AstForSwiftTokenCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private def astForArrowToken(@unused node: arrow): Ast                       = Ast()
  private def astForAtSignToken(@unused node: atSign): Ast                     = Ast()
  private def astForBackslashToken(@unused node: backslash): Ast               = Ast()
  private def astForBacktickToken(@unused node: backtick): Ast                 = Ast()
  private def astForBinaryOperatorToken(@unused node: binaryOperator): Ast     = Ast()
  private def astForColonToken(@unused node: colon): Ast                       = Ast()
  private def astForCommaToken(@unused node: comma): Ast                       = Ast()
  private def astForDollarIdentifierToken(@unused node: dollarIdentifier): Ast = Ast()
  private def astForEllipsisToken(@unused node: ellipsis): Ast                 = Ast()
  private def astForEndOfFileToken(@unused node: endOfFile): Ast               = Ast()
  private def astForEqualToken(@unused node: equal): Ast                       = Ast()
  private def astForExclamationMarkToken(@unused node: exclamationMark): Ast   = Ast()

  private def astForFloatLiteralToken(node: floatLiteral): Ast = {
    Ast(literalNode(node, code(node), Option(Defines.Float)))
  }

  private def astForIdentifierToken(node: identifier): Ast = {
    val name = code(node)
    Ast(identifierNode(node, name, name, Defines.Any))
  }

  private def astForInfixQuestionMarkToken(@unused node: infixQuestionMark): Ast = Ast()

  protected def astForIntegerLiteralToken(node: integerLiteral): Ast = {
    Ast(literalNode(node, code(node), Option(Defines.Int)))
  }

  private def astForKeywordToken(node: keyword): Ast = {
    val code = this.code(node)
    code match
      case "true" | "false" => Ast(literalNode(node, code, Option(Defines.Bool)))
      case _                => notHandledYet(node)
  }

  private def astForLeftAngleToken(@unused node: leftAngle): Ast                             = Ast()
  private def astForLeftBraceToken(@unused node: leftBrace): Ast                             = Ast()
  private def astForLeftParenToken(@unused node: leftParen): Ast                             = Ast()
  private def astForLeftSquareToken(@unused node: leftSquare): Ast                           = Ast()
  private def astForMultilineStringQuoteToken(@unused node: multilineStringQuote): Ast       = Ast()
  private def astForPeriodToken(@unused node: period): Ast                                   = Ast()
  private def astForPostfixOperatorToken(@unused node: postfixOperator): Ast                 = Ast()
  private def astForPostfixQuestionMarkToken(@unused node: postfixQuestionMark): Ast         = Ast()
  private def astForPoundToken(@unused node: pound): Ast                                     = Ast()
  private def astForPoundAvailableToken(@unused node: poundAvailable): Ast                   = Ast()
  private def astForPoundElseToken(@unused node: poundElse): Ast                             = Ast()
  private def astForPoundElseifToken(@unused node: poundElseif): Ast                         = Ast()
  private def astForPoundEndifToken(@unused node: poundEndif): Ast                           = Ast()
  private def astForPoundIfToken(@unused node: poundIf): Ast                                 = Ast()
  private def astForPoundSourceLocationToken(@unused node: poundSourceLocation): Ast         = Ast()
  private def astForPoundUnavailableToken(@unused node: poundUnavailable): Ast               = Ast()
  private def astForPrefixAmpersandToken(@unused node: prefixAmpersand): Ast                 = Ast()
  private def astForPrefixOperatorToken(@unused node: prefixOperator): Ast                   = Ast()
  private def astForRawStringPoundDelimiterToken(@unused node: rawStringPoundDelimiter): Ast = Ast()
  private def astForRegexLiteralPatternToken(@unused node: regexLiteralPattern): Ast         = Ast()
  private def astForRegexPoundDelimiterToken(@unused node: regexPoundDelimiter): Ast         = Ast()
  private def astForRegexSlashToken(@unused node: regexSlash): Ast                           = Ast()
  private def astForRightAngleToken(@unused node: rightAngle): Ast                           = Ast()
  private def astForRightBraceToken(@unused node: rightBrace): Ast                           = Ast()
  private def astForRightParenToken(@unused node: rightParen): Ast                           = Ast()
  private def astForRightSquareToken(@unused node: rightSquare): Ast                         = Ast()
  private def astForSemicolonToken(@unused node: semicolon): Ast                             = Ast()
  private def astForshebangToken(@unused node: shebang): Ast                                 = Ast()
  private def astForSingleQuoteToken(@unused node: singleQuote): Ast                         = Ast()
  private def astForStringQuoteToken(@unused node: stringQuote): Ast                         = Ast()
  private def astForStringSegmentToken(@unused node: stringSegment): Ast                     = Ast()
  private def astForUnknownToken(@unused node: unknown): Ast                                 = Ast()
  private def astForWildcardToken(@unused node: wildcard): Ast                               = Ast()

  protected def astForSwiftToken(swiftToken: SwiftToken): Ast = swiftToken match {
    case node: arrow                   => astForArrowToken(node)
    case node: atSign                  => astForAtSignToken(node)
    case node: backslash               => astForBackslashToken(node)
    case node: backtick                => astForBacktickToken(node)
    case node: binaryOperator          => astForBinaryOperatorToken(node)
    case node: colon                   => astForColonToken(node)
    case node: comma                   => astForCommaToken(node)
    case node: dollarIdentifier        => astForDollarIdentifierToken(node)
    case node: ellipsis                => astForEllipsisToken(node)
    case node: endOfFile               => astForEndOfFileToken(node)
    case node: equal                   => astForEqualToken(node)
    case node: exclamationMark         => astForExclamationMarkToken(node)
    case node: floatLiteral            => astForFloatLiteralToken(node)
    case node: identifier              => astForIdentifierToken(node)
    case node: infixQuestionMark       => astForInfixQuestionMarkToken(node)
    case node: integerLiteral          => astForIntegerLiteralToken(node)
    case node: keyword                 => astForKeywordToken(node)
    case node: leftAngle               => astForLeftAngleToken(node)
    case node: leftBrace               => astForLeftBraceToken(node)
    case node: leftParen               => astForLeftParenToken(node)
    case node: leftSquare              => astForLeftSquareToken(node)
    case node: multilineStringQuote    => astForMultilineStringQuoteToken(node)
    case node: period                  => astForPeriodToken(node)
    case node: postfixOperator         => astForPostfixOperatorToken(node)
    case node: postfixQuestionMark     => astForPostfixQuestionMarkToken(node)
    case node: pound                   => astForPoundToken(node)
    case node: poundAvailable          => astForPoundAvailableToken(node)
    case node: poundElse               => astForPoundElseToken(node)
    case node: poundElseif             => astForPoundElseifToken(node)
    case node: poundEndif              => astForPoundEndifToken(node)
    case node: poundIf                 => astForPoundIfToken(node)
    case node: poundSourceLocation     => astForPoundSourceLocationToken(node)
    case node: poundUnavailable        => astForPoundUnavailableToken(node)
    case node: prefixAmpersand         => astForPrefixAmpersandToken(node)
    case node: prefixOperator          => astForPrefixOperatorToken(node)
    case node: rawStringPoundDelimiter => astForRawStringPoundDelimiterToken(node)
    case node: regexLiteralPattern     => astForRegexLiteralPatternToken(node)
    case node: regexPoundDelimiter     => astForRegexPoundDelimiterToken(node)
    case node: regexSlash              => astForRegexSlashToken(node)
    case node: rightAngle              => astForRightAngleToken(node)
    case node: rightBrace              => astForRightBraceToken(node)
    case node: rightParen              => astForRightParenToken(node)
    case node: rightSquare             => astForRightSquareToken(node)
    case node: semicolon               => astForSemicolonToken(node)
    case node: shebang                 => astForshebangToken(node)
    case node: singleQuote             => astForSingleQuoteToken(node)
    case node: stringQuote             => astForStringQuoteToken(node)
    case node: stringSegment           => astForStringSegmentToken(node)
    case node: unknown                 => astForUnknownToken(node)
    case node: wildcard                => astForWildcardToken(node)
  }

}
