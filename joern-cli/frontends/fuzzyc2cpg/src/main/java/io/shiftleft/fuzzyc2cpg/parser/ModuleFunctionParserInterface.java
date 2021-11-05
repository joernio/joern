package io.shiftleft.fuzzyc2cpg.parser;

import io.shiftleft.fuzzyc2cpg.ModuleParser.Compound_statementContext;
import io.shiftleft.fuzzyc2cpg.ModuleParser.Function_defContext;
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import io.shiftleft.fuzzyc2cpg.parser.functions.AntlrCFunctionParserDriver;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.misc.Interval;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ModuleFunctionParserInterface {

  private static Logger logger = LoggerFactory.getLogger(ModuleFunctionParserInterface.class);

  // Extracts compound statement from input stream
  // as a string and passes that string to the
  // function parser. The resulting 'CompoundStatement'
  // (an AST node) is returned.

  public static CompoundStatement parseFunctionContents(
      Function_defContext ctx) {
    String text = getCompoundStmtAsString(ctx);

    AntlrCFunctionParserDriver driver = new AntlrCFunctionParserDriver();

    try {
      driver.parseAndWalkString(text);
    } catch (RuntimeException ex) {
      logger.warn(ctx.function_name().getText() + " was skipped."/*, ex*/);
    }
    CompoundStatement result = driver.getResult();
    Compound_statementContext statementContext = ctx.compound_statement();
    AstNodeFactory.initializeFromContext(result, statementContext);
    return result;
  }

  private static String getCompoundStmtAsString(
      Function_defContext ctx) {
    Compound_statementContext compound_statement = ctx.compound_statement();

    CharStream inputStream = compound_statement.start.getInputStream();

    int startIndex = compound_statement.start.getStopIndex();
    int stopIndex = compound_statement.stop.getStopIndex();

    // We insert new lines to compensate for the missing code previous to the function.
    // This gives us correct line numbers for the function and its content.
    return StringUtils.repeat("\n",null, ctx.compound_statement().start.getLine() - 1)
            + inputStream.getText(new Interval(startIndex + 1, stopIndex - 1));
  }

}
