package io.shiftleft.fuzzyc2cpg.parser;

import org.antlr.v4.runtime.tree.ParseTree;

public class ParseTreeUtils {

  public static String childTokenString(ParseTree ctx) {

    // The reason we don't just call ctx.getText()
    // here is because it removes whitespace, making
    // 'inti' from 'int i'.

    if (ctx == null) {
      return "";
    }

    int numChildren = ctx.getChildCount();

    if (numChildren == 0) {
      return ctx.getText();
    }

    StringBuilder retval = new StringBuilder();

    for (int i = 0; i < numChildren; i++) {
      ParseTree child = ctx.getChild(i);
      String childText = childTokenString(child);
      if (!childText.equals("")) {
        retval.append(childText).append(" ");
      }
    }

    if (retval.length() > 0) {
      return retval.substring(0, retval.length() - 1);
    }
    return retval.toString();
  }
}
