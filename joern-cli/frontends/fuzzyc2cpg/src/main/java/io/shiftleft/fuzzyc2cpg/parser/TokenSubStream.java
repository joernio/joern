package io.shiftleft.fuzzyc2cpg.parser;

import java.util.Stack;
import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenSource;

public class TokenSubStream extends CommonTokenStream {

  private int stopIndex = -1;
  private int startIndex = 0;

  private Stack<Integer> stopIndexStack = new Stack<Integer>();
  private Stack<Integer> startIndexStack = new Stack<Integer>();

  public TokenSubStream(TokenSource tokenSource) {
    super(tokenSource);
  }

  public void restrict(int aStartIndex, int aStopIndex) {
    startIndexStack.push(index());
    stopIndexStack.push(stopIndex);

    startIndex = aStartIndex;
    stopIndex = aStopIndex;
    seek(aStartIndex);
  }

  public void resetRestriction() {
    stopIndex = stopIndexStack.pop();
    startIndex = startIndexStack.pop();
    seek(startIndex);
  }

  @Override
  @SuppressWarnings("deprecation")
  public void reset() {
    seek(startIndex);
  }

  @Override
  public Token LT(int k) {
    int newIdx = p + k - 1;
    if (stopIndex != -1 && newIdx >= stopIndex) {
      sync(newIdx);
      return tokens.get(tokens.size() - 1);
    } else {
      return super.LT(k);
    }
  }

}
