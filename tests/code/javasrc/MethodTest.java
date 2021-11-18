package io.shiftleft.java2cpg.testcode.methods;

import java.util.ArrayList;
import java.util.List;

public class MethodTest {

  public static int staticMethod(int var) {
    return var;
  }

  public int nonStaticMethod(int var) {
    return var;
  }

  public int annotatedFunction(int var) {
    return var;
  }

  public void lambdaUser() {
    int capturedValue = 3;
    List<Integer> someStream = new ArrayList<>();
    someStream.stream().map(x -> capturedValue + x);
  }

  public String callsExternalMethod() {
    return super.toString();
  }

  private String internalMethod() {
    return "";
  }

  public String callsNoExternalMethod() {
    return internalMethod();
  }

}
