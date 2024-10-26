package io.joern.dataflowengineoss

import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, PassThroughMapping, FullNameSemantics}
import io.shiftleft.codepropertygraph.generated.Operators

import scala.annotation.unused

object DefaultSemantics {

  /** @return
    *   a default set of common external procedure calls for all languages.
    */
  def apply(): FullNameSemantics = {
    val list = operatorFlows ++ cFlows ++ javaFlows
    FullNameSemantics.fromList(list)
  }

  private def F = (x: String, y: List[(Int, Int)]) => FlowSemantic.from(x, y)

  private def PTF(x: String, ys: List[(Int, Int)] = List.empty): FlowSemantic =
    FlowSemantic(x).copy(mappings = FlowSemantic.from(x, ys).mappings :+ PassThroughMapping)

  def operatorFlows: List[FlowSemantic] = List(
    F(Operators.addition, List((1, -1), (2, -1))),
    F(Operators.addressOf, List((1, -1))),
    F(Operators.assignment, List((2, 1), (2, -1))),
    F(Operators.assignmentAnd, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentArithmeticShiftRight, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentDivision, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentExponentiation, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentLogicalShiftRight, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentMinus, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentModulo, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentMultiplication, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentOr, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentPlus, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentShiftLeft, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentXor, List((2, 1), (1, 1), (2, -1))),
    F(Operators.cast, List((1, -1), (2, -1))),
    F(Operators.computedMemberAccess, List((1, -1))),
    F(Operators.conditional, List((2, -1), (3, -1))),
    F(Operators.elvis, List((1, -1), (2, -1))),
    F(Operators.notNullAssert, List((1, -1))),
    F(Operators.fieldAccess, List((1, -1))),
    F(Operators.getElementPtr, List((1, -1))),
    PTF(Operators.modulo, List.empty),
    PTF(Operators.arrayInitializer, List.empty),

    // TODO does this still exist?
    F("<operator>.incBy", List((1, 1), (2, 1), (3, 1), (4, 1))),
    F(Operators.indexAccess, List((1, -1))),
    F(Operators.indirectComputedMemberAccess, List((1, -1))),
    F(Operators.indirectFieldAccess, List((1, -1))),
    F(Operators.indirectIndexAccess, List((1, -1), (2, 1))),
    F(Operators.indirectMemberAccess, List((1, -1))),
    F(Operators.indirection, List((1, -1))),
    F(Operators.memberAccess, List((1, -1))),
    F(Operators.pointerShift, List((1, -1))),
    F(Operators.postDecrement, List((1, 1), (1, -1))),
    F(Operators.postIncrement, List((1, 1), (1, -1))),
    F(Operators.preDecrement, List((1, 1), (1, -1))),
    F(Operators.preIncrement, List((1, 1), (1, -1))),
    F(Operators.sizeOf, List.empty[(Int, Int)]),

    // Language specific operators
    PTF("<operator>.tupleLiteral"),
    PTF("<operator>.dictLiteral"),
    PTF("<operator>.setLiteral"),
    PTF("<operator>.listLiteral")
  )

  /** Semantic summaries for common external C/C++ calls.
    *
    * @see
    *   <a href="https://www.ibm.com/docs/en/i/7.3?topic=extensions-standard-c-library-functions-table-by-name">Standard
    *   C Library Functions</a>
    */
  def cFlows: List[FlowSemantic] = List(
    F("abs", List((1, 1), (1, -1))),
    F("abort", List.empty[(Int, Int)]),
    F("asctime", List((1, 1), (1, -1))),
    F("asctime_r", List((1, 1), (1, -1))),
    F("atof", List((1, 1), (1, -1))),
    F("atoi", List((1, 1), (1, -1))),
    F("atol", List((1, 1), (1, -1))),
    F("calloc", List((1, -1), (2, -1))),
    F("ceil", List((1, 1), (1, 1))),
    F("clock", List.empty[(Int, Int)]),
    F("ctime", List((1, -1))),
    F("ctime64", List((1, -1))),
    F("ctime_r", List((1, -1))),
    F("ctime64_r", List((1, -1))),
    F("difftime", List((1, -1), (2, -1))),
    F("difftime64", List((1, -1), (2, -1))),
    PTF("div"),
    F("exit", List((1, 1))),
    F("exp", List((1, -1))),
    F("fabs", List((1, -1))),
    F("fclose", List((1, 1), (1, -1))),
    F("fdopen", List((1, -1), (2, -1))),
    F("feof", List((1, 1), (1, -1))),
    F("ferror", List((1, 1), (1, -1))),
    F("fflush", List((1, 1), (1, -1))),
    F("fgetc", List((1, 1), (1, -1))),
    F("fwrite", List((1, 1), (1, -1), (2, -1), (3, -1), (4, -1))),
    F("free", List((1, 1))),
    F("getc", List((1, 1))),
    F("scanf", List((2, 2))),
    F("strcmp", List((1, 1), (1, -1), (2, 2), (2, -1))),
    F("strlen", List((1, 1), (1, -1))),
    F("strncpy", List((1, 1), (2, 2), (3, 3), (1, -1), (2, -1))),
    F("strncat", List((1, 1), (1, -1), (2, 2), (2, -1)))
  )

  /** Semantic summaries for common external Java calls.
    */
  def javaFlows: List[FlowSemantic] = List(
    PTF("java.lang.String.split:java.lang.String[](java.lang.String)", List((0, 0))),
    PTF("java.lang.String.split:java.lang.String[](java.lang.String,int)", List((0, 0))),
    PTF("java.lang.String.compareTo:int(java.lang.String)", List((0, 0))),
    F("java.io.PrintWriter.print:void(java.lang.String)", List((0, 0), (1, 1))),
    F("java.io.PrintWriter.println:void(java.lang.String)", List((0, 0), (1, 1))),
    F("java.io.PrintStream.println:void(java.lang.String)", List((0, 0), (1, 1))),
    PTF("java.io.PrintStream.print:void(java.lang.String)", List((0, 0))),
    F("android.text.TextUtils.isEmpty:boolean(java.lang.String)", List((0, -1), (1, -1))),
    F("java.sql.PreparedStatement.prepareStatement:java.sql.PreparedStatement(java.lang.String)", List((1, -1))),
    F("java.sql.PreparedStatement.prepareStatement:setDouble(int,double)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setFloat(int,float)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setInt(int,int)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setLong(int,long)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setShort(int,short)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setString(int,java.lang.String)", List((1, 1), (2, 2))),
    F("org.apache.http.HttpRequest.<init>:void(org.apache.http.RequestLine)", List((1, 1), (1, 0))),
    F("org.apache.http.HttpRequest.<init>:void(java.lang.String,java.lang.String)", List((1, 1), (1, 0), (2, 0))),
    F(
      "org.apache.http.HttpRequest.<init>:void(java.lang.String,java.lang.String,org.apache.http.ProtocolVersion)",
      List((1, 1), (1, 0), (2, 2), (2, 0), (3, 3), (3, 0))
    ),
    F("org.apache.http.HttpResponse.getStatusLine:org.apache.http.StatusLine()", List((0, -1))),
    F("org.apache.http.HttpResponse.setStatusLine:void(org.apache.http.StatusLine)", List((1, 0), (1, 1), (0, -1))),
    F("org.apache.http.HttpResponse.setReasonPhrase:void(java.lang.String)", List((1, 0), (1, 1), (0, -1))),
    F("org.apache.http.HttpResponse.getEntity:org.apache.http.HttpEntity()", List((0, -1))),
    F("org.apache.http.HttpResponse.setEntity:void(org.apache.http.HttpEntity)", List((1, 0), (1, 1), (1, 0)))
  )

  /** @return
    *   procedure semantics for operators and common external Java calls only.
    */
  @unused
  def javaSemantics(): FullNameSemantics = FullNameSemantics.fromList(operatorFlows ++ javaFlows)

}
