import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Expression, FieldIdentifier, Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.{Operators, nodes}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension._
import overflowdb.traversal._

//> using file assertions.sc

private def expressionIsPointer(argument: Expression, isSubExpression: Boolean = false): Boolean = {
  argument match {
    case identifier: Identifier =>
      identifier.typeFullName.endsWith("*") ||
      identifier.typeFullName.endsWith("]") ||
      cpg.local(identifier.name).l.headOption.exists(_.code.contains("*"))
    case call: Call if call.name == Operators.indirectFieldAccess => // On '->', only check the selected field.
      expressionIsPointer(call.start.argument.l.last, isSubExpression = true)
    case call: Call => // On normal nested call, check all arguments are also pointers.
      call.name == Operators.addressOf ||
      call.start.argument.l.exists(expressionIsPointer(_, isSubExpression = true))
    case _ => false
  }
}

@main def main(inputPath: String) = {
  importCode(inputPath)
  val calls = cpg.assignment
    .filter(assign => assign.source.isInstanceOf[Call] && assign.target.isInstanceOf[Identifier])
    .filter { assignment =>
      val target = assignment.target.asInstanceOf[Identifier]
      val source = assignment.source.asInstanceOf[Call]

      source.name.contains(Operators.subtraction) &&
      target.typeFullName == "int" &&
      expressionIsPointer(source)
    }
    .code

  val expected = Seq(
    "simple_subtraction = p - q",
    "nested_subtraction = p - q - r",
    "literal_subtraction = p - i",
    "addrOf_subtraction = p - &i",
    "nested_addrOf_subtraction =  3 - &i - 4",
    "literal_addrOf_subtraction = 3 - &i",
    "array_subtraction = x - p",
    "array_literal_subtraction = x - 3",
    "array_addrOf_subtraction = x - &i",
    // TODO: We don't have access to type info for indirect field member access.
    // "unsafe_struct = foo_t->p - 1"
  )

  assertContains("calls", calls, expected)
}
