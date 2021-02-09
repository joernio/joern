package io.shiftleft.pythonparser
import io.shiftleft.pythonparser.ast._

class AstPrinter(indentStr: String) extends AstVisitor[String] {
  def print(astNode: iast): String = {
    astNode.accept(this)
  }

  def printIndented(astNode: iast): String = {
    val printStr = astNode.accept(this)

    indentStr + printStr.replaceAll("\n", "\n" + indentStr)
  }

  override def visit(ast: iast): String = ???

  override def visit(mod: imod): String = ???

  override def visit(module: Module): String = {
    module.stmts.map(print).mkString("\n")
  }

  override def visit(stmt: istmt): String = ???

  override def visit(functionDef: FunctionDef): String = {
    functionDef.decorator_list.map(d => "@" + print(d) + "\n").mkString("") +
      "def " + functionDef.name + "(" + print(functionDef.args) + ")" +
      functionDef.returns.map(r => " -> " + print(r)).getOrElse("") +
      ":" +  functionDef.body.map(printIndented).mkString("\n", "\n", "")

  }

  override def visit(classDef: ClassDef): String = {
    val optionArgEndComma = if (classDef.bases.nonEmpty && classDef.keywords.nonEmpty) ", " else ""

    classDef.decorator_list.map(d => "@" + print(d) + "\n").mkString("") +
      "class " + classDef.name +
      "(" +
      classDef.bases.map(print).mkString(", ") +
      optionArgEndComma +
      classDef.keywords.map(print).mkString(", ") +
      ")" + ":" +
      classDef.body.map(printIndented).mkString("\n", "\n", "")
  }

  override def visit(ret: Return): String = {
    "return" + ret.value.map(v => " " + print(v)).getOrElse("")
  }

  override def visit(assign: Assign): String = {
    assign.targets.map(print).mkString(", ") + " = " + print(assign.value)
  }

  override def visit(annAssign: AnnAssign): String = ???

  override def visit(augAssign: AugAssign): String = ???

  override def visit(whileStmt: While): String = {
    "while " + print(whileStmt.test) + ":" +
      whileStmt.body.map(printIndented).mkString("\n", "\n", "") +
      (if (whileStmt.orelse.nonEmpty)
        "\nelse:" +
          whileStmt.orelse.map(printIndented).mkString("\n", "\n", "")
      else ""
        )
  }

  override def visit(ifStmt: If): String = {
    val elseString =
      ifStmt.orelse.size match {
        case 0 => ""
        case 1 if ifStmt.orelse.head.isInstanceOf[If] =>
          "\nel" + print(ifStmt.orelse.head)
        case _ =>
          "\nelse:" +
            ifStmt.orelse.map(printIndented).mkString("\n", "\n", "")
      }


    "if " + print(ifStmt.test) + ":" +
      ifStmt.body.map(printIndented).mkString("\n", "\n", "") +
      elseString
  }

  override def visit(raise: Raise): String = {
    "raise" + raise.exc.map(e => " " + print(e)).getOrElse("") +
      raise.cause.map(c => " from " + print(c)).getOrElse("")
  }

  override def visit(tryStmt: Try): String = {
    val elseString =
      if (tryStmt.orelse.nonEmpty) {
        "\nelse:" +
          tryStmt.orelse.map(printIndented).mkString("\n", "\n", "")
      } else {
        ""
      }

    val finallyString =
      if (tryStmt.finalbody.nonEmpty) {
        "\nfinally:" +
          tryStmt.finalbody.map(printIndented).mkString("\n", "\n", "")
      } else {
        ""
      }

    val handlersString = {
      if (tryStmt.handlers.nonEmpty) {
        tryStmt.handlers.map(print).mkString("\n", "\n", "")
      } else {
        ""
      }
    }

    "try:" +
      tryStmt.body.map(printIndented).mkString("\n", "\n", "") +
      handlersString +
      elseString +
      finallyString
  }

  override def visit(assert: Assert): String = {
    "assert " + print(assert.test) + assert.msg.map(m => ", " + print(m)).getOrElse("")
  }

  override def visit(importStmt: Import): String = {
    "import " + importStmt.names.map(print).mkString(", ")
  }

  override def visit(importFrom: ImportFrom): String = {
    val relativeImportDots =
      if (importFrom.level != 0) {
        " " + "." * importFrom.level
      } else {
        ""
      }
    "from" + relativeImportDots + importFrom.module.map(m => " " + m) .getOrElse("") +
      " import " + importFrom.names.map(print).mkString(", ")
  }

  override def visit(global: Global): String = {
    "global " + global.names.mkString(", ")
  }

  override def visit(nonlocal: Nonlocal): String = {
    "nonlocal " + nonlocal.names.mkString(", ")
  }

  override def visit(expr: Expr): String = {
    print(expr.value)
  }

  override def visit(pass: Pass): String = {
    "pass"
  }

  override def visit(break: Break): String = {
    "break"
  }

  override def visit(continue: Continue): String = {
    "continue"
  }

  override def visit(expr: iexpr): String = ???

  override def visit(boolOp: BoolOp): String = {
    val opString = " " + print(boolOp.op) + " "
    boolOp.values.map(print).mkString(opString)
  }

  override def visit(namedExpr: NamedExpr): String = {
    print(namedExpr.target) + " := " + print(namedExpr.value)
  }

  override def visit(binOp: BinOp): String = {
    print(binOp.left) + " " + print(binOp.op) + " " + print(binOp.right)
  }

  override def visit(unaryOp: UnaryOp): String = {
    val opString = unaryOp.op match {
      case Not =>
        print(unaryOp.op) + " "
      case _ =>
        print(unaryOp.op)
    }
    opString + print(unaryOp.operand)
  }

  override def visit(ifExp: IfExp): String = {
    print(ifExp.body) + " if " + print(ifExp.test) + " else " + print(ifExp.orElse)
  }

  override def visit(await: Await): String = {
    "await " + print(await.value)
  }

  override def visit(yieldExpr: Yield): String = {
    "yield" + yieldExpr.value.map(v => " " + print(v)).getOrElse("")
  }

  override def visit(yieldFrom: YieldFrom): String = {
    "yield from " + print(yieldFrom.value)
  }

  override def visit(compare: Compare): String = {
    print(compare.left) + compare.ops.zip(compare.comparators).map { case (op, comparator) =>
      " " + print(op) + " " + print(comparator)
    }.mkString("")
  }

  override def visit(call: Call): String = {
    val optionArgEndComma = if (call.args.nonEmpty && call.keywords.nonEmpty) ", " else ""
    print(call.func) + "(" + call.args.map(print).mkString(", ") + optionArgEndComma +
      call.keywords.map(print).mkString(", ") + ")"
  }

  override def visit(constant: Constant): String = {
    print(constant.value)
  }

  override def visit(attribute: Attribute): String = {
    print(attribute.value) + "." + attribute.attr
  }

  override def visit(subscript: Subscript): String = {
    print(subscript.value) + "[" + print(subscript.slice) + "]"
  }

  override def visit(starred: Starred): String = {
    "*" + print(starred.value)
  }

  override def visit(name: Name): String = {
    name.id
  }

  override def visit(tuple: Tuple): String = {
    if (tuple.elts.size == 1) {
      "(" + print(tuple.elts.head) + ",)"
    } else {
      "(" + tuple.elts.map(print).mkString(",") + ")"
    }
  }

  override def visit(slice: Slice): String = {
    slice.lower.map(print).getOrElse("") +
      ":" + slice.upper.map(print).getOrElse("") +
      slice.step.map(expr => ":" + print(expr)).getOrElse("")
  }

  override def visit(alias: ialias): String = ???

  override def visit(alias: Alias): String = {
    alias.name + alias.asName.map(n => " as " + n).getOrElse("")
  }

  override def visit(boolop: iboolop): String = ???

  override def visit(and: And.type): String =  {
    "and"
  }

  override def visit(or: Or.type): String = {
    "or"
  }

  override def visit(compop: icompop): String = ???

  override def visit(eq: Eq.type): String = {
    "=="
  }

  override def visit(noteq: NotEq.type): String = {
    "!="
  }

  override def visit(lt: Lt.type): String = {
    "<"
  }

  override def visit(ltE: LtE.type): String = {
    "<="
  }

  override def visit(gt: Gt.type): String = {
    ">"
  }

  override def visit(gtE: GtE.type): String = {
    ">="
  }

  override def visit(is: Is.type): String = {
    "is"
  }

  override def visit(isNot: IsNot.type): String = {
    "is not"
  }

  override def visit(in: In.type): String = {
    "in"
  }

  override def visit(notIn: NotIn.type): String = {
    "not in"
  }

  override def visit(constant: iconstant): String = ???

  override def visit(stringConstant: StringConstant): String = {
    stringConstant.value
  }

  override def visit(boolConstant: BoolConstant): String = {
    if (boolConstant.value) {
      "True"
    } else {
      "False"
    }
  }

  override def visit(intConstant: IntConstant): String = {
    intConstant.value.toString()
  }

  override def visit(noneConstant: NoneConstant.type): String = {
    "None"
  }

  override def visit(ellipsisConstant: EllipsisConstant.type): String = {
    "..."
  }

  override def visit(exceptHandler: iexcepthandler): String = ???

  override def visit(exceptHandler: ExceptHandler): String = {
    "except" +
      exceptHandler.typ.map(t => " " + print(t)).getOrElse("") +
      exceptHandler.name.map(n => " as " + n).getOrElse("") +
      ":" +
      exceptHandler.body.map(printIndented).mkString("\n", "\n", "")
  }

  override def visit(keyword: ikeyword): String = ???
  override def visit(keyword: Keyword): String = {
    keyword.arg match {
      case Some(argName) =>
        argName + " = " + print(keyword.value)
      case None =>
        "**" + print(keyword.value)
    }
  }

  override def visit(operator: ioperator): String = ???

  override def visit(add: Add.type): String = {
    "+"
  }

  override def visit(sub: Sub.type): String = {
    "-"
  }

  override def visit(mult: Mult.type): String = {
    "*"
  }

  override def visit(matMult: MatMult.type): String = {
    "@"
  }

  override def visit(div: Div.type): String = {
    "/"
  }

  override def visit(mod: Mod.type): String = {
    "%"
  }

  override def visit(pow: Pow.type): String = {
    "**"
  }

  override def visit(lShift: LShift.type): String = {
    "<<"
  }

  override def visit(rShift: RShift.type): String = {
    ">>"
  }

  override def visit(bitOr: BitOr.type): String = {
    "|"
  }

  override def visit(bitXor: BitXor.type): String = {
    "^"
  }

  override def visit(bitAnd: BitAnd.type): String = {
    "&"
  }

  override def visit(floorDiv: FloorDiv.type): String = {
    "//"
  }

  override def visit(unaryop: iunaryop): String = ???

  override def visit(invert: Invert.type): String = {
    "~"
  }

  override def visit(not: Not.type): String = {
    "not"
  }

  override def visit(uAdd: UAdd.type): String = {
    "+"
  }

  override def visit(uSub: USub.type): String = {
    "-"
  }

  override def visit(arg: iarg): String = ???

  override def visit(arg: Arg): String = {
    arg.arg + arg.annotation.map(a => ": " + print(a)).getOrElse("")
  }

  override def visit(arguments: iarguments): String = ???

  override def visit(arguments: Arguments): String = {
    var result = ""
    var separatorString = ""
    val combinedPosArgSize = arguments.posonlyargs.size + arguments.args.size
    val defaultArgs = List.fill(combinedPosArgSize - arguments.defaults.size)(None) ++
      arguments.defaults.map(Option.apply)

    if (arguments.posonlyargs.nonEmpty) {
      val posOnlyArgsString =
        arguments.posonlyargs.zip(defaultArgs).map { case (arg, defaultOption) =>
          print(arg) + defaultOption.map(d => " = " + print(d)).getOrElse("")
        }.mkString("", ", ", ", /")

      result += posOnlyArgsString
      separatorString = ", "
    }

    if (arguments.args.nonEmpty) {
      val defaultsForArgs = defaultArgs.drop(arguments.posonlyargs.size)
      val argsString =
        arguments.args.zip(defaultsForArgs).map { case (arg, defaultOption) =>
          print(arg) + defaultOption.map(d => " = " + print(d)).getOrElse("")
        }.mkString(separatorString, ", ", "")

      result += argsString
      separatorString = ", "
    }

    arguments.vararg match {
      case Some(v) =>
        result += separatorString
        result += "*" + print(v)
        separatorString = ", "
      case None if arguments.kwonlyargs.nonEmpty =>
        result += separatorString
        result += "*"
        separatorString = ", "
      case None =>
    }

    if (arguments.kwonlyargs.nonEmpty) {
      val kwOnlyArgsString =
        arguments.kwonlyargs.zip(arguments.kw_defaults).map { case (arg, defaultOption) =>
          print(arg) + defaultOption.map(d => " = " + print(d)).getOrElse("")
        }.mkString(separatorString, ", ", "")

      result += kwOnlyArgsString
      separatorString = ", "
    }

    arguments.kw_arg.foreach { k =>
      result += separatorString
      result += "**" + print(k)
    }

    result
  }
}
