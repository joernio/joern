package io.joern.pythonparser
import io.joern.pythonparser.ast.*
import scala.collection.immutable

class AstPrinter(indentStr: String) extends AstVisitor[String] {
  private val ls = "\n"

  def print(astNode: iast): String = {
    astNode.accept(this)
  }

  def printIndented(astNode: iast): String = {
    val printStr = astNode.accept(this)

    indentStr + printStr.replaceAll(ls, ls + indentStr)
  }

  private def printTypeParams(typeParams: CollType[itypeParam]): String = {
    if (typeParams.nonEmpty) {
      "[" + typeParams.map(print).mkString(", ") + "]"
    } else {
      ""
    }
  }

  override def visit(ast: iast): String = ???

  override def visit(mod: imod): String = ???

  override def visit(module: Module): String = {
    module.stmts.map(print).mkString(ls)
  }

  override def visit(stmt: istmt): String = ???

  override def visit(functionDef: FunctionDef): String = {
    functionDef.decorator_list.map(d => "@" + print(d) + ls).mkString("") +
      "def " + functionDef.name + printTypeParams(functionDef.type_params) + "(" + print(functionDef.args) + ")" +
      functionDef.returns.map(r => " -> " + print(r)).getOrElse("") +
      ":" + functionDef.body.map(printIndented).mkString(ls, ls, "")

  }

  override def visit(functionDef: AsyncFunctionDef): String = {
    functionDef.decorator_list.map(d => "@" + print(d) + ls).mkString("") +
      "async def " + functionDef.name + printTypeParams(functionDef.type_params) + "(" + print(functionDef.args) + ")" +
      functionDef.returns.map(r => " -> " + print(r)).getOrElse("") +
      ":" + functionDef.body.map(printIndented).mkString(ls, ls, "")

  }

  override def visit(classDef: ClassDef): String = {
    val optionArgEndComma = if (classDef.bases.nonEmpty && classDef.keywords.nonEmpty) ", " else ""

    classDef.decorator_list.map(d => "@" + print(d) + ls).mkString("") +
      "class " + classDef.name + printTypeParams(classDef.type_params) +
      "(" +
      classDef.bases.map(print).mkString(", ") +
      optionArgEndComma +
      classDef.keywords.map(print).mkString(", ") +
      ")" + ":" +
      classDef.body.map(printIndented).mkString(ls, ls, "")
  }

  override def visit(ret: Return): String = {
    "return" + ret.value.map(v => " " + print(v)).getOrElse("")
  }

  override def visit(delete: Delete): String = {
    "del " + delete.targets.map(print).mkString(", ")
  }

  override def visit(assign: Assign): String = {
    assign.targets.map(print).mkString("", " = ", " = ") + print(assign.value)
  }

  override def visit(typeAlias: TypeAlias): String = {
    "type " + print(typeAlias.name) + printTypeParams(typeAlias.type_params) + " = " + print(typeAlias.value)
  }

  override def visit(annAssign: AnnAssign): String = {
    print(annAssign.target) +
      ": " + print(annAssign.annotation) +
      annAssign.value.map(v => " = " + print(v)).getOrElse("")
  }

  override def visit(augAssign: AugAssign): String = {
    print(augAssign.target) +
      " " + print(augAssign.op) + "= " +
      print(augAssign.value)
  }

  override def visit(forStmt: For): String = {
    "for " + print(forStmt.target) + " in " + print(forStmt.iter) + ":" +
      forStmt.body.map(printIndented).mkString(ls, ls, "") +
      (if (forStmt.orelse.nonEmpty)
         s"${ls}else:" +
           forStmt.orelse.map(printIndented).mkString(ls, ls, "")
       else "")
  }

  override def visit(forStmt: AsyncFor): String = {
    "async for " + print(forStmt.target) + " in " + print(forStmt.iter) + ":" +
      forStmt.body.map(printIndented).mkString(ls, ls, "") +
      (if (forStmt.orelse.nonEmpty)
         s"${ls}else:" +
           forStmt.orelse.map(printIndented).mkString(ls, ls, "")
       else "")
  }

  override def visit(whileStmt: While): String = {
    "while " + print(whileStmt.test) + ":" +
      whileStmt.body.map(printIndented).mkString(ls, ls, "") +
      (if (whileStmt.orelse.nonEmpty)
         s"${ls}else:" +
           whileStmt.orelse.map(printIndented).mkString(ls, ls, "")
       else "")
  }

  override def visit(ifStmt: If): String = {
    val elseString =
      ifStmt.orelse.size match {
        case 0 => ""
        case 1 if ifStmt.orelse.head.isInstanceOf[If] =>
          s"${ls}el" + print(ifStmt.orelse.head)
        case _ =>
          s"${ls}else:" +
            ifStmt.orelse.map(printIndented).mkString(ls, ls, "")
      }

    "if " + print(ifStmt.test) + ":" +
      ifStmt.body.map(printIndented).mkString(ls, ls, "") +
      elseString
  }

  override def visit(withStmt: With): String = {
    "with " + withStmt.items.map(print).mkString(", ") + ":" +
      withStmt.body.map(printIndented).mkString(ls, ls, "")
  }

  override def visit(withStmt: AsyncWith): String = {
    "async with " + withStmt.items.map(print).mkString(", ") + ":" +
      withStmt.body.map(printIndented).mkString(ls, ls, "")
  }

  override def visit(matchStmt: Match): String = {
    val subjectSuffix = matchStmt.subject match {
      case _: Starred =>
        ","
      case _ =>
        ""
    }
    "match " + print(matchStmt.subject) + subjectSuffix + ":" +
      matchStmt.cases.map(printIndented).mkString(ls, ls, "")
  }

  override def visit(raise: Raise): String = {
    "raise" + raise.exc.map(e => " " + print(e)).getOrElse("") +
      raise.cause.map(c => " from " + print(c)).getOrElse("")
  }

  override def visit(tryStmt: Try): String = {
    val elseString =
      if (tryStmt.orelse.nonEmpty) {
        s"${ls}else:" +
          tryStmt.orelse.map(printIndented).mkString(ls, ls, "")
      } else {
        ""
      }

    val finallyString =
      if (tryStmt.finalbody.nonEmpty) {
        s"${ls}finally:" +
          tryStmt.finalbody.map(printIndented).mkString(ls, ls, "")
      } else {
        ""
      }

    val handlersString = {
      if (tryStmt.handlers.nonEmpty) {
        tryStmt.handlers.map(print).mkString(ls, ls, "")
      } else {
        ""
      }
    }

    "try:" +
      tryStmt.body.map(printIndented).mkString(ls, ls, "") +
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
    "from" + relativeImportDots + importFrom.module.map(m => " " + m).getOrElse("") +
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

  override def visit(raise: RaiseP2): String = {
    "raise" + raise.typ.map(t => " " + print(t)).getOrElse("") +
      raise.inst.map(i => ", " + print(i)).getOrElse("") +
      raise.tback.map(t => ", " + print(t)).getOrElse("")
  }

  override def visit(errorStmt: ErrorStatement): String = {
    "<error>"
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

  override def visit(lambda: Lambda): String = {
    val argStr = print(lambda.args)
    if (argStr.nonEmpty) {
      "lambda " + argStr + ": " + print(lambda.body)
    } else {
      "lambda: " + print(lambda.body)
    }
  }

  override def visit(ifExp: IfExp): String = {
    print(ifExp.body) + " if " + print(ifExp.test) + " else " + print(ifExp.orelse)
  }

  override def visit(dict: Dict): String = {
    "{" + dict.keys
      .zip(dict.values)
      .map { case (key, value) =>
        key match {
          case Some(k) =>
            print(k) + ":" + print(value)
          case None =>
            "**" + print(value)
        }
      }
      .mkString(", ") + "}"
  }

  override def visit(set: Set): String = {
    "{" + set.elts.map(print).mkString(", ") + "}"
  }

  override def visit(listComp: ListComp): String = {
    "[" + print(listComp.elt) + listComp.generators.map(print).mkString("") + "]"
  }

  override def visit(setComp: SetComp): String = {
    "{" + print(setComp.elt) + setComp.generators.map(print).mkString("") + "}"
  }

  override def visit(dictComp: DictComp): String = {
    "{" + print(dictComp.key) + ":" + print(dictComp.value) +
      dictComp.generators.map(print).mkString("") + "}"
  }

  override def visit(generatorExp: GeneratorExp): String = {
    "(" + print(generatorExp.elt) + generatorExp.generators.map(print).mkString("") + ")"
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
    print(compare.left) + compare.ops
      .zip(compare.comparators)
      .map { case (op, comparator) =>
        " " + print(op) + " " + print(comparator)
      }
      .mkString("")
  }

  override def visit(call: Call): String = {
    if (call.args.size == 1 && call.args.head.isInstanceOf[GeneratorExp]) {
      // Special case in order to avoid double parenthesis since GeneratorExp adds a
      // set of parenthesis on its own.
      print(call.func) + print(call.args.head)
    } else {
      val optionArgEndComma = if (call.args.nonEmpty && call.keywords.nonEmpty) ", " else ""
      print(call.func) + "(" + call.args.map(print).mkString(", ") + optionArgEndComma +
        call.keywords.map(print).mkString(", ") + ")"
    }
  }

  override def visit(formattedValue: FormattedValue): String = {
    val equalSignStr = if (formattedValue.equalSign) "=" else ""
    val conversionStr = formattedValue.conversion match {
      case -1  => ""
      case 115 => "!s"
      case 114 => "!r"
      case 97  => "!a"
    }

    val formatSpecStr = formattedValue.format_spec match {
      case Some(formatSpec) => ":" + formatSpec
      case None             => ""
    }

    "{" + print(formattedValue.value) +
      equalSignStr +
      conversionStr +
      formatSpecStr +
      "}"
  }

  override def visit(joinedString: JoinedString): String = {
    joinedString.prefix + joinedString.quote +
      joinedString.values.map(print).mkString("") + joinedString.quote
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

  override def visit(list: List): String = {
    "[" + list.elts.map(print).mkString(", ") + "]"
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

  override def visit(stringExpList: StringExpList): String = {
    stringExpList.elts.map(print).mkString(" ")
  }

  override def visit(alias: Alias): String = {
    alias.name + alias.asName.map(n => " as " + n).getOrElse("")
  }

  override def visit(boolop: iboolop): String = ???

  override def visit(and: And.type): String = {
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
    stringConstant.prefix + stringConstant.quote + stringConstant.value + stringConstant.quote
  }

  override def visit(joinedStringConstant: JoinedStringConstant): String = {
    joinedStringConstant.value
  }

  override def visit(boolConstant: BoolConstant): String = {
    if (boolConstant.value) {
      "True"
    } else {
      "False"
    }
  }

  override def visit(intConstant: IntConstant): String = {
    intConstant.value
  }

  override def visit(floatConstant: FloatConstant): String = {
    floatConstant.value
  }

  override def visit(imaginaryConstant: ImaginaryConstant): String = {
    imaginaryConstant.value
  }

  override def visit(noneConstant: NoneConstant.type): String = {
    "None"
  }

  override def visit(ellipsisConstant: EllipsisConstant.type): String = {
    "..."
  }

  override def visit(exceptHandler: ExceptHandler): String = {
    "except" +
      exceptHandler.typ.map(t => " " + print(t)).getOrElse("") +
      exceptHandler.name.map(n => " as " + n).getOrElse("") +
      ":" +
      exceptHandler.body.map(printIndented).mkString(ls, ls, "")
  }

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

  override def visit(arg: Arg): String = {
    arg.arg + arg.annotation.map(a => ": " + print(a)).getOrElse("")
  }

  override def visit(arguments: Arguments): String = {
    var result             = ""
    var separatorString    = ""
    val combinedPosArgSize = arguments.posonlyargs.size + arguments.args.size
    val defaultArgs = immutable.List.fill(combinedPosArgSize - arguments.defaults.size)(None) ++
      arguments.defaults.map(Option.apply)

    if (arguments.posonlyargs.nonEmpty) {
      val posOnlyArgsString =
        arguments.posonlyargs
          .zip(defaultArgs)
          .map { case (arg, defaultOption) =>
            print(arg) + defaultOption.map(d => " = " + print(d)).getOrElse("")
          }
          .mkString("", ", ", ", /")

      result += posOnlyArgsString
      separatorString = ", "
    }

    if (arguments.args.nonEmpty) {
      val defaultsForArgs = defaultArgs.drop(arguments.posonlyargs.size)
      val argsString =
        arguments.args
          .zip(defaultsForArgs)
          .map { case (arg, defaultOption) =>
            print(arg) + defaultOption.map(d => " = " + print(d)).getOrElse("")
          }
          .mkString(separatorString, ", ", "")

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
        arguments.kwonlyargs
          .zip(arguments.kw_defaults)
          .map { case (arg, defaultOption) =>
            print(arg) + defaultOption.map(d => " = " + print(d)).getOrElse("")
          }
          .mkString(separatorString, ", ", "")

      result += kwOnlyArgsString
      separatorString = ", "
    }

    arguments.kw_arg.foreach { k =>
      result += separatorString
      result += "**" + print(k)
    }

    result
  }

  override def visit(withItem: Withitem): String = {
    print(withItem.context_expr) + withItem.optional_vars.map(o => " as " + print(o)).getOrElse("")
  }

  override def visit(matchCase: MatchCase): String = {
    "case " + print(matchCase.pattern) + matchCase.guard.map(g => " if " + print(g)).getOrElse("") + ":" +
      matchCase.body.map(printIndented).mkString(ls, ls, "")
  }

  override def visit(matchValue: MatchValue): String = {
    print(matchValue.value)
  }

  override def visit(matchSingleton: MatchSingleton): String = {
    print(matchSingleton.value)
  }

  override def visit(matchSequence: MatchSequence): String = {
    matchSequence.patterns.map(print).mkString("[", ", ", "]")
  }

  override def visit(matchMapping: MatchMapping): String = {
    "{" + matchMapping.keys
      .zip(matchMapping.patterns)
      .map { case (key, pattern) =>
        print(key) + ": " + print(pattern)
      }
      .mkString(", ") +
      matchMapping.rest
        .map { r =>
          val separatorString =
            if (matchMapping.keys.nonEmpty) {
              ", "
            } else {
              ""
            }
          separatorString + "**" + r
        }
        .getOrElse("") + "}"
  }

  override def visit(matchClass: MatchClass): String = {
    val separatorString =
      if (matchClass.patterns.nonEmpty && matchClass.kwd_patterns.nonEmpty) {
        ", "
      } else {
        ""
      }

    print(matchClass.cls) +
      "(" +
      matchClass.patterns.map(print).mkString(", ") +
      separatorString +
      matchClass.kwd_attrs
        .zip(matchClass.kwd_patterns)
        .map { case (name, pattern) =>
          name + " = " + print(pattern)
        }
        .mkString(", ") +
      ")"
  }

  override def visit(matchStar: MatchStar): String = {
    "*" + matchStar.name.getOrElse("_")
  }

  override def visit(matchAs: MatchAs): String = {
    matchAs.pattern match {
      case Some(pattern) =>
        print(pattern) + matchAs.name.map(name => " as " + name).getOrElse("")
      case None =>
        matchAs.name.getOrElse("_")
    }
  }

  override def visit(matchOr: MatchOr): String = {
    matchOr.patterns.map(print).mkString(" | ")
  }

  override def visit(comprehension: Comprehension): String = {
    val prefix =
      if (comprehension.is_async) {
        " async for "
      } else {
        " for "
      }

    prefix + print(comprehension.target) + " in " + print(comprehension.iter) +
      comprehension.ifs.map(i => " if " + print(i)).mkString("")
  }

  override def visit(typeIgnore: TypeIgnore): String = {
    typeIgnore.tag
  }

  override def visit(typeVar: TypeVar): String = {
    typeVar.name + typeVar.bound.map(b => ": " + print(b)).getOrElse("")
  }

  override def visit(paramSpec: ParamSpec): String = {
    "**" + paramSpec.name
  }

  override def visit(typeVarTuple: TypeVarTuple): String = {
    "*" + typeVarTuple.name
  }
}
