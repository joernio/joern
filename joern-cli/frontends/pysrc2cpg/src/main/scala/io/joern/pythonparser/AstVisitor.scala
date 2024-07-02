package io.joern.pythonparser

import io.joern.pythonparser.ast.*

trait AstVisitor[T] {
  def visit(ast: iast): T

  def visit(mod: imod): T
  def visit(module: Module): T

  def visit(stmt: istmt): T
  def visit(functionDef: FunctionDef): T
  def visit(functionDef: AsyncFunctionDef): T
  def visit(classDef: ClassDef): T
  def visit(ret: Return): T
  def visit(delete: Delete): T
  def visit(assign: Assign): T
  def visit(typeAlias: TypeAlias): T
  def visit(annAssign: AnnAssign): T
  def visit(augAssign: AugAssign): T
  def visit(forStmt: For): T
  def visit(forStmt: AsyncFor): T
  def visit(whileStmt: While): T
  def visit(ifStmt: If): T
  def visit(withStmt: With): T
  def visit(withStmt: AsyncWith): T
  def visit(matchStmt: Match): T
  def visit(raise: Raise): T
  def visit(tryStmt: Try): T
  def visit(assert: Assert): T
  def visit(importStmt: Import): T
  def visit(importFrom: ImportFrom): T
  def visit(global: Global): T
  def visit(nonlocal: Nonlocal): T
  def visit(expr: Expr): T
  def visit(pass: Pass): T
  def visit(break: Break): T
  def visit(continue: Continue): T
  def visit(raise: RaiseP2): T
  def visit(errorStatement: ErrorStatement): T

  def visit(expr: iexpr): T
  def visit(boolOp: BoolOp): T
  def visit(namedExpr: NamedExpr): T
  def visit(binOp: BinOp): T
  def visit(unaryOp: UnaryOp): T
  def visit(lambda: Lambda): T
  def visit(ifExp: IfExp): T
  def visit(dict: Dict): T
  def visit(set: Set): T
  def visit(listComp: ListComp): T
  def visit(setComp: SetComp): T
  def visit(dictComp: DictComp): T
  def visit(generatorExp: GeneratorExp): T
  def visit(await: Await): T
  def visit(yieldExpr: Yield): T
  def visit(yieldFrom: YieldFrom): T
  def visit(compare: Compare): T
  def visit(call: Call): T
  def visit(formattedValue: FormattedValue): T
  def visit(joinedString: JoinedString): T
  def visit(constant: Constant): T
  def visit(attribute: Attribute): T
  def visit(subscript: Subscript): T
  def visit(starred: Starred): T
  def visit(name: Name): T
  def visit(list: List): T
  def visit(tuple: Tuple): T
  def visit(slice: Slice): T
  def visit(stringExpList: StringExpList): T

  def visit(boolop: iboolop): T
  def visit(and: And.type): T
  def visit(or: Or.type): T

  def visit(operator: ioperator): T
  def visit(add: Add.type): T
  def visit(sub: Sub.type): T
  def visit(mult: Mult.type): T
  def visit(matMult: MatMult.type): T
  def visit(div: Div.type): T
  def visit(mod: Mod.type): T
  def visit(pow: Pow.type): T
  def visit(lShift: LShift.type): T
  def visit(rShift: RShift.type): T
  def visit(bitOr: BitOr.type): T
  def visit(bitXor: BitXor.type): T
  def visit(bitAnd: BitAnd.type): T
  def visit(floorDiv: FloorDiv.type): T

  def visit(unaryop: iunaryop): T
  def visit(invert: Invert.type): T
  def visit(not: Not.type): T
  def visit(uAdd: UAdd.type): T
  def visit(uSub: USub.type): T

  def visit(compop: icompop): T
  def visit(eq: Eq.type): T
  def visit(notEq: NotEq.type): T
  def visit(lt: Lt.type): T
  def visit(ltE: LtE.type): T
  def visit(gt: Gt.type): T
  def visit(gtE: GtE.type): T
  def visit(is: Is.type): T
  def visit(isNot: IsNot.type): T
  def visit(in: In.type): T
  def visit(notIn: NotIn.type): T

  def visit(comprehension: Comprehension): T

  def visit(exceptHandler: ExceptHandler): T

  def visit(arguments: Arguments): T

  def visit(arg: Arg): T

  def visit(constant: iconstant): T
  def visit(stringConstant: StringConstant): T
  def visit(joinedStringConstant: JoinedStringConstant): T
  def visit(boolConstant: BoolConstant): T
  def visit(intConstant: IntConstant): T
  def visit(intConstant: FloatConstant): T
  def visit(imaginaryConstant: ImaginaryConstant): T
  def visit(noneConstant: NoneConstant.type): T
  def visit(ellipsisConstant: EllipsisConstant.type): T

  def visit(keyword: Keyword): T

  def visit(alias: Alias): T

  def visit(withItem: Withitem): T

  def visit(matchCase: MatchCase): T

  def visit(matchValue: MatchValue): T

  def visit(matchSingleton: MatchSingleton): T

  def visit(matchSequence: MatchSequence): T

  def visit(matchMapping: MatchMapping): T

  def visit(matchClass: MatchClass): T

  def visit(matchStar: MatchStar): T

  def visit(matchAs: MatchAs): T

  def visit(matchOr: MatchOr): T

  def visit(typeIgnore: TypeIgnore): T

  def visit(typeVar: TypeVar): T

  def visit(paramSpec: ParamSpec): T

  def visit(typeVarTyple: TypeVarTuple): T
}
