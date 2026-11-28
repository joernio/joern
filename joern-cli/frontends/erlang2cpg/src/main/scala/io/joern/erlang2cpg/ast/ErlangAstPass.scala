package io.joern.erlang2cpg.ast

import io.joern.erlang2cpg.parser.{ParsedModule, Term}
import io.joern.x2cpg.{Ast, AstCreatorBase, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, DispatchTypes, EvaluationStrategies, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.CpgPass
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path}
import scala.collection.mutable

class ErlangAstPass(cpg: Cpg, root: Path, units: Seq[ParsedModule]) extends CpgPass(cpg) {
  override def run(dstGraph: DiffGraphBuilder): Unit = {
    val base  = if (Files.isDirectory(root)) root else root.toAbsolutePath.getParent
    val index = new CallIndex(units)
    units.sortBy(_.file.toString).foreach { unit =>
      dstGraph.absorb(new ModuleAst(base.toAbsolutePath.normalize, unit, index).createAst())
    }
  }
}

/** Branches inherit the bindings at entry but declare variables independently. */
private[ast] final class Scope(
  val bindings: mutable.LinkedHashMap[String, NewNode] = mutable.LinkedHashMap.empty,
  val locals: mutable.ArrayBuffer[NewLocal] = mutable.ArrayBuffer.empty,
  val inherited: String => Option[NewNode] = _ => None
) {
  // A case/if has one lexical slot for each new name, but each arm must bind it independently.
  var alternativeLocals: Option[mutable.LinkedHashMap[String, NewLocal]] = None
  var funHead                                                            = false
  def forHead(patterns: Seq[Term]): Scope                                = {
    if (!funHead) child()
    else {
      def variables(term: Term): Seq[String] = term.tag match {
        case "var"                              => Seq(term(2).text)
        case "map_field_exact" | "record_field" => variables(term(3))
        case "bin_element"                      => variables(term(2))
        case _                                  => term.items.flatMap(variables)
      }
      val shadowed = patterns.flatMap(variables).toSet
      new Scope(bindings.clone(), locals, name => if (shadowed(name)) None else inherited(name))
    }
  }
  def child(): Scope                        = new Scope(bindings.clone(), locals, inherited)
  def lookup(name: String): Option[NewNode] = bindings.get(name).orElse(inherited(name))
}

private[ast] final class ModuleAst(root: Path, unit: ParsedModule, val index: CallIndex)
    extends AstCreatorBase[Term, ModuleAst](unit.file.toString)(using ValidationMode.Disabled)
    with ErlangControl
    with ErlangCatch
    with ErlangPatterns
    with ErlangFuns
    with ErlangProcesses {
  protected def line(node: Term)                       = node.line
  protected def column(node: Term)                     = node.column
  protected def lineEnd(node: Term)                    = None
  protected def columnEnd(node: Term)                  = None
  protected def offsetSource                           = None
  protected def unadjustedOffset(node: Term)           = None
  override protected[ast] def code(node: Term): String = node.tag match {
    case "atom" | "var" | "integer" | "float" | "char" => node(2).text
    case "string"                                      => "\"" + node(2).text + "\""
    case "nil"                                         => "[]"
    case "tuple"                                       => node(2).items.map(code).mkString("{", ", ", "}")
    case "cons"                                        => s"[${code(node(2))} | ${code(node(3))}]"
    case "bin"                                         => node(2).items.map(code).mkString("<<", ", ", ">>")
    case "bin_element"                                 =>
      val size = if (node(3) == Term.Atom("default")) "" else ":" + code(node(3))
      code(node(2)) + size
    case "map_field_exact"            => s"${code(node(2))} := ${code(node(3))}"
    case "map_field_assoc"            => s"${code(node(2))} => ${code(node(3))}"
    case "map"                        => node.items.last.items.map(code).mkString("#{", ", ", "}")
    case "remote"                     => s"${code(node(2))}:${code(node(3))}"
    case "catch"                      => "catch " + code(node(2))
    case "call"                       => code(node(2)) + node(3).items.map(code).mkString("(", ", ", ")")
    case "match"                      => s"${code(node(2))} = ${code(node(3))}"
    case "op" if node.items.size == 5 => s"${code(node(3))} ${node(2).text} ${code(node(4))}"
    case "op"                         => s"${node(2).text} ${code(node(3))}"
    case kind                         => kind
  }

  val module: String = unit.moduleName

  private def relative(file: Path): String = {
    val absolute = file.toAbsolutePath.normalize
    if (absolute.startsWith(root)) root.relativize(absolute).toString else absolute.toString
  }

  private var filename              = relative(unit.file)
  val closures                      = mutable.ArrayBuffer.empty[Ast]
  private val unsupported           = mutable.TreeMap.empty[String, Int]
  private var freshId               = 0
  def fresh(prefix: String): String = {
    freshId += 1
    s"<$prefix$freshId>"
  }

  def createAst(): DiffGraphBuilder = {
    val namespace = NewNamespaceBlock().name(module).fullName(module).filename(filename)
    val members   = unit.forms.flatMap { form =>
      filename = relative(Path.of(form.file))
      val term = form.term
      if (term.tag == "function") Some(function(term, term(2).text, term(4).items, new Scope()))
      else declaration(term)
    }
    Ast.storeInDiffGraph(Ast(namespace).withChildren(members ++ closures), diffGraph)
    unsupported.foreach { case (kind, count) =>
      LoggerFactory.getLogger(getClass).warn(s"${unit.file}: $count unsupported Erlang form(s) of kind $kind")
    }
    diffGraph
  }

  private val metadata = Set(
    "module",
    "export",
    "import",
    "export_type",
    "file",
    "compile",
    "spec",
    "type",
    "opaque",
    "nominal",
    "doc",
    "moduledoc",
    "behaviour",
    "behavior",
    "optional_callbacks",
    "on_load",
    "vsn",
    "deprecated",
    "feature",
    "dialyzer"
  )

  private def declaration(term: Term): Option[Ast] = term.tag match {
    case "eof" | "warning" => None
    case "attribute"       =>
      term(2).text match {
        case "callback" => callback(term, filename)
        case "record"   =>
          val name   = term(3)(0).text
          val fields = term(3)(1).items.map { entry =>
            val field     = if (entry.tag == "typed_record_field") entry(1) else entry
            val fieldName = field(2)(2).text
            Ast(memberNode(field, fieldName, fieldName, Defines.Any))
          }
          Some(Ast(typeDeclNode(term, name, s"$module:#$name", filename, s"-record($name)")).withChildren(fields))
        case kind if metadata(kind) => None
        case kind                   => Some(unknown(term, s"attribute:$kind"))
      }
    case kind => Some(unknown(term, kind))
  }

  def function(origin: Term, name: String, rows: Seq[Term], scope: Scope, shadowHead: Boolean = false): Ast = {
    scope.funHead = shadowHead
    val heads      = rows.map(_(2).items)
    val arity      = heads.head.size
    val parameters = heads.head.indices.map { i =>
      val head      = heads.head(i)
      val shared    = heads.map(_(i)).forall(term => term.tag == "var" && head.tag == "var" && term(2) == head(2))
      val paramName =
        if (shared && head(2).text != "_" && !scope.bindings.contains(head(2).text)) head(2).text
        else s"<arg${i + 1}>"
      val param = parameterInNode(head, paramName, paramName, i + 1, false, EvaluationStrategies.BY_VALUE, Defines.Any)
      scope.bindings(paramName) = param
      param
    }
    val inputs = parameters.map(param => () => reference(origin, param.name, Some(param)))
    val body   = clauses(rows, inputs, scope, tail = true)
    methodAst(
      methodNode(origin, name, s"$module:$name/$arity", s"/$arity", filename),
      parameters.map(Ast(_)),
      block(origin, scope.locals.toSeq.map(Ast(_)) :+ body),
      methodReturnNode(origin, Defines.Any)
    )
  }

  def block(term: Term, children: Seq[Ast]): Ast =
    blockAst(blockNode(term, "", Defines.Any), children.toList)

  def returned(term: Term, ast: Ast): Ast = returnAst(returnNode(term, code(term)), Seq(ast))

  def statements(terms: Seq[Term], scope: Scope, tail: Boolean): Seq[Ast] = terms.zipWithIndex.map { case (term, i) =>
    expr(term, scope, tail && i == terms.size - 1)
  }

  def reference(term: Term, name: String, declaration: Option[NewNode]): Ast = {
    val identifier = identifierNode(term, name, name, Defines.Any)
    declaration.fold(Ast(identifier))(decl => Ast(identifier).withRefEdge(identifier, decl))
  }

  def local(term: Term, name: String, scope: Scope): NewLocal = {
    def declare(): NewLocal = {
      val declaration = localNode(term, name, name, Defines.Any)
      scope.locals += declaration
      declaration
    }
    val declaration = scope.alternativeLocals match {
      case Some(shared) if !name.startsWith("<") => shared.getOrElseUpdate(name, declare())
      case _                                     => declare()
    }
    scope.bindings(name) = declaration
    declaration
  }

  def operator(term: Term, name: String, args: Seq[Ast], rendered: String): Ast =
    callAst(operatorCallNode(term, rendered, name, None), args)

  private val operators = Map(
    "+"       -> Operators.addition,
    "-"       -> Operators.subtraction,
    "*"       -> Operators.multiplication,
    "/"       -> Operators.division,
    "div"     -> Operators.division,
    "rem"     -> Operators.modulo,
    "=="      -> Operators.equals,
    "=:="     -> Operators.equals,
    "/="      -> Operators.notEquals,
    "=/="     -> Operators.notEquals,
    "<"       -> Operators.lessThan,
    "=<"      -> Operators.lessEqualsThan,
    ">"       -> Operators.greaterThan,
    ">="      -> Operators.greaterEqualsThan,
    "not"     -> Operators.logicalNot,
    "band"    -> Operators.and,
    "bor"     -> Operators.or,
    "bxor"    -> Operators.xor,
    "bsl"     -> Operators.shiftLeft,
    "bsr"     -> Operators.arithmeticShiftRight,
    "andalso" -> Operators.logicalAnd,
    "orelse"  -> Operators.logicalOr
  )

  private def composite(term: Term, parts: Seq[Term], scope: Scope): Ast = {
    val args = parts.map {
      case Term.Atom(value) => Ast(literalNode(term, value, "atom"))
      case part             => expr(part, scope)
    }
    operator(term, "<operator>." + term.tag, args, code(term))
  }

  def addModelTag(ast: Ast, name: String, value: String): Unit = ast.root.foreach { node =>
    val tag = NewTag().name(name).value(value)
    diffGraph.addNode(tag)
    diffGraph.addEdge(node, tag, io.shiftleft.codepropertygraph.generated.EdgeTypes.TAGGED_BY)
  }

  private def unknown(term: Term, kind: String): Ast = {
    unsupported.updateWith(kind)(prior => Some(prior.getOrElse(0) + 1))
    Ast(unknownNode(term, kind).parserTypeName(kind))
  }

  def expr(term: Term, scope: Scope, tail: Boolean = false): Ast = {
    // A try expression has its own handler and after regions; crossing those with a lexical catch jump is unsound.
    def controlled = if (term.tag == "try") withoutCatch(control(term, scope, tail)) else control(term, scope, tail)
    process(term, scope, tail).orElse(controlled).getOrElse {
      val result = term.tag match {
        case "var" => reference(term, term(2).text, scope.lookup(term(2).text))
        case "atom" | "integer" | "float" | "char" | "string" | "nil" =>
          Ast(literalNode(term, code(term), term.tag))
        case "match"             => matched(term, scope)
        case "fun" | "named_fun" => withoutCatch(fun(term, scope))
        case "catch"             => catchExpr(term, scope)
        case "block"             => block(term, statements(term(2).items, scope, tail = false))
        case "tuple" | "bin"     => composite(term, term(2).items, scope)
        case "cons" | "map_field_assoc" | "map_field_exact" => composite(term, term.items.drop(2), scope)
        case "map" | "record"                               =>
          val parts = term.items.drop(2).flatMap {
            case Term.ListTerm(fields) => fields
            case part                  => Vector(part)
          }
          composite(term, parts, scope)
        case "bin_element" =>
          val parts = Seq(term(2)) ++ Option.when(term(3) != Term.Atom("default"))(term(3)) ++
            Option.when(term(4) != Term.Atom("default"))(Term.Atom(term(4).text))
          composite(term, parts, scope)
        case "record_field" | "record_index" => composite(term, term.items.drop(2), scope)
        case "op"                            =>
          val name = operators.getOrElse(term(2).text, "<operator>." + term(2).text)
          operator(term, name, term.items.drop(3).map(expr(_, scope)), code(term))
        case "call" =>
          val callee = term(2)
          val args   = term(3).items
          index.target(module, callee, args.size) match {
            case Some(target) =>
              val name = if (callee.tag == "remote") callee(3)(2).text else callee(2).text
              capturedRaise(term, scope, target, name).getOrElse {
                callAst(
                  callNode(term, code(term), name, target, DispatchTypes.STATIC_DISPATCH),
                  args.map(expr(_, scope))
                )
              }
            case None =>
              val receiver =
                if (callee.tag == "remote")
                  operator(callee, "<operator>.remote", callee.items.drop(2).map(expr(_, scope)), code(callee))
                else expr(callee, scope)
              callAst(
                callNode(
                  term,
                  code(term),
                  "<dynamic>",
                  Defines.DynamicCallUnknownFullName,
                  DispatchTypes.DYNAMIC_DISPATCH
                ),
                args.map(expr(_, scope)),
                base = Some(receiver)
              )
          }
        case other => unknown(term, other)
      }
      if (tail) returned(term, result) else result
    }
  }
}
