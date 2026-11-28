package io.joern.erlang2cpg.ast

import io.joern.erlang2cpg.parser.{ParsedModule, Term}
import io.joern.erlang2cpg.parser.Term.{Atom, Integer, ListTerm, Tuple}

/** Build the export and import tables before lowering any function bodies. */
private[ast] final class CallIndex(units: Seq[ParsedModule]) {
  private type Signature = (String, Int)

  private def signatures(term: Term): Seq[Signature] = term.items.collect {
    case Tuple(Vector(Atom(name), Integer(arity))) => name -> arity.toInt
  }

  private class Symbols(unit: ParsedModule) {
    private val forms                    = unit.forms.map(_.term)
    private def attributes(name: String) =
      forms.collect { case form if form.tag == "attribute" && form(2) == Atom(name) => form(3) }
    val module  = unit.moduleName
    val defined = forms.collect { case form if form.tag == "function" => form(2).text -> form(3).text.toInt }.toSet
    private val options = attributes("compile").flatMap {
      case ListTerm(values) => values
      case option           => Seq(option)
    }
    val exported = if (options.contains(Atom("export_all"))) defined else attributes("export").flatMap(signatures).toSet
    val imported = attributes("import")
      .collect { case Tuple(Vector(Atom(name), entries)) =>
        signatures(entries).map(_ -> name)
      }
      .flatten
      .toMap
    val autoImported =
      if (options.contains(Atom("no_auto_import"))) Set.empty[Signature]
      else
        unit.autoImports -- options.collect { case Tuple(Vector(Atom("no_auto_import"), entries)) =>
          signatures(entries)
        }.flatten
  }

  private val modules = units.map(new Symbols(_)).map(symbols => symbols.module -> symbols).toMap

  private def remote(module: String, name: String, arity: Int): String = {
    val fullName = s"$module:$name/$arity"
    if (modules.get(module).exists(!_.exported.contains(name -> arity))) s"<unexported>.$fullName" else fullName
  }

  def target(module: String, callee: Term, arity: Int): Option[String] = callee.tag match {
    case "atom" =>
      val name      = callee(2).text
      val signature = name -> arity
      val symbols   = modules(module)
      if (symbols.defined(signature)) Some(s"$module:$name/$arity")
      else if (symbols.imported.contains(signature)) Some(remote(symbols.imported(signature), name, arity))
      else if (symbols.autoImported(signature)) Some(s"erlang:$name/$arity")
      else Some(s"$module:$name/$arity")
    case "remote" if callee(2).tag == "atom" && callee(3).tag == "atom" =>
      Some(remote(callee(2)(2).text, callee(3)(2).text, arity))
    case _ => None
  }
}
