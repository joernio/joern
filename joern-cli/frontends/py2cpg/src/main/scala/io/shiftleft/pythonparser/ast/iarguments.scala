package io.shiftleft.pythonparser.ast

import java.util
import io.shiftleft.pythonparser.AstVisitor
import scala.jdk.CollectionConverters._

trait iarguments extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

// posonlyargs, args and kwonlyargs are lists of arg nodes.
// vararg and kwarg are single arg nodes, referring to the *args, **kwargs parameters.
// kw_defaults is a list of default values for keyword-only arguments.
// If one is None, the corresponding argument is required.
// defaults is a list of default values for arguments that can be passed positionally.
// If there are fewer defaults, they correspond to the last n arguments.
case class Arguments(posonlyargs: Iterable[iarg],
                     args: Iterable[iarg],
                     vararg: Option[iarg],
                     kwonlyargs: Iterable[iarg],
                     kw_defaults: Iterable[Option[iexpr]],
                     kw_arg: Option[iarg],
                     defaults: Iterable[iexpr]) extends iarguments {
  def this(posonlyargs: util.List[iarg],
           args: util.List[iarg],
           vararg: iarg,
           kwonlyargs: util.List[iarg],
           kw_defaults: util.List[iexpr],
           kw_arg: iarg,
           defaults: util.List[iexpr]) = {
    this(posonlyargs.asScala,
      args.asScala,
      Option(vararg),
      kwonlyargs.asScala,
      kw_defaults.asScala.map(Option.apply),
      Option(kw_arg),
      defaults.asScala
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
