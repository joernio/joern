package io.joern.jssrc2cpg.datastructures

import ujson.Value

case class Parameter(components: List[Value], initExpression: Option[Value])
