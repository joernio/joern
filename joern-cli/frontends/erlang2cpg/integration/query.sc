import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, Method}
import scala.collection.mutable

def one[A](label: String, values: IterableOnce[A]): A = {
  val found = values.iterator.toList
  require(found.size == 1, s"$label expected one endpoint, found ${found.size}: $found")
  found.head
}

def targets(call: Call): List[Method] = call._callOut.collect { case method: Method => method }.toList

def cfgPath(start: CfgNode, end: CfgNode, avoiding: Set[CfgNode] = Set.empty): Boolean = {
  val todo = mutable.Queue(start)
  val visited = mutable.Set.empty[CfgNode]
  while (todo.nonEmpty) {
    val current = todo.dequeue()
    if (!avoiding(current) && visited.add(current)) {
      if (current == end) return true
      todo ++= current._cfgOut.collect { case node: CfgNode => node }
    }
  }
  false
}

@main def verify(input: String, mode: String, check: String, expected: String = ""): Unit = {
  if (mode == "code") importCode(input) else require(importCpg(input).isDefined)
  def method(name: String) = one(name, cpg.method.fullNameExact(name).filterNot(_.isExternal))
  val identities = cpg.method.isExternal(false).fullName.l.sorted
  println("INTERNAL=" + identities.mkString(","))
  if (expected.nonEmpty) require(identities == expected.split(",").sorted.toList, identities.toString)
  if (check == "headers") {
    for ((module, value) <- List("left" -> "101", "right" -> "202")) {
      val expanded = method(s"$module:header_value/0")
      require(expanded.filename.replace('\\', '/').endsWith("include dir/shared.hrl"), expanded.filename)
      val literal = one(s"$module header expansion", expanded.ast.isLiteral)
      require(literal.code == value && literal.lineNumber.contains(2), literal.toString)
    }
  }
  if (check == "other") {
    one("C control method", cpg.method.nameExact("unchanged_frontend").filterNot(_.isExternal))
  }
  if (check == "otp") {
    val get = method("proplists:get_value/2")
    val call = one("line 200", get.call.lineNumber(200).nameExact("get_value"))
    val rows = targets(call).map(_.fullName) -> call.argument(3).code
    val docs = cpg.call.filter(_.lineNumber.exists(line => line >= 194 && line <= 197)).code.toList
    println(s"PROPLISTS get_value/2 line=200 targets=${rows._1} argument3=${rows._2} docSpecCalls=$docs")
    require(rows == (List("proplists:get_value/3"), "undefined") && docs.isEmpty)
  }
  if (check == "fixture") {
    val route = method("demo:route/1")
    val first = one("keep at first site", route.call.nameExact("keep").lineNumber(3))
    val fallback = one("fallback keep", route.call.nameExact("keep").lineNumber(4))
    method("helper:keep/1")
    method("helper:keep/2")
    for (site <- List(first, fallback)) {
      require(targets(site).map(_.fullName) == List("helper:keep/1"), s"keep at ${site.lineNumber}: ${targets(site)}")
    }

    val callees = cpg.method.fullNameExact("demo:route/1").call.nameExact("keep").callee.fullName.l
    require(callees == List("helper:keep/1", "helper:keep/1"), callees.toString)
    val flowTable = cpg.method.fullNameExact("demo:through/2").call.nameExact("observe").argument(1)
      .reachableByFlows(cpg.method.fullNameExact("demo:through/2").parameter.index(2)).p
    require(flowTable.nonEmpty, "README flow query produced no paths")

    val dispatch = method("demo:dispatch/3")
    val dynamic = one("dynamic call", dispatch.call.filter { call =>
      val operands = call.ast.isIdentifier.name.toSet
      Set("M", "F", "X").subsetOf(operands)
    })
    require(targets(dynamic).filterNot(_.isExternal).isEmpty, targets(dynamic).toString)

    val guard = one("route guard", route.call.nameExact("is_integer"))
    val exit = route.methodReturn
    require(cfgPath(guard, fallback, Set(first)), "guard cannot fail over to second clause")
    require(cfgPath(guard, first, Set(fallback)) && cfgPath(first, exit), "guard success cannot return")
    require(!cfgPath(first, fallback), "first successful clause falls through")
    val param = one("route parameter", route.parameter.index(1))
    val bound = one("X at first site", Iterator(first).argument(1).codeExact("X"))
    val zero = one("0 at fallback", Iterator(fallback).argument(1).codeExact("0"))
    require(Iterator(bound).reachableBy(Iterator(param)).nonEmpty, "missing pattern extraction")
    require(Iterator(zero).reachableBy(Iterator(param)).isEmpty, "parameter reaches literal 0")

    val through = method("demo:through/2")
    val observed = one("observe argument", through.call.nameExact("observe").argument(1))
    val kept = one("through parameter 2", through.parameter.index(2))
    val discarded = one("through parameter 1", through.parameter.index(1))
    require(Iterator(observed).reachableBy(Iterator(kept)).nonEmpty, "missing cross-module return")
    require(Iterator(observed).reachableBy(Iterator(discarded)).isEmpty, "discarded parameter reaches observe")
    val paths = Iterator(observed).reachableByFlows(Iterator(kept)).toList
    val across = paths.find(_.elements.exists {
      case node: CfgNode => node.lineNumber.contains(4) && node.method.fullName == "helper:keep/2"
      case _             => false
    })
    require(across.isDefined, s"no path goes through helper:keep/2: $paths")
    println("FLOW=" + across.get.elements.map(node => s"${node.lineNumber}: ${node.code}").mkString(" -> "))

    val waiting = method("demo:wait/0")
    val observe = one("receive observe", waiting.call.nameExact("observe"))
    val payload = one("payload sink", Iterator(observe).argument(1).codeExact("V"))
    val binds = waiting.ast.isIdentifier.nameExact("V").filter(_.id != payload.id).toList
    require(binds.nonEmpty, "missing receive pattern V")
    require(Iterator(payload).reachableBy(binds.iterator).nonEmpty, "receive binding cannot reach use")
    val idle = one("timeout result", waiting.ast.isLiteral.codeExact("idle"))
    require(cfgPath(waiting, idle, Set(observe)) && cfgPath(idle, waiting.methodReturn, Set(observe)), "no timeout return")
    require(!cfgPath(idle, observe), "timeout falls through to receive body")
    val notify = method("demo:notify/2")
    val send = one("notify send", notify.call.filter(_.code.contains("!")))
    val recipient = one("send recipient", Iterator(send).argument(1))
    val message = one("send message", Iterator(send).argument(2))
    require(recipient.code == "Pid" && message.code == "{value, Value}", s"send operands: ${recipient.code} ${message.code}")
    require(!notify.call.flatMap(targets).exists(_.id == waiting.id), "ordinary call from send to receive")
  }
  println(s"PASS $check $mode")
}
