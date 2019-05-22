/** Simple query to output function names:
  * We might also do cpg.method.name.l.mkString("\n").
  * As operators like, +,-,* etc. are treated as methods,
  * the query will also give us the operator methods whose
  * names start with "<". So we filter them out with "\w+"
  * */
cpg.method.name("\\w+").l.map(_.name).mkString("\n")
