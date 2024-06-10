package io.joern.scanners.java

import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.joern.x2cpg.Defines
import io.joern.scanners.{Crew, QueryTags}
import io.shiftleft.semanticcpg.language.*

/** Taggers for builtin Java classes.
  */
object JavaBuiltinsTagger extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def javaFileSinks(): Query =
    Query.make(
      name = "java-builtin-file-sources",
      author = Crew.dave,
      title = "Sensitive File Operations",
      description = """
          |File operations can allow attackers to control what files are accessed or read and/or 
          |untrusted data to be written which can lead to remote-code execution.
          |""".stripMargin,
      score = 7,
      withStrRep({ cpg =>
        (cpg.method
          .filter(_.fullName.startsWith("java.io.File"))
          .nameExact(Defines.ConstructorMethodName, "write", "createNewFile") ++
          cpg.method
            .filter(_.fullName.startsWith("java.io.PrintWriter"))
            .nameExact("print", "println") ++
          cpg.method
            .filter(_.fullName.startsWith("java.sql.Connection"))
            .nameExact("prepareStatement") ++
          cpg.method
            .filter(_.fullName.startsWith("java.sql.Statement"))
            .nameExact("execute", "executeUpdate", "executeQuery") ++
          cpg.method
            .filter(_.fullName.startsWith("java.io.FileInputStream"))
            .nameExact(Defines.ConstructorMethodName) ++
          cpg.method
            .filter(_.fullName.startsWith("java.io.FileWriter"))
            .nameExact(Defines.ConstructorMethodName)).parameter.argument
          .where(_.argumentIndex(1))
      }),
      tags = List(QueryTags.taint, QueryTags.sink, QueryTags.default)
    )

  @q
  def javaSqlSinks(): Query =
    Query.make(
      name = "java-builtin-sql-sources",
      author = Crew.dave,
      title = "Sensitive SQL Queries",
      description = """
          |Untrusted data directly injected into database queries can lead to data leaks
          |and possible remote code execution.
          |""".stripMargin,
      score = 8,
      withStrRep({ cpg =>
        (cpg.method
          .filter(_.fullName.startsWith("java.sql.Connection"))
          .nameExact("prepareStatement") ++
          cpg.method
            .filter(_.fullName.startsWith("java.sql.Statement"))
            .nameExact("execute", "executeUpdate", "executeQuery")).parameter.argument
          .where(_.argumentIndex(1))
      }),
      tags = List(QueryTags.taint, QueryTags.sink, QueryTags.default)
    )
}
