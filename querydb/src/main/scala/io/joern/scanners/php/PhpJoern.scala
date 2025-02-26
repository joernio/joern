package io.joern.scanners.php

import io.joern.console.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.macros.QueryMacros.*
import io.joern.scanners.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

object PhpJoern extends QueryBundle {

  private val PHPJOERN_TAG: String = "phpjoern"
  private val PHPJOERN_EXT_AUT: String = "yichao"

  implicit val resolver: ICallResolver = NoResolve

  @q
  def sqli()(implicit context: EngineContext): Query =
    Query.make(
      name = "phpjoern_sqli",
      author = PHPJOERN_EXT_AUT,
      title = "CWE-89(SQL Injection): A parameter is used in an insecure sqli related func call.",
      description = """
          |An attacker controlled parameter is used in an insecure sqli related func call.
          |
          |If the parameter is not validated and sanitized, this is a sql injection.
          |""".stripMargin,
      score = 5,
      withStrRep({ cpg =>
        // $_REQUEST["foo"], $_GET["foo"], $_POST["foo"]
        // are identifier (at the moment)
        def source =
          cpg.call.name(Operators.assignment).argument.code(".*_(REQUEST|GET|POST).*")

        def sink = cpg.call.name("(query|mysql_query|mysqli_query|mysqli_prepare|mysqli_execute|pg_query|pg_prepare)").argument

        sink.reachableBy(source).l.iterator
      }),
      tags = List(QueryTags.sqlInjection, QueryTags.default, PHPJOERN_TAG)
    )

  @q
  def cmdi()(implicit context: EngineContext): Query =
    Query.make(
      name = "phpjoern_cmdi",
      author = PHPJOERN_EXT_AUT,
      title = "CWE-77(Command Injection): A parameter is used in an insecure cmdi related func call.",
      description = """
          |An attacker controlled parameter is used in an insecure cmdi related func call.
          |
          |If the parameter is not validated and sanitized, this is a remote code execution.
          |""".stripMargin,
      score = 5,
      withStrRep({ cpg =>
        // $_REQUEST["foo"], $_GET["foo"], $_POST["foo"]
        // are identifier (at the moment)
        def source =
          cpg.call.name(Operators.assignment).argument.code(".*_(REQUEST|GET|POST).*")

        def sink = cpg.call.name("(system|exec|shell_exec|passthru|popen|proc_open|backticks)").argument

        sink.reachableBy(source).l.iterator
      }),
      tags = List(QueryTags.xss, QueryTags.default, PHPJOERN_TAG)
    ) 
    
  @q
  def codei()(implicit context: EngineContext): Query =
    Query.make(
      name = "phpjoern_codei",
      author = PHPJOERN_EXT_AUT,
      title = "CWE-94(Code Injection): A parameter is used in an insecure codei related func call.",
      description = """
          |An attacker controlled parameter is used in an insecure codei related func call.
          |
          |If the parameter is not validated and sanitized, this is a remote code execution.
          |""".stripMargin,
      score = 5,
      withStrRep({ cpg =>
        // $_REQUEST["foo"], $_GET["foo"], $_POST["foo"]
        // are identifier (at the moment)
        def source =
          cpg.call.name(Operators.assignment).argument.code(".*_(REQUEST|GET|POST).*")

        def sink = cpg.call.name("(eval|assert|create_function|include|include_once|require|require_once|call_user_func|call_user_func_array)").argument

        sink.reachableBy(source).l.iterator
      }),
      tags = List(QueryTags.remoteCodeExecution, QueryTags.default, PHPJOERN_TAG)
    ) 

  @q
  def uuf()(implicit context: EngineContext): Query =
    Query.make(
      name = "phpjoern_uuf",
      author = PHPJOERN_EXT_AUT,
      title = "CWE-434(Unrestricted Upload of File): A parameter is used in an insecure uuf related func call.",
      description = """
          |An attacker controlled parameter is used in an insecure uuf related func call.
          |
          |If the parameter is not validated and sanitized, this is a remote code execution.
          |""".stripMargin,
      score = 5,
      withStrRep({ cpg =>
        // $_REQUEST["foo"], $_GET["foo"], $_POST["foo"]
        // are identifier (at the moment)
        def source =
          cpg.call.name(Operators.assignment).argument.code(".*_(REQUEST|GET|POST).*")

        def sink = cpg.call.name("(file_get_contents|readfile|fgets|file|fopen|file_put_contents|fwrite|move_uploaded_file|unlink|rename|chmod|chown)").argument

        sink.reachableBy(source).l.iterator
      }),
      tags = List(QueryTags.default, PHPJOERN_TAG)
    ) 


  @q
  def xss()(implicit context: EngineContext): Query =
    Query.make(
      name = "phpjoern_xss",
      author = PHPJOERN_EXT_AUT,
      title = "CWE-79(Cross-site Scription): A parameter is used in an insecure xss related func call.",
      description = """
          |An attacker controlled parameter is used in an insecure xss related func call.
          |
          |If the parameter is not validated and sanitized, this is a XSS.
          |""".stripMargin,
      score = 5,
      withStrRep({ cpg =>
        // $_REQUEST["foo"], $_GET["foo"], $_POST["foo"]
        // are identifier (at the moment)
        def source =
          cpg.call.name(Operators.assignment).argument.code(".*_(REQUEST|GET|POST).*")

        def sink = cpg.call.name("(assert|echo|exit|print|printf|vprintf|print_r|var_dump)").argument

        sink.reachableBy(source).l.iterator
      }),
      tags = List(QueryTags.xss, QueryTags.default, PHPJOERN_TAG)
    )

}
