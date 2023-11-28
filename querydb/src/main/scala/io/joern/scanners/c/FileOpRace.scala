package io.joern.scanners.c

import io.joern.console.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.macros.QueryMacros.*
import io.joern.scanners.{Crew, QueryTags}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

object FileOpRace extends QueryBundle {

  @q
  def fileOperationRace()(implicit context: EngineContext): Query = {

    Query.make(
      name = "file-operation-race",
      author = Crew.malte,
      title = "Two file operations on the same path can act on different files",
      description = """
        |Two subsequent file operations are performed on the same path. Depending on the permissions
        |on this path, an attacker can exploit a race condition and replace the file or directory
        |the path refers to between these calls.
        |Use file operations based on file descriptor/pointer/handles instead of paths to avoid this issue.
        |""".stripMargin,
      score = 3.0,
      withStrRep({ cpg =>
        val operations: Map[String, Seq[Int]] = Map(
          "access"     -> Seq(1),
          "chdir"      -> Seq(1),
          "chmod"      -> Seq(1),
          "chown"      -> Seq(1),
          "creat"      -> Seq(1),
          "faccessat"  -> Seq(2),
          "fchmodat"   -> Seq(2),
          "fopen"      -> Seq(1),
          "fstatat"    -> Seq(2),
          "lchown"     -> Seq(1),
          "linkat"     -> Seq(2, 4),
          "link"       -> Seq(1, 2),
          "lstat"      -> Seq(1),
          "mkdirat"    -> Seq(2),
          "mkdir"      -> Seq(1),
          "mkfifoat"   -> Seq(2),
          "mkfifo"     -> Seq(1),
          "mknodat"    -> Seq(2),
          "mknod"      -> Seq(1),
          "openat"     -> Seq(2),
          "open"       -> Seq(1),
          "readlinkat" -> Seq(2),
          "readlink"   -> Seq(1),
          "renameat"   -> Seq(2, 4),
          "rename"     -> Seq(1, 2),
          "rmdir"      -> Seq(1),
          "stat"       -> Seq(1),
          "unlinkat"   -> Seq(2),
          "unlink"     -> Seq(1)
        )

        def fileCalls(calls: Iterator[Call]) =
          calls.nameExact(operations.keys.toSeq: _*)

        def fileArgs(c: Call) =
          c.argument.whereNot(_.isLiteral).argumentIndex(operations(c.name): _*)

        fileCalls(cpg.call)
          .filter(call => {
            val otherCalls = fileCalls(call.method.ast.isCall).filter(_ != call)
            val argsForOtherCalls =
              otherCalls.flatMap(c => fileArgs(c)).code.toSet

            fileArgs(call).code.exists(arg => argsForOtherCalls.contains(arg))
          })
      }),
      tags = List(QueryTags.raceCondition, QueryTags.default),
      codeExamples = CodeExamples(
        List("""
          |
          |void insecure_race(char *path) {
          |    chmod(path, 0);
          |    rename(path, "/some/new/path");
          |}
          |
          |""".stripMargin),
        List("""
          |
          |void secure_handle(char *path) {
          |    FILE *file = fopen(path, "r");
          |    fchown(fileno(file), 0, 0);
          |}
          |
          |""".stripMargin)
      )
    )
  }
}
