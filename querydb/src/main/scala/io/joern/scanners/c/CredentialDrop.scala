package io.joern.scanners.c

import io.joern.scanners.*
import io.joern.console.*
import io.shiftleft.semanticcpg.language.*
import io.joern.macros.QueryMacros.*

object CredentialDrop extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def userCredDrop(): Query =
    Query.make(
      name = "setuid-without-setgid",
      author = Crew.malte,
      title = "Process user ID is changed without changing groups first",
      description = """
        |The set*uid system calls do not affect the groups a process belongs to. However, often
        |there exists a group that is equivalent to a user (e.g. wheel or shadow groups are often
        |equivalent to the root user).
        |Group membership can only be changed by the root user.
        |Changes to the user should therefore always be preceded by calls to set*gid and setgroups,
        |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        cpg
          .method("(?i)set(res|re|e|)uid")
          .callIn
          .whereNot(_.dominatedBy.isCall.name("set(res|re|e|)?gid"))
      }),
      tags = List(QueryTags.setxid, QueryTags.default),
      codeExamples = CodeExamples(
        List("""
          |
          |void bad1() {
          |  setresuid();
          |}
          |
          |void bad3() {
          |  setgroups();
          |  setresuid();
          |}
          |
          |""".stripMargin),
        List("""
          |
          |void good() {
          |  setgroups();
          |  setresgid();
          |  setresuid();
          |}
          |
          |""".stripMargin)
      )
    )

  @q
  def groupCredDrop(): Query =
    Query.make(
      name = "setgid-without-setgroups",
      author = Crew.malte,
      title = "Process group membership is changed without setting ancillary groups first",
      description = """
        |The set*gid system calls do not affect the ancillary groups a process belongs to.
        |Changes to the group membership should therefore always be preceded by a call to setgroups.
        |Otherwise the process may still be a secondary member of the group it tries to disavow.
        |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        cpg
          .method("(?i)set(res|re|e|)gid")
          .callIn
          .whereNot(_.dominatedBy.isCall.name("setgroups"))
      }),
      tags = List(QueryTags.setxid, QueryTags.default),
      codeExamples = CodeExamples(
        List("""
          |
          |void bad2() {
          |  setresgid();
          |  setresuid();
          |}
          |
          |""".stripMargin),
        List("""
          |
          |void good() {
          |  setgroups();
          |  setresgid();
          |  setresuid();
          |}
          |
          |""".stripMargin)
      )
    )
}
