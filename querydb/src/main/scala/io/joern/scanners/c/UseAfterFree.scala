package io.joern.scanners.c

import io.joern.console.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.macros.QueryMacros.*
import io.joern.scanners.{Crew, QueryTags}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

object UseAfterFree extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def freeFieldNoReassign()(implicit context: EngineContext): Query =
    Query.make(
      name = "free-field-no-reassign",
      author = Crew.fabs,
      title = "A field of a parameter is free'd and not reassigned on all paths",
      description = """
          | The function is able to modify a field of a structure passed in by
          | the caller. It frees this field and does not guarantee that on
          | all paths to the exit, the field is reassigned. If any
          | caller now accesses the field, then it accesses memory that is no
          | longer allocated. We also check that the function does not free
          | or clear the entire structure, as in that case, it is unlikely that the
          | passed in structure will be used again.
          |""".stripMargin,
      score = 5.0,
      withStrRep({ cpg =>
        val freeOfStructField = cpg
          .method("free")
          .callIn
          .where(
            _.argument(1)
              .isCallTo("<operator>.*[fF]ieldAccess.*")
              .filter(x => x.method.parameter.name.toSet.contains(x.argument(1).code))
          )
          .whereNot(_.argument(1).isCall.argument(1).filter { struct =>
            struct.method.ast.isCall
              .name(".*free$", "memset", "bzero")
              .argument(1)
              .codeExact(struct.code)
              .nonEmpty
          })
          .l

        freeOfStructField.argument(1).filter { arg =>
          arg.method.methodReturn.reachableBy(arg).nonEmpty
        }
      }),
      tags = List(QueryTags.uaf, QueryTags.default),
      codeExamples = CodeExamples(
        List("""
            |
            |void free_field_reassigned(a_struct_type *a_struct) {
            |  free(a_struct->ptr);
            |  if (something) {
            |    return;
            |  }
            |  a_struct->ptr = foo;
            |}
            |
            |""".stripMargin),
        List("""
            |
            |void not_free_field_reassigned(a_struct_type *a_struct) {
            |  free(a_struct->ptr);
            |  if (something) {
            |    a_struct->ptr = NULL;
            |    return;
            |  }
            |  a_struct->ptr = foo;
            |}
            |
            |""".stripMargin)
      )
    )

  @q
  def freeReturnedValue(): Query = {
    Query.make(
      name = "free-returned-value",
      author = Crew.malte,
      title = "A value that is returned through a parameter is free'd in a path",
      description = """
          |The function sets a field of a function parameter to a value of a local
          |variable.
          |This variable is then freed in some paths. Unless the value set in the
          |function parameter is overridden later on, the caller has access to the
          |free'd memory, which is undefined behavior.
          |
          |Finds bugs like CVE-2019-18902.
          |""".stripMargin,
      score = 5.0,
      withStrRep({ cpg =>
        def outParams =
          cpg.parameter
            .code(".+\\*.+")
            .whereNot(
              _.referencingIdentifiers
                .argumentIndex(1)
                .inCall
                .nameExact(Operators.assignment, Operators.addressOf)
            )

        def assignedValues =
          outParams.referencingIdentifiers
            .argumentIndex(1)
            .inCall
            .nameExact(Operators.indirectFieldAccess, Operators.indirection, Operators.indirectIndexAccess)
            .argumentIndex(1)
            .inCall
            .nameExact(Operators.assignment)
            .argument(2)
            .isIdentifier

        def freeAssigned =
          assignedValues.map(id =>
            (
              id,
              id.refsTo
                .flatMap {
                  case p: MethodParameterIn => p.referencingIdentifiers
                  case v: Local             => v.referencingIdentifiers
                }
                .inCall
                .name("(.*_)?free")
            )
          )

        freeAssigned
          .filter { case (id, freeCall) =>
            freeCall.dominatedBy.exists(_ == id)
          }
          .flatMap(_._1)
      }),
      tags = List(QueryTags.uaf),
      codeExamples = CodeExamples(
        List("""
            |
            |void bad(a_struct_type *a_struct) {
            |  void *x = NULL;
            |  a_struct->foo = x;
            |  free(x);
            |}
            |
            |""".stripMargin),
        List("""
            |
            |void good1(a_struct_type *a_struct) {
            |  void *x = NULL, *y = NULL;
            |  a_struct->foo = x;
            |  free(y);
            |}
            |
            |void good2(a_struct_type *a_struct) {
            |  void *x = NULL;
            |  free(a_struct->foo);
            |  a_struct->foo = x;
            |}
            |
            |void bad_not_covered(a_struct_type *a_struct) {
            |  void *x = NULL;
            |  a_struct->foo = x;
            |  free(a_struct->foo);
            |}
            |
            |""".stripMargin)
      )
    )
  }

  @q
  def freePostDominatesUsage(): Query =
    Query.make(
      name = "free-follows-value-reuse",
      author = Crew.malte,
      title = "A value that is free'd is reused without reassignment.",
      description = """
        |A value is used after being free'd in a path that leads to it
        |without reassignment.
        |
        |Modeled after CVE-2019-18903.
        |""".stripMargin,
      score = 5.0,
      withStrRep({ cpg =>
        cpg.method
          .name("(.*_)?free")
          .filter(_.parameter.size == 1)
          .callIn
          .where(_.argument(1).isIdentifier)
          .flatMap(f => {
            val freedIdentifierCode = f.argument(1).code
            val postDom             = f.postDominatedBy.toSetImmutable

            val assignedPostDom = postDom.isIdentifier
              .where(_.inAssignment)
              .codeExact(freedIdentifierCode)
              .flatMap(id => Iterator.single(id) ++ id.postDominatedBy)

            postDom
              .removedAll(assignedPostDom)
              .isIdentifier
              .codeExact(freedIdentifierCode)
          })
      }),
      tags = List(QueryTags.uaf),
      codeExamples = CodeExamples(
        List("""
          |
          |void *bad() {
          |  void *x = NULL;
          |  if (cond)
          |    free(x);
          |  return x;
          |}
          |
          |void *false_positive() {
          |  void *x = NULL;
          |  free(x);
          |  if (cond)
          |    x = NULL;
          |  else
          |    x = NULL;
          |  return x;
          |}
          |
          |""".stripMargin),
        List("""
          |
          |void *false_negative() {
          |  void *x = NULL;
          |  if (cond) {
          |    free(x);
          |    if (cond2)
          |      return x; // doesn't post-dominate the free call
          |    x = NULL;
          |  }
          |  return x;
          |}
          |
          |void *good() {
          |  void *x = NULL;
          |  if (cond)
          |    free(x);
          |  x = NULL;
          |  return x;
          |}
          |
          |""".stripMargin)
      )
    )
}
