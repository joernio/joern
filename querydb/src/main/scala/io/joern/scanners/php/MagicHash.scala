package io.joern.scanners.php

import io.joern.console.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.macros.QueryMacros.*
import io.joern.scanners.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

object MagicHash extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def magicHash()(implicit context: EngineContext): Query =
    Query.make(
      name = "magic-hash",
      author = Crew.penghui,
      title = "Magic Hash: A parameter is used to compute a hash and then loosely compared (`==`) with another value.",
      description = """
          |An attacker controlled parameter is used in a hash related func call and then loosely compared with another value. This is also known as type juggling.
          |
          |This could potentially lead to bypass of the checks, resulting in authentication bypass, etc.
          |""".stripMargin,
      score = 5,
      withStrRep({ cpg =>
        // $_REQUEST["foo"], $_GET["foo"], $_POST["foo"]
        // are identifier (at the moment)
        def source = cpg.call.name(Operators.assignment).argument.code(".*_(REQUEST|GET|POST).*")

        // according to real-world cases listed in https://github.com/spaze/hashes
        def hash = cpg.call
          .name("(?i)hash")
          .where(
            _.argument(1).isLiteral.code(
              "\"(?i)(md2|md4|md5|sha1|sha224|sha256|ripemd128|ripemd160|tiger128,3|tiger128,4|tiger160,3|tiger160,4|tiger192,3|haval128,3|haval160,3|haval128,4|haval160,4|haval128,5|haval160,5|adler32|crc32|crc32b|crc32c|fnv132|fnv1a32|fnv164|fnv1a64|joaat|murmur3a|murmur3c|murmur3f|xxh32|xxh64|xxh128)\""
            )
          ) ++
          cpg.call.name("(?i)(md5|md5_file|sha1|sha1_file)")

        def sink = cpg.call.name(Operators.equals, Operators.notEquals).argument

        sink.reachableBy(hash).argument(2).reachableBy(source)
      }),
      tags = List(QueryTags.magicHash, QueryTags.default)
    )
}
