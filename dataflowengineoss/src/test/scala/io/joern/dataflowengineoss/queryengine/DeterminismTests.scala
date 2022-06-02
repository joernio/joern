package io.joern.dataflowengineoss.queryengine

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}

class DeterminismTests extends AnyWordSpec with Matchers {

  val semanticsFile: String = ProjectRoot.relativise("dataflowengineoss/src/test/resources/default.semantics")
  lazy val defaultSemantics: Semantics           = Semantics.fromList(new Parser().parseFile(semanticsFile))
  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext(defaultSemantics, EngineConfig(maxCallDepth = 6))

  private val fooCpgPath = ProjectRoot.relativise("dataflowengineoss/src/test/resources/Foo.bin")
  private val fooCpg     = Cpg.withStorage(fooCpgPath)

  "entries should maintain the same order within the cache for query" in {
    def src = fooCpg.call("parse").argument(1)
    def snk = fooCpg.call("println")

    def query = snk.reachableByDetailed(src)

    var t0 = System.nanoTime()
    val r1 = query
    t0 = System.nanoTime() - t0
    var t1 = System.nanoTime()
    val r2 = query
    t1 = System.nanoTime() - t1
    val r1Paths = r1.map(_.path)
    val r2Paths = r2.map(_.path)

    // Would consistently return 10
    r1Paths.size shouldBe 10
    r1Paths.size shouldBe r2Paths.size
    // Since we're re-using the cache, should have run much faster the second time around
    t0 should be > t1

    r1Paths.zip(r2Paths).foreach { case (p1, p2) =>
      p1.size shouldBe p2.size
      p1.zip(p2).foreach { case (p1e, p2e) =>
        p1e.node.id() shouldBe p2e.node.id()
      }
    }
  }

//  Vector(reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out2, out2, writer.println(out2))
//  Vector(reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out1, out1, writer.println(out1))
//  Vector(reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out3, out3, writer.println(out3))
//  Vector(reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out4, out4, writer.println(out4))
//  Vector(reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out2, out2, writer.println(out2))
//  Vector(reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out3, out3, writer.println(out3))
//  Vector(reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out1, out1, writer.println(out1))
//  Vector(reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out4, out4, writer.println(out4))
//  Vector(reader, reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out2, out2, writer.println(out2))
//  Vector(reader, reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out4, out4, writer.println(out4))
//  Vector(reader, reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out1, out1, writer.println(out1))
//  Vector(reader, reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out3, out3, writer.println(out3))
//  Vector(reader, reader, reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out2, out2, writer.println(out2))
//  Vector(reader, reader, reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out4, out4, writer.println(out4))
//  Vector(reader, reader, reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out1, out1, writer.println(out1))
//  Vector(reader, reader, reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out3, out3, writer.println(out3))

  /* Straight forward flows
  Vector(reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out1, out1, writer.println(out1))
  Vector(reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out3, out3, writer.println(out3))
  Vector(reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out2, out2, writer.println(out2))
  Vector(reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out4, out4, writer.println(out4))
   */
  /* Flows by proxy
  Vector(reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out2, out2, writer.println(out2))
  Vector(reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out4, out4, writer.println(out4))
  Vector(reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out3, out3, writer.println(out3))
  Vector(reader, reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out3, out3, writer.println(out3))
  Vector(reader, reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out4, out4, writer.println(out4))
  Vector(reader, reader, reader, reader, BufferedReader reader, reader, reader, reader.readLine(), line, line, return line;, java.lang.String, parse(reader), out4, out4, writer.println(out4))
   */

}
