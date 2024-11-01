package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class MethodTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  "Data flow through class method" in {
    val cpg = code("""
                     |class MyClass
                     |  def print(text)
                     |    puts text
                     |  end
                     |end
                     |
                     |
                     |x = "some text"
                     |inst = MyClass.new
                     |inst.print(x)
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").argument.l
    sink.reachableByFlows(src).size shouldBe 4
  }

  "Data flow through do-while loop" in {
    val cpg = code("""
                     |x = 0
                     |num = -1
                     |loop do
                     |   num = x + 1
                     |   x = x + 1
                     |   if x > 10
                     |     break
                     |   end
                     |end
                     |puts num
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 5
  }

  "Data flow through methodOnlyIdentifier usage" in {
    val cpg = code("""
                     |x = 1
                     |y = SomeConstant! + x
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  // Works in deprecated
  "Data flow through blockExprAssocTypeArguments" in {
    val cpg = code("""
                     |def foo(*args)
                     |puts args
                     |end
                     |
                     |x = "value1"
                     |foo(key1: x, key2: "value2")
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").argument.l
    sink.reachableByFlows(source).size shouldBe 4
  }

  "Data flow through blockSplattingTypeArguments" in {
    val cpg = code("""
                     |def foo(arg)
                     |puts arg
                     |end
                     |
                     |x = 1
                     |foo(*x)
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").argument.l
    sink.reachableByFlows(source).size shouldBe 4
  }

  "Data flow through blockSplattingExprAssocTypeArguments without block" in {
    val cpg = code("""
                     |def foo(*arg)
                     |puts arg
                     |end
                     |
                     |x = 1
                     |foo( x+1, key1: x*2, key2: x*3 )
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").argument.l
    sink.reachableByFlows(source).size shouldBe 4
  }

  "Data flow through blockSplattingTypeArguments without block" in {
    val cpg = code("""
                     |def foo (blockArg,&block)
                     |block.call(blockArg)
                     |end
                     |
                     |x = 10
                     |foo(*x do |arg|
                     |  y = x + arg
                     |  puts y
                     |end
                     |)
                     |
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").argument.l
    sink.reachableByFlows(source).size shouldBe 2
  }

  "Data flow through blockExprAssocTypeArguments with block argument in the wrapper function" ignore {
    val cpg = code("""
                     |def foo(blockArg, &block)
                     |  block.call(blockArg)
                     |end
                     |
                     |def foo_wrap(blockArg, &block)
                     |  foo(blockArg, &block)
                     |end
                     |
                     |x = 10
                     |foo_wrap x do |arg|
                     |  y = 100 + arg
                     |  puts y
                     |end
                     |""".stripMargin)

    val source = cpg.literal.code("10").l
    val sink   = cpg.call.name("puts").argument(1).l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // TODO:
  "Data flow through overloaded operator method" ignore {
    val cpg = code("""
                     |class Foo
                     |    @@x = 1
                     |    def +(y)
                     |        @@x + y
                     |    end
                     |end
                     |
                     |y = Foo.new + 1
                     |
                     |puts y
                     |""".stripMargin)

    val source = cpg.member.name("@@x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 3
  }

  // TODO:
  // Works in deprecated
  "Data flow through assignment-like method identifier" ignore {
    val cpg = code("""
                     |class Foo
                     |    @@x = 1
                     |    def CONST=(y)
                     |        return @@x == y
                     |    end
                     |end
                     |puts Foo::CONST= 2
                     |""".stripMargin)

    val source = cpg.member.name("@@x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 3
  }

  // Works in deprecated
  "Data flow through block argument context" ignore {
    val cpg = code("""
                     |x=10
                     |y=0
                     |def foo(n, &block)
                     |   woo(n, &block)
                     |end
                     |
                     |def woo(n, &block)
                     |    n.times {yield}
                     |end
                     |
                     |foo(5) {
                     |    y = x
                     |}
                     |
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }

  // Works in deprecated
  "Flow through tainted object" ignore {
    val cpg = code("""
                     |def put_req(api_endpoint, params)
                     |    puts "Hitting " + api_endpoint + " with params: " + params
                     |end
                     |class TestClient
                     |    def get_event_data(accountId)
                     |        payload = accountId
                     |        r = put_req(
                     |            "https://localhost:8080/v3/users/me/",
                     |            params=payload
                     |        )
                     |    end
                     |end
                     |""".stripMargin)

    val source = cpg.identifier.name("accountId").l
    val sink   = cpg.call.name("put_req").l
    sink.reachableByFlows(source).size shouldBe 1
  }

  // Works in deprecated
  "flow through endless method" ignore {
    val cpg = code("""
                     |def multiply(a,b) = a*b
                     |x = 10
                     |y = multiply(3,x)
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).size shouldBe 2
  }
}
