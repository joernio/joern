package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.generated.{NodeTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers

/** End-to-end validation: realistic Python code exercising all Phase 1-3 fixes */
class E2EPythonValidationTests extends PySrc2CpgFixture(withOssDataflow = false) with Matchers {

  lazy val cpg = code(
    """from typing import Optional, List, Union
      |
      |# Type hints on function params and return
      |def greet(name: str) -> str:
      |    return "Hello, " + name
      |
      |def add_numbers(a: int, b: int) -> int:
      |    return a + b
      |
      |# Optional type hint
      |def find_user(user_id: Optional[int] = None) -> Optional[str]:
      |    if user_id is None:
      |        return None
      |    return f"User {user_id}"
      |
      |# **kwargs usage
      |def send_request(**kwargs):
      |    url = kwargs.get("url", "")
      |    return url
      |
      |def dispatch(target, **options):
      |    return send_request(**options)
      |
      |# Walrus operator
      |def check_values(items):
      |    if (n := len(items)) > 10:
      |        print(f"Too many: {n}")
      |    return n
      |
      |# Match statement with patterns
      |def handle_command(cmd):
      |    match cmd:
      |        case 42:
      |            return "the answer"
      |        case [first, *rest]:
      |            return f"list: {first}"
      |        case _:
      |            return "unknown"
      |
      |# List comprehension with filter
      |filtered = [x * 2 for x in range(20) if x > 5]
      |
      |# Calls
      |result = greet("world")
      |total = add_numbers(1, 2)
      |user = find_user(42)
      |""".stripMargin,
    "test.py"
  )

  "type hints on parameters" should {
    "set typeFullName from annotation" in {
      val nameParam = cpg.method("greet").parameter.nameExact("name").head
      nameParam.typeFullName shouldBe "__builtin.str"
    }

    "set int type from annotation" in {
      val aParam = cpg.method("add_numbers").parameter.nameExact("a").head
      aParam.typeFullName shouldBe "__builtin.int"
    }
  }

  "Optional type hint" should {
    "resolve Optional[int] to int|None" in {
      val param = cpg.method("find_user").parameter.nameExact("user_id").head
      param.typeFullName shouldBe "__builtin.int|__builtin.None"
    }
  }

  "return type hints" should {
    "set method return typeFullName" in {
      cpg.method("greet").methodReturn.typeFullName.head shouldBe "__builtin.str"
    }

    "set Optional return type" in {
      cpg.method("find_user").methodReturn.typeFullName.head shouldBe "__builtin.str|__builtin.None"
    }
  }

  "**kwargs handling" should {
    "preserve unpacked dict as argument" in {
      // dispatch calls send_request(**options)
      val sendCalls = cpg.call.codeExact("send_request(**options)").l
      sendCalls should not be empty
      val kwargsArgs = sendCalls.head.argument.isIdentifier.nameExact("options").l
      kwargsArgs should not be empty
    }
  }

  "walrus operator" should {
    "create an assignment used as expression in condition" in {
      val assigns = cpg.call.methodFullName(Operators.assignment).codeExact("n = len(items)").l
      assigns should not be empty
    }
  }

  "match statement" should {
    "create control structure with MATCH type" in {
      cpg.controlStructure.controlStructureType("MATCH").size shouldBe 1
    }

    "have literal 42 as pattern AST node in case block" in {
      val matchStmt = cpg.controlStructure.controlStructureType("MATCH").head
      val bodyBlock = matchStmt.astChildren.order(2).head
      // First case block should contain literal 42
      val blocks = bodyBlock.astChildren.isBlock.l
      val hasLiteral42 = blocks.exists(_.astChildren.code.l.contains("42"))
      hasLiteral42 shouldBe true
    }

    "have default jump target" in {
      cpg.jumpTarget.nameExact("default").size should be >= 1
    }
  }

  "list comprehension" should {
    "create nodes for the comprehension" in {
      // The comprehension creates a range call and multiplication
      cpg.call.name("range").size should be >= 1
    }
  }

  "type recovery from hints" should {
    "propagate return type to call site" in {
      // result = greet("world") should give result type __builtin.str
      val resultTypes = cpg.identifier("result").typeFullName.toSet
      resultTypes should contain("__builtin.str")
    }
  }
}
