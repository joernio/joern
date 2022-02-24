package io.joern.joerncli

import better.files.File
import io.joern.{console, joerncli}
import io.joern.joerncli.console.JoernConsole
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RunScriptTests extends AnyWordSpec with Matchers with AbstractJoernCliTest {

  "Executing scripts for example code 'testcode/unsafe-ptr" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/unsafe-ptr"))
  ) { case (cpg: Cpg, _) =>
    "work correctly for 'pointer-to-int.sc'" in {
      val calls =
        joerncli.console.JoernConsole.runScriptTest("c/pointer-to-int.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.code) should contain theSameElementsAs List(
        "simple_subtraction = p - q",
        "nested_subtraction = p - q - r",
        "literal_subtraction = p - i",
        "addrOf_subtraction = p - &i",
        "nested_addrOf_subtraction =  3 - &i - 4",
        "literal_addrOf_subtraction = 3 - &i",
        "array_subtraction = x - p",
        "array_literal_subtraction = x - 3",
        "array_addrOf_subtraction = x - &i"
        // TODO: We don't have access to type info for indirect field member access.
        // "unsafe_struct = foo_t->p - 1"
      )
    }
  }

  "Executing scripts for example code 'testcode/syscalls" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/syscalls"))
  ) { case (cpg: Cpg, _) =>
    "work correctly for 'syscalls.sc'" in {
      val calls =
        joerncli.console.JoernConsole.runScriptTest("c/syscalls.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.name) should contain theSameElementsAs List("gettimeofday", "exit")
    }

    "work correctly for 'userspace-memory-access.sc'" in {
      val calls =
        joerncli.console.JoernConsole
          .runScriptTest("c/userspace-memory-access.sc", Map.empty, cpg)
          .asInstanceOf[List[Call]]

      calls.map(_.name) should contain theSameElementsAs List("get_user")
    }
  }

  "Executing scripts for example code 'testcode/malloc-overflow" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/malloc-overflow"))
  ) { case (cpg: Cpg, _) =>
    "work correctly for 'malloc-overflow.sc'" in {
      val calls =
        joerncli.console.JoernConsole.runScriptTest("c/malloc-overflow.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.code) should contain theSameElementsAs List(
        "malloc(sizeof(int) * 42)",
        "malloc(sizeof(int) * 3)",
        "malloc(sizeof(int) + 55)"
      )
    }
  }

  "Executing scripts for example code 'testcode/leak" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/leak"))
  ) { case (cpg: Cpg, _) =>
    "work correctly for 'malloc-leak.sc'" in {
      val calls =
        joerncli.console.JoernConsole.runScriptTest("c/malloc-leak.sc", Map.empty, cpg).asInstanceOf[Set[String]]

      calls should contain theSameElementsAs Set("leak")
    }
  }

  "Executing scripts for example code 'testcode/const-ish" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/const-ish"))
  ) { case (cpg: Cpg, _) =>
    "work correctly for 'const-ish.sc'" in {
      val methods =
        JoernConsole.runScriptTest("c/const-ish.sc", Map.empty, cpg).asInstanceOf[Set[Method]]

      // "side_effect_number" is included here as we are simply trying to emulate a side effect.
      methods.map(_.name) should contain theSameElementsAs
        Set(
          "modify_const_struct_member_cpp_cast",
          "modify_const_struct_member_c_cast",
          "modify_const_struct_cpp_cast",
          "modify_const_struct_c_cast"
        )
    }
  }

  "Executing scripts for example code 'testcode/free'" should withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/free"))
  ) { case (cpg: Cpg, _) =>
    "work correctly for 'list-funcs'" in {
      val expected = cpg.method.name.l
      val actual   = joerncli.console.JoernConsole.runScriptTest("general/list-funcs.sc", Map.empty, cpg)
      actual shouldBe expected
    }

  }

}
