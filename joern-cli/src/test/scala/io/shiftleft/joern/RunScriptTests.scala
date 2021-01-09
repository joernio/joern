package io.shiftleft.joern

import better.files.File
import better.files.Dsl._

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}

class RunScriptTests extends AnyWordSpec with Matchers with AbstractJoernCliTest {

  private def withCpgZip[T](file: File)(f: Cpg => T): T = {
    val cpgZip = File(".") / "cpg.bin"
    withTestCpg(file) {
      case (cpg, outputFilename) =>
        cp(File(outputFilename), cpgZip)
        try {
          f(cpg)
        } finally {
          rm(cpgZip)
        }
    }
  }

  "Executing scripts for example code 'testcode/unsafe-ptr" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/unsafe-ptr"))) { cpg: Cpg =>
    "work correctly for 'pointer-to-int.sc'" in {
      val calls =
        console.JoernConsole.runScriptTest("c/pointer-to-int.sc", Map.empty, cpg).asInstanceOf[List[Call]]

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

  "Executing scripts for example code 'testcode/syscalls" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/syscalls"))) { cpg: Cpg =>
    "work correctly for 'syscalls.sc'" in {
      val calls =
        console.JoernConsole.runScriptTest("c/syscalls.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.name) should contain theSameElementsAs List(
        "gettimeofday",
        "exit"
      )
    }

    "work correctly for 'userspace-memory-access.sc'" in {
      val calls =
        console.JoernConsole.runScriptTest("c/userspace-memory-access.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.name) should contain theSameElementsAs List(
        "get_user"
      )
    }
  }

  "Executing scripts for example code 'testcode/malloc-overflow" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/malloc-overflow"))) { cpg: Cpg =>
    "work correctly for 'malloc-overflow.sc'" in {
      val calls =
        console.JoernConsole.runScriptTest("c/malloc-overflow.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.code) should contain theSameElementsAs List(
        "malloc(sizeof(int) * 42)",
        "malloc(sizeof(int) * 3)",
        "malloc(sizeof(int) + 55)"
      )
    }
  }

  "Executing scripts for example code 'testcode/leak" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/leak"))) { cpg: Cpg =>
    "work correctly for 'malloc-leak.sc'" in {
      val calls =
        console.JoernConsole.runScriptTest("c/malloc-leak.sc", Map.empty, cpg).asInstanceOf[Set[String]]

      calls should contain theSameElementsAs Set("leak")
    }
  }

  "Executing scripts for example code 'testcode/const-funcs" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/const-funcs"))) { cpg: Cpg =>
    "work correctly for 'const-funcs.sc'" in {
      val methods =
        console.JoernConsole.runScriptTest("c/const-funcs.sc", Map.empty, cpg).asInstanceOf[Set[Method]]

      // "side_effect_number" is included here as we are simply trying to emulate a side effect.
      methods.map(_.name) should contain theSameElementsAs
        Set("eligible", "eligible_params", "side_effect_number")
    }
  }

  "Executing scripts for example code 'testcode/const-ish" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/const-ish"))) { cpg: Cpg =>
    "work correctly for 'const-ish.sc'" in {
      val methods =
        console.JoernConsole.runScriptTest("c/const-ish.sc", Map.empty, cpg).asInstanceOf[Set[Method]]

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

  "Executing scripts for example code 'testcode/free'" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/free"))) { cpg: Cpg =>
    "work correctly for 'list-funcs'" in {
      val expected = cpg.method.name.l
      val actual = console.JoernConsole.runScriptTest("general/list-funcs.sc", Map.empty, cpg)
      actual shouldBe expected
    }

  }

}
