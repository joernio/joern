package io.joern.ghidra2cpg.querying.mips

import io.joern.ghidra2cpg.fixtures.GhidraBinToCpgSuite
import io.shiftleft.semanticcpg.language.*

class CallArgumentsTest extends GhidraBinToCpgSuite {

  override def beforeAll(): Unit = {
    super.beforeAll()
    buildCpgForBin("linux/mips/mips32_memcpy_test.bin")
  }
  /*
    // Test code is:
    void test(char *input, int size) {
      char arr[168];
      char *chr = (char *)memcpy(arr,input + 5,size - 5);
    }
    int main() {
      char *foo = "abcdefghij";
      int size = strlen(foo);
      printf("%s\n", foo);
      test(foo,size);
    }
   */
  "The call to 'memcpy' should have three arguments " in {
    cpg.call
      .name("memcpy")
      .argument
      .code
      .l shouldBe List("auStack_b4", "param_1 + 0x5", "param_2 - 0x5")

  }
  "The call to 'test' should have two arguments " in {
    cpg.call
      .name("test")
      .argument
      .code
      .l shouldBe List("abcdefghij", "sVar1")
  }

  "The call to 'strlen' should have two arguments " in {
    cpg.call
      .name("strlen")
      .argument
      .code
      .l shouldBe List("abcdefghij")
  }

  "The call to 'puts' in 'main' should have 'abcdefghij' arguments " in {
    val a = cpg.method.name("main").call.name("puts").argument.code.l
    a shouldBe List("abcdefghij")
  }
  "The call to 'puts' in 'test' should have '__s' arguments " in {
    val __s = cpg.method.name("test").call.name("puts").argument.code.l
    __s shouldBe List("__s")
  }
}
