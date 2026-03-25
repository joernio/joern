package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

/** End-to-end validation: realistic Go code exercising all Phase 1-3 fixes */
class E2EGoValidationTests extends GoCodeToCpgSuite {

  val cpg = code("""
      |package main
      |
      |import (
      |	"fmt"
      |	"os"
      |)
      |
      |type Reader interface {
      |	Read(p []byte) (int, error)
      |}
      |
      |type FileReader struct {
      |	path string
      |}
      |
      |func (f *FileReader) Read(p []byte) (int, error) {
      |	return len(p), nil
      |}
      |
      |func divide(a, b float64) (float64, error) {
      |	if b == 0 {
      |		return 0, fmt.Errorf("division by zero")
      |	}
      |	return a / b, nil
      |}
      |
      |func main() {
      |	f, err := os.Open("test.txt")
      |	defer f.Close()
      |
      |	ch := make(chan string)
      |	done := make(chan bool)
      |
      |	ch <- "hello"
      |	go fmt.Println("in goroutine")
      |
      |	select {
      |	case msg := <-ch:
      |		fmt.Println(msg)
      |	case <-done:
      |		fmt.Println("done")
      |	}
      |
      |	var r Reader = &FileReader{path: "test.txt"}
      |	fr := r.(*FileReader)
      |	fmt.Println(fr.path)
      |
      |	result, err := divide(10.0, 3.0)
      |	fmt.Println(result, err)
      |
      |	switch 2 {
      |	case 1:
      |		fmt.Println("one")
      |	case 2:
      |		fmt.Println("two")
      |		fallthrough
      |	case 3:
      |		fmt.Println("three")
      |	}
      |}
      |""".stripMargin)

  "defer statement" should {
    "produce a call node for Close" in {
      cpg.call.name("Close").size should be >= 1
    }
  }

  "go statement with channel send" should {
    "produce send operator calls" in {
      cpg.call.name("<operator>.send").size should be >= 1
    }

    "have channel and value as arguments" in {
      val sends = cpg.call.name("<operator>.send").l
      sends.foreach(_.argument.size shouldBe 2)
    }
  }

  "select statement" should {
    "produce a control structure" in {
      val selects = cpg.method.name("main").controlStructure.l
        .filter(_.code == "select")
      selects.size shouldBe 1
      selects.head.controlStructureType shouldBe ControlStructureTypes.SWITCH
    }
  }

  "tuple return type" should {
    "represent multiple returns as (type1, type2)" in {
      val divideReturn = cpg.method.name("divide").methodReturn.typeFullName.head
      divideReturn shouldBe "(float64, error)"
    }
  }

  "fallthrough" should {
    "produce a control structure node" in {
      cpg.controlStructure.controlStructureType("fallthrough").size shouldBe 1
    }
  }

  "type assertion" should {
    "produce a cast operator call" in {
      val casts = cpg.call.name(Operators.cast).l
      casts.size should be >= 1
    }

    "have the asserted type as typeFullName" in {
      val casts = cpg.call.name(Operators.cast).l
      casts.exists(_.typeFullName.contains("FileReader")) shouldBe true
    }
  }

  "interface declaration" should {
    "create a TypeDecl for Reader" in {
      cpg.typeDecl.name("Reader").size shouldBe 1
    }
  }
}
