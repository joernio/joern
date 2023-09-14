package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.edges.Ref
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  Block,
  Call,
  ControlStructure,
  FieldIdentifier,
  Identifier,
  Literal,
  Local,
  Return
}
import io.shiftleft.semanticcpg.language.*

import scala.jdk.CollectionConverters.*

class NewControlStructureTests extends JavaSrcCode2CpgFixture {

  "try with multiple catches and finally" should {
    val cpg = code("""
        |public class Foo {
        |  static void foo() {
        |    try { foo(); }
        |    catch (SomeException x1) { x1(); }
        |    catch (OtherException x2) { x2(); }
        |    finally { bar(); }
        |  }
        |}
        |""".stripMargin)

    "create correct control structures" in {
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        val List(tryBlock) = t.astChildren.isBlock.l
        tryBlock.order shouldBe 1
        tryBlock.astChildren.isCall.code.l shouldBe List("foo()")
        val List(catchX1, catchX2) = t.astChildren.isControlStructure.isCatch.l
        catchX1.order shouldBe 2
        catchX1.astChildren.isBlock.astChildren.isCall.code.l shouldBe List("x1()")
        catchX2.order shouldBe 3
        catchX2.astChildren.isBlock.astChildren.isCall.code.l shouldBe List("x2()")
        val List(finallyNode) = t.astChildren.isControlStructure.isFinally.l
        finallyNode.order shouldBe 4
        finallyNode.astChildren.isBlock.astChildren.isCall.code.l shouldBe List("bar()")
      }
    }
  }

  "try-with-resource blocks" should {
    val cpg = code("""
		|import java.io.FileReader;
        |import java.io.IOException;
        |import java.io.BufferedReader;
	    |
        |public class Foo {
        |    static String foo(String path) throws IOException {
        |        try (FileReader fr = new FileReader(path);
        |             BufferedReader br = new BufferedReader(fr)) {
        |            return br.readLine();
        |        }
        |    }
        |}
        |""".stripMargin)

    "create nodes for resources" in {
      cpg.method.name("foo").body.astChildren.l match {
        case List(
              frLocal: Local,
              frAssign: Call,
              frInit: Call,
              brLocal: Local,
              brAssign: Call,
              brInit: Call,
              tryBlock: ControlStructure
            ) =>
          frLocal.name shouldBe "fr"
          frLocal.code shouldBe "FileReader fr"
          frLocal.typeFullName shouldBe "java.io.FileReader"

          frAssign.name shouldBe Operators.assignment
          val List(frAssignLhs: Identifier, frAssignRhs: Call) = frAssign.argument.l: @unchecked
          frAssignLhs.name shouldBe "fr"
          frAssignLhs.typeFullName shouldBe "java.io.FileReader"
          frAssignRhs.name shouldBe Operators.alloc
          frAssignRhs.typeFullName shouldBe "java.io.FileReader"

          frInit.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
          val List(frInitThis: Identifier, frInitArg: Identifier) = frInit.argument.l: @unchecked
          frInitThis.name shouldBe "fr"
          frInitThis.typeFullName shouldBe "java.io.FileReader"
          frInitArg.name shouldBe "path"
          frInitArg.typeFullName shouldBe "java.lang.String"

          brLocal.name shouldBe "br"
          brLocal.code shouldBe "BufferedReader br"
          brLocal.typeFullName shouldBe "java.io.BufferedReader"

          brAssign.name shouldBe Operators.assignment
          val List(brAssignLhs: Identifier, brAssignRhs: Call) = brAssign.argument.l: @unchecked
          brAssignLhs.name shouldBe "br"
          brAssignLhs.typeFullName shouldBe "java.io.BufferedReader"
          brAssignRhs.name shouldBe Operators.alloc
          brAssignRhs.typeFullName shouldBe "java.io.BufferedReader"

          brInit.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
          val List(brInitThis: Identifier, brInitArg: Identifier) = brInit.argument.l: @unchecked
          brInitThis.name shouldBe "br"
          brInitThis.typeFullName shouldBe "java.io.BufferedReader"
          brInitArg.name shouldBe "fr"
          brInitArg.typeFullName shouldBe "java.io.FileReader"

          tryBlock.controlStructureType shouldBe ControlStructureTypes.TRY
          tryBlock.astChildren.l match {
            case List(block: Block) =>
              val List(returnStmt: Return) = block.astChildren.l: @unchecked
              returnStmt.code shouldBe "return br.readLine();"

            case result => fail(s"Expected single block as try body but got $result")
          }

        case result => fail(s"Expected resource assignments before try but got $result")
      }
    }
  }

  "foreach loops over arrays imported through static imports" should {
    val cpg = code("""
        |import static Bar.STATIC_ARR;
        |public class Foo {
        |  public static void sink(String s) {}
        |
        |  public static void foo() {
        |    for (String s : STATIC_ARR) {
        |      sink(s);
        |    }
        |  }
        |}
        |""".stripMargin)
      .moreCode(
        """
        |public class Bar {
        |  public static String[] STATIC_ARR = new String[10];
        |}
        |""".stripMargin,
        fileName = "Bar.java"
      )

    "have the correct assignment target in the clinit block of the defining class" in {
      inside(cpg.typeDecl.name("Bar").method.nameExact("<clinit>").body.astChildren.l) { case List(assignment: Call) =>
        assignment.name shouldBe Operators.assignment

        inside(assignment.argument.l) { case List(fieldAccess: Call, _: Call) =>
          fieldAccess.name shouldBe Operators.fieldAccess
          fieldAccess.typeFullName shouldBe "java.lang.String[]"

          inside(fieldAccess.argument.l) { case List(barIdentifier: Identifier, staticArr: FieldIdentifier) =>
            barIdentifier.name shouldBe "Bar"
            barIdentifier.typeFullName shouldBe "Bar"

            staticArr.canonicalName shouldBe "STATIC_ARR"
          }
        }
      }
    }

    "not create REF edges from the STATIC_ARR identifiers to the import identifier used only during AST generation" in {
      cpg.typeDecl.name("Foo").ast.isIdentifier.name("STATIC_ARR").outE.collectAll[Ref].isEmpty shouldBe true
    }
  }

  "foreach loops over native array initialization expressions" should {
    val cpg = code("""
       |public class Foo {
       |  public static void sink(String s) {}
       |
       |  public static void foo() {
       |    for (String item : new String[] {"a", "b", "c"}) {
       |      sink(item);
       |    }
       |  }
       |}
       |""".stripMargin)

    "create a local node for the array" in {
      val local = cpg.method.name("foo").local.nameExact("$iterLocal0").l match {
        case List(local) => local
        case result      => fail(s"Expected single iterator local but got $result")
      }

      local.typeFullName shouldBe "java.lang.String[]"
      local.order shouldBe 1
    }

    "assign the array to the created local" in {
      val assignment = cpg.method
        .name("foo")
        .assignment
        .find { assignment =>
          assignment.argument.l match {
            case List(identifier: Identifier, _: Call) if identifier.name == "$iterLocal0" => true
            case _                                                                         => false
          }
        } match {
        case Some(iterAssign) => iterAssign
        case result           => fail(s"Expected an assign to iterLocal in method but got $result")
      }

      assignment.name shouldBe Operators.assignment
      assignment.methodFullName shouldBe Operators.assignment
      assignment.order shouldBe 2
      assignment.typeFullName shouldBe "java.lang.String[]"

      val (iterIdentifier, arrayInitializer) = assignment.argument.l match {
        case List(iterIdentifier: Identifier, arrayAlloc: Call) => (iterIdentifier, arrayAlloc)
        case result => fail(s"Expected arrayInitializer assign to iterLocal but got $result")
      }

      iterIdentifier.name shouldBe "$iterLocal0"
      iterIdentifier.typeFullName shouldBe "java.lang.String[]"
      iterIdentifier.order shouldBe 1
      iterIdentifier.argumentIndex shouldBe 1
      iterIdentifier.refOut.toSet should contain(cpg.local.nameExact("$iterLocal0").head)

      arrayInitializer.name shouldBe Operators.arrayInitializer
      arrayInitializer.methodFullName shouldBe Operators.arrayInitializer
      arrayInitializer.order shouldBe 2
      arrayInitializer.argumentIndex shouldBe 2
      arrayInitializer.astChildren.size shouldBe 3
      val expectedLiterals = List("\"a\"", "\"b\"", "\"c\"")
      arrayInitializer.astChildren.zip(expectedLiterals).foreach { case (initChild, expectedCode) =>
        initChild shouldBe a[Literal]
        initChild.asInstanceOf[Literal].code shouldBe expectedCode
      }
    }

    "create a local node for idx" in {
      val local = cpg.controlStructure.astChildren.l match {
        case List(local: Local, _, _, _, _) => local
        case result                         => fail(s"Expected local as first AST child but got $result")
      }
      local.name shouldBe "$idx0"
      local.typeFullName shouldBe "int"
      local.order shouldBe 1
    }

    "initialize idx to 0" in {
      val (idxLocal, initializer) = cpg.controlStructure.astChildren.l match {
        case List(itemLocal: Local, initializer: Call, _, _, _) => (itemLocal, initializer)
        case result => fail(s"Expected initializer as second ast child but got $result")
      }

      initializer.name shouldBe Operators.assignment
      initializer.methodFullName shouldBe Operators.assignment
      initializer.typeFullName shouldBe "int"
      initializer.order shouldBe 2

      initializer.argument.l match {
        case List(idx: Identifier, zeroLiteral: Literal) =>
          idx.name shouldBe "$idx0"
          idx.typeFullName shouldBe "int"
          idx.order shouldBe 1
          idx.argumentIndex shouldBe 1
          idx.refOut.toSet should contain(idxLocal)

          zeroLiteral.code shouldBe "0"
          zeroLiteral.typeFullName shouldBe "int"
          zeroLiteral.order shouldBe 2
          zeroLiteral.argumentIndex shouldBe 2

        case result => fail(s"Expected args for idx = 0 but got $result")
      }
    }

    "compare idx to input array size" in {
      val (idxLocal, condition) = cpg.controlStructure.astChildren.l match {
        case List(idxLocal: Local, _, conditionCall: Call, _, _) => (idxLocal, conditionCall)
        case result => fail(s"Expected condition call as third AST child but got $result")
      }

      condition.name shouldBe Operators.lessThan
      condition.methodFullName shouldBe Operators.lessThan
      condition.typeFullName shouldBe "boolean"
      condition.order shouldBe 3

      condition.argument.l match {
        case List(idx: Identifier, arraySize: Call) =>
          idx.name shouldBe "$idx0"
          idx.typeFullName shouldBe "int"
          idx.order shouldBe 1
          idx.argumentIndex shouldBe 1
          idx.refOut.toSet should contain(idxLocal)

          arraySize.name shouldBe Operators.fieldAccess
          arraySize.typeFullName shouldBe "int"
          arraySize.order shouldBe 2
          arraySize.argumentIndex shouldBe 2

          arraySize.argument.l match {
            case List(items: Identifier, length: FieldIdentifier) =>
              items.name shouldBe "$iterLocal0"
              items.typeFullName shouldBe "java.lang.String[]"
              items.order shouldBe 1
              items.argumentIndex shouldBe 1
              items.refOut.toSet should contain(cpg.method.name("foo").local.nameExact("$iterLocal0").head)

              length.code shouldBe "length"
              length.order shouldBe 2
              length.argumentIndex shouldBe 2

            case result => fail(s"Expected array.length field access but got $result")
          }

        case result => fail(s"Expected idx < array.length args but got $result")
      }
    }

    "update idx on each loop" in {
      val (idxLocal, update) = cpg.controlStructure.astChildren.l match {
        case List(idxLocal: Local, _, _, update: Call, _) => (idxLocal, update)
        case result                                       => fail(s"Expected update as 4th AST child but got $result")
      }

      update.name shouldBe Operators.postIncrement
      update.typeFullName shouldBe "int"
      update.order shouldBe 4

      update.argument.l match {
        case List(idx: Identifier) =>
          idx.name shouldBe "$idx0"
          idx.typeFullName shouldBe "int"
          idx.order shouldBe 1
          idx.argumentIndex shouldBe 1
          idx.refOut.toSet should contain(idxLocal)

        case result => fail(s"Expected single argument to ++ call but got $result")
      }
    }

    "create an assignment to the `item` local in the FOR body" in {
      val (idxLocal, body) = cpg.controlStructure.astChildren.l match {
        case List(idxLocal: Local, _, _, _, body: Block) => (idxLocal, body)
        case result                                      => fail(s"Expected body as 5th AST child but got $result")
      }

      val (itemLocal: Local, itemAssign: Call, sink: Call) = body.astChildren.l match {
        case List(itemLocal: Local, itemAssign: Call, sink: Call) => (itemLocal, itemAssign, sink)
        case result => fail(s"Expected local, assign and sink, but got $result")
      }

      itemLocal.name shouldBe "item"
      itemLocal.typeFullName shouldBe "java.lang.String"
      itemLocal.order shouldBe 1

      itemAssign.name shouldBe Operators.assignment
      itemAssign.typeFullName shouldBe "java.lang.String"
      itemAssign.order shouldBe 2
      itemAssign.argument.l match {
        case List(itemIdentifier: Identifier, indexAccess: Call) =>
          itemIdentifier.name shouldBe "item"
          itemIdentifier.typeFullName shouldBe "java.lang.String"
          itemIdentifier.order shouldBe 1
          itemIdentifier.argumentIndex shouldBe 1
          itemIdentifier.refOut.toSet should contain(itemLocal)

          indexAccess.name shouldBe Operators.indexAccess
          indexAccess.typeFullName shouldBe "java.lang.String"
          indexAccess.order shouldBe 2
          indexAccess.argumentIndex shouldBe 2
          val (iterLocal: Identifier, idx: Identifier) = (indexAccess.argument.l match {
            case List(items: Identifier, idx: Identifier) => (items, idx)
            case result                                   => s"Expected iterLocal0[idx] args but got $result"
          }): @unchecked
          iterLocal.name shouldBe "$iterLocal0"
          iterLocal.typeFullName shouldBe "java.lang.String[]"
          iterLocal.order shouldBe 1
          iterLocal.argumentIndex shouldBe 1
          iterLocal.refOut.toSet should contain(cpg.local.nameExact("$iterLocal0").head)

          idx.name shouldBe "$idx0"
          idx.typeFullName shouldBe "int"
          idx.order shouldBe 2
          idx.argumentIndex shouldBe 2
          idx.refOut.toSet should contain(idxLocal)
        case result => fail(s"Expected item = iterLocal0[idx] args but got $result")
      }

      sink.name shouldBe "sink"
      sink.methodFullName shouldBe "Foo.sink:void(java.lang.String)"
      sink.order shouldBe 3
      sink.argument.l match {
        case List(item: Identifier) =>
          item.name shouldBe "item"
          item.typeFullName shouldBe "java.lang.String"
          item.order shouldBe 1
          item.argumentIndex shouldBe 1
          item.refOut.toSet should contain(itemLocal)

        case result => fail(s"Expected single identifier argument to sink but got $result")
      }
    }
  }

  "foreach loops over native arrays" should {
    val cpg = code("""
        |public class Foo {
        |  public static void sink(String s) {}
        |
        |  public static void foo(String[] items) {
        |    for (String item : items) {
        |      sink(item);
        |    }
        |  }
        |}
        |""".stripMargin)

    "create a local node for idx" in {
      val local = cpg.controlStructure.astChildren.l match {
        case List(local: Local, _, _, _, _) => local
        case result                         => fail(s"Expected local as first AST child but got $result")
      }
      local.name shouldBe "$idx0"
      local.typeFullName shouldBe "int"
      local.order shouldBe 1
    }

    "initialize idx to 0" in {
      val (idxLocal, initializer) = cpg.controlStructure.astChildren.l match {
        case List(itemLocal: Local, initializer: Call, _, _, _) => (itemLocal, initializer)
        case result => fail(s"Expected initializer as second ast child but got $result")
      }

      initializer.name shouldBe Operators.assignment
      initializer.methodFullName shouldBe Operators.assignment
      initializer.typeFullName shouldBe "int"
      initializer.order shouldBe 2

      initializer.argument.l match {
        case List(idx: Identifier, zeroLiteral: Literal) =>
          idx.name shouldBe "$idx0"
          idx.typeFullName shouldBe "int"
          idx.order shouldBe 1
          idx.argumentIndex shouldBe 1
          idx.refOut.toSet should contain(idxLocal)

          zeroLiteral.code shouldBe "0"
          zeroLiteral.typeFullName shouldBe "int"
          zeroLiteral.order shouldBe 2
          zeroLiteral.argumentIndex shouldBe 2

        case result => fail(s"Expected args for idx = 0 but got $result")
      }
    }

    "compare idx to input array size" in {
      val (idxLocal, condition) = cpg.controlStructure.astChildren.l match {
        case List(idxLocal: Local, _, conditionCall: Call, _, _) => (idxLocal, conditionCall)
        case result => fail(s"Expected condition call as third AST child but got $result")
      }

      condition.name shouldBe Operators.lessThan
      condition.methodFullName shouldBe Operators.lessThan
      condition.typeFullName shouldBe "boolean"
      condition.order shouldBe 3

      condition.argument.l match {
        case List(idx: Identifier, arraySize: Call) =>
          idx.name shouldBe "$idx0"
          idx.typeFullName shouldBe "int"
          idx.order shouldBe 1
          idx.argumentIndex shouldBe 1
          idx.refOut.toSet should contain(idxLocal)

          arraySize.name shouldBe Operators.fieldAccess
          arraySize.typeFullName shouldBe "int"
          arraySize.order shouldBe 2
          arraySize.argumentIndex shouldBe 2

          arraySize.argument.l match {
            case List(items: Identifier, length: FieldIdentifier) =>
              items.name shouldBe "items"
              items.typeFullName shouldBe "java.lang.String[]"
              items.order shouldBe 1
              items.argumentIndex shouldBe 1
              items.refOut.toSet should contain(cpg.parameter.name("items").head)

              length.code shouldBe "length"
              length.order shouldBe 2
              length.argumentIndex shouldBe 2

            case result => fail(s"Expected array.length field access but got $result")
          }

        case result => fail(s"Expected idx < array.length args but got $result")
      }
    }

    "update idx on each loop" in {
      val (idxLocal, update) = cpg.controlStructure.astChildren.l match {
        case List(idxLocal: Local, _, _, update: Call, _) => (idxLocal, update)
        case result                                       => fail(s"Expected update as 4th AST child but got $result")
      }

      update.name shouldBe Operators.postIncrement
      update.typeFullName shouldBe "int"
      update.order shouldBe 4

      update.argument.l match {
        case List(idx: Identifier) =>
          idx.name shouldBe "$idx0"
          idx.typeFullName shouldBe "int"
          idx.order shouldBe 1
          idx.argumentIndex shouldBe 1
          idx.refOut.toSet should contain(idxLocal)

        case result => fail(s"Expected single argument to ++ call but got $result")
      }
    }

    "create an assignment to the `item` local in the FOR body" in {
      val (idxLocal, body) = cpg.controlStructure.astChildren.l match {
        case List(idxLocal: Local, _, _, _, body: Block) => (idxLocal, body)
        case result                                      => fail(s"Expected body as 5th AST child but got $result")
      }

      val (itemLocal: Local, itemAssign: Call, sink: Call) = body.astChildren.l match {
        case List(itemLocal: Local, itemAssign: Call, sink: Call) => (itemLocal, itemAssign, sink)
        case result => fail(s"Expected local, assign and sink, but got $result")
      }

      itemLocal.name shouldBe "item"
      itemLocal.typeFullName shouldBe "java.lang.String"
      itemLocal.order shouldBe 1

      itemAssign.name shouldBe Operators.assignment
      itemAssign.typeFullName shouldBe "java.lang.String"
      itemAssign.order shouldBe 2
      itemAssign.argument.l match {
        case List(itemIdentifier: Identifier, indexAccess: Call) =>
          itemIdentifier.name shouldBe "item"
          itemIdentifier.typeFullName shouldBe "java.lang.String"
          itemIdentifier.order shouldBe 1
          itemIdentifier.argumentIndex shouldBe 1
          itemIdentifier.refOut.toSet should contain(itemLocal)

          indexAccess.name shouldBe Operators.indexAccess
          indexAccess.typeFullName shouldBe "java.lang.String"
          indexAccess.order shouldBe 2
          indexAccess.argumentIndex shouldBe 2
          val (items: Identifier, idx: Identifier) = (indexAccess.argument.l match {
            case List(items: Identifier, idx: Identifier) => (items, idx)
            case result                                   => s"Expected items[idx] args but got $result"
          }): @unchecked
          items.name shouldBe "items"
          items.typeFullName shouldBe "java.lang.String[]"
          items.order shouldBe 1
          items.argumentIndex shouldBe 1
          items.refOut.toSet should contain(cpg.parameter.name("items").head)

          idx.name shouldBe "$idx0"
          idx.typeFullName shouldBe "int"
          idx.order shouldBe 2
          idx.argumentIndex shouldBe 2
          idx.refOut.toSet should contain(idxLocal)
        case result => fail(s"Expected item = items[idx] args but got $result")
      }

      sink.name shouldBe "sink"
      sink.methodFullName shouldBe "Foo.sink:void(java.lang.String)"
      sink.order shouldBe 3
      sink.argument.l match {
        case List(item: Identifier) =>
          item.name shouldBe "item"
          item.typeFullName shouldBe "java.lang.String"
          item.order shouldBe 1
          item.argumentIndex shouldBe 1
          item.refOut.toSet should contain(itemLocal)

        case result => fail(s"Expected single identifier argument to sink but got $result")
      }
    }
  }

  "foreach loops over collections" should {
    val cpg = code("""
       |import java.util.List;
       |
       |public class Foo {
       |  public static void sink(String s) {}
       |
       |  public static void foo(List<String> items) {
       |    for (String item : items) {
       |      sink(item);
       |    }
       |  }
       |}
       |""".stripMargin)

    "create a local for the iterator as a child of the FOR block" in {
      val iterLocal = cpg.method.name("foo").body.astChildren.l match {
        case List(iterLocal: Local, _, _) => iterLocal
        case result                       => fail(s"Expected iterLocal as first of 3 foo body children but got $result")
      }

      iterLocal.name shouldBe "$iterLocal0"
      iterLocal.typeFullName shouldBe "java.util.Iterator"
      iterLocal.code shouldBe "$iterLocal0"
      iterLocal.order shouldBe 1
    }

    "assign items.iterator() to iterLocal" in {
      val (iterLocal, iterAssign) = cpg.method.name("foo").body.astChildren.l match {
        case List(iterLocal: Local, iterAssign: Call, _) => (iterLocal, iterAssign)
        case result => fail(s"Expected local and assign in foo body but got $result")
      }

      iterAssign.name shouldBe Operators.assignment
      iterAssign.methodFullName shouldBe Operators.assignment
      iterAssign.typeFullName shouldBe "java.util.Iterator"
      iterAssign.order shouldBe 2
      iterAssign.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val (target, iteratorCall) = iterAssign.argument.l match {
        case List(target: Identifier, iteratorCall: Call) => (target, iteratorCall)
        case result => fail(s"Expected iter = items.iterator() args but got $result")
      }

      target.name shouldBe "$iterLocal0"
      target.typeFullName shouldBe "java.util.Iterator"
      target.order shouldBe 1
      target.argumentIndex shouldBe 1
      target.refOut.toSet should contain(iterLocal)

      iteratorCall.name shouldBe "iterator"
      iteratorCall.methodFullName shouldBe "java.util.List.iterator:java.util.Iterator()"
      iteratorCall.signature shouldBe "java.util.Iterator()"
      iteratorCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      iteratorCall.typeFullName shouldBe "java.util.Iterator"
      iteratorCall.order shouldBe 2
      iteratorCall.argumentIndex shouldBe 2

      iteratorCall.argument(0).start.l match {
        case List(items: Identifier) =>
          items.name shouldBe "items"
          items.typeFullName shouldBe "java.util.List"
          items.order shouldBe 1
          items.argumentIndex shouldBe 0

        case result => fail(s"Expected single identifier receiver but got $result")
      }
    }

    "create hasNext() condition call" in {
      val (iterLocal, whileBlock) = cpg.method.name("foo").body.astChildren.l match {
        case List(iterLocal: Local, _, whileBlock: ControlStructure) => (iterLocal, whileBlock)
        case result => fail(s"Expected local and assign in foo body but got $result")
      }

      val conditionCall = whileBlock.condition.l match {
        case List(conditionCall: Call) => conditionCall
        case result                    => fail(s"Expected condition call but got $result")
      }

      conditionCall.name shouldBe "hasNext"
      conditionCall.methodFullName shouldBe "java.util.Iterator.hasNext:boolean()"
      conditionCall.signature shouldBe "boolean()"
      conditionCall.typeFullName shouldBe "boolean"
      conditionCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      conditionCall.order shouldBe 1

      conditionCall.argument(0).start.l match {
        case List(receiver: Identifier) =>
          receiver.name shouldBe "$iterLocal0"
          receiver.typeFullName shouldBe "java.util.Iterator"
          receiver.order shouldBe 1
          receiver.argumentIndex shouldBe 0
          receiver.refOut.toSet should contain(iterLocal)

        case result => fail(s"Expected single identifier receiver but got $result")
      }
    }

    "create an item local and assignment in the body of the FOR loop" in {
      val (iterLocal, whileBlock) = cpg.method.name("foo").body.astChildren.l match {
        case List(iterLocal: Local, _, whileBlock: ControlStructure) => (iterLocal, whileBlock)
        case result => fail(s"Expected local and assign in foo body but got $result")
      }

      val body = whileBlock.astChildren.l match {
        case List(_, body: Block) => body
        case result               => fail(s"Expected body as last child of FOR but got $result")
      }

      body.order shouldBe 2

      val (itemLocal, itemAssign, sinkCall) = body.astChildren.l match {
        case List(itemLocal: Local, itemAssign: Call, sinkCall: Call) => (itemLocal, itemAssign, sinkCall)
        case result => fail(s"Expected 3 statements in for body but got $result")
      }

      itemLocal.name shouldBe "item"
      itemLocal.code shouldBe "item"
      itemLocal.typeFullName shouldBe "java.lang.String"
      itemLocal.order shouldBe 1

      itemAssign.name shouldBe Operators.assignment
      itemAssign.methodFullName shouldBe Operators.assignment
      itemAssign.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      itemAssign.typeFullName shouldBe "java.lang.String"
      itemAssign.order shouldBe 2
      val (assignTarget, assignSource) = itemAssign.argument.l match {
        case List(assignTarget: Identifier, assignSource: Call) => (assignTarget, assignSource)
        case result => fail(s"Expected item = iterLocal.next args but got $result")
      }

      assignTarget.name shouldBe "item"
      assignTarget.typeFullName shouldBe "java.lang.String"
      assignTarget.order shouldBe 1
      assignTarget.argumentIndex shouldBe 1
      assignTarget.refOut.toSet should contain(itemLocal)

      assignSource.name shouldBe "next"
      assignSource.methodFullName shouldBe "java.util.Iterator.next:java.lang.Object()"
      assignSource.signature shouldBe "java.lang.Object()"
      assignSource.typeFullName shouldBe "java.lang.Object"
      assignSource.order shouldBe 2
      assignSource.argumentIndex shouldBe 2
      assignSource.argument(0).start.l match {
        case List(iterIdent: Identifier) =>
          iterIdent.name shouldBe "$iterLocal0"
          iterIdent.typeFullName shouldBe "java.util.Iterator"
          iterIdent.order shouldBe 1
          iterIdent.argumentIndex shouldBe 0
          iterIdent.refOut.toSet should contain(iterLocal)

        case result => fail(s"Expected single identifier receiver but got $result")
      }

      sinkCall.name shouldBe "sink"
      sinkCall.methodFullName shouldBe "Foo.sink:void(java.lang.String)"
      sinkCall.signature shouldBe "void(java.lang.String)"
      sinkCall.typeFullName shouldBe "void"
      sinkCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      sinkCall.order shouldBe 3
      sinkCall.argument.l match {
        case List(itemIdent: Identifier) =>
          itemIdent.name shouldBe "item"
          itemIdent.typeFullName shouldBe "java.lang.String"
          itemIdent.order shouldBe 1
          itemIdent.argumentIndex shouldBe 1
          itemIdent.refOut.toSet should contain(itemLocal)

        case result => fail(s"Expected single identifier arg to sink but got $result")
      }
    }
  }
}

class ControlStructureTests extends JavaSrcCode2CpgFixture {

  private val cpg = code("""
      |class Foo {
      |  int baz(Iterable<Integer> xs) {
      |    int sum = 0;
      |    for( Integer x : xs) {
      |      sum += x;
      |    }
      |    return sum;
      |  }
      |
      |  int bar(boolean x, boolean y, boolean z) {
      |    if (x || (y && z)) {
      |      return 1;
      |    }
      |    return 2;
      |  }
      |
      |  void foo(int x, int y) {
      |    try { } catch(exc_t exc) {
      |     // ...
      |    }
      |
      |    for(int i = 0; i < 10; i++) {
      |      if (x > y) {
      |        continue;
      |      }
      |      while(y++ < x) {
      |        printf("foo\n");
      |      }
      |    }
      |
      |    switch(y) {
      |      case 1:
      |        printf("bar\n");
      |        break;
      |      default:
      |    };
      |
      |    int i = 0;
      |    do {
      |      i++;
      |    } while(i < 11);
      |  }
      |
      |  public void elseTest(boolean b) {
      |    int x;
      |    if (b) {
      |      x = 42;
      |    } else {
      |      x = 39;
      |    }
      |  }
      |
      |  public boolean isConnected() {
      |    switch (this) {
      |      case Reconnected:
      |        return true;
      |
      |      case ConnectionLost:
      |      default:
      |        return false;
      |    }
      |  }
      |}
      |""".stripMargin)

  "should identify `try` block" in {
    cpg.method.name("foo").tryBlock.code.l shouldBe List("try")
  }

  "should identify `if` block" in {
    cpg.method.name("foo").ifBlock.condition.code.l shouldBe List("x > y")
  }

  "should identify `switch` block" in {
    cpg.method.name("foo").switchBlock.code.l shouldBe List("switch(y)")
  }

  "should identify `for` block" in {
    cpg.method.name("foo").forBlock.condition.code.l shouldBe List("i < 10")
  }

  "should identify `while` block" in {
    cpg.method.name("foo").whileBlock.condition.code.l shouldBe List("y++ < x")
  }

  "should identify `do` block" in {
    cpg.method.name("foo").doBlock.condition.code.l shouldBe List("i < 11")
  }

  "should identify `break`" in {
    cpg.method.name("foo").break.code.l shouldBe List("break;")
  }

  "should identify `continue`" in {
    cpg.method.name("foo").continue.code.l shouldBe List("continue;")
  }

  "should handle complex boolean conditions" in {
    cpg.method.name("bar").ifBlock.condition.code.l shouldBe List("x || (y && z)")
  }

  "should identify an else block" in {
    val ifBlock = cpg.method.name("elseTest").ifBlock.head
    ifBlock.code shouldBe "if (b)"
    val List(condition: Identifier, thenBlock: Block, elseBlock: ControlStructure) = ifBlock.astChildren.l: @unchecked
    condition.code shouldBe "b"
    condition.order shouldBe 1

    thenBlock.order shouldBe 2
    val thenBody = thenBlock.astChildren.head.asInstanceOf[Call]
    thenBody.code shouldBe "x = 42"
    thenBody.argument.head.code shouldBe "x"
    thenBody.argument.l.tail.head.code shouldBe "42"
    thenBody.order shouldBe 1

    elseBlock.code shouldBe "else"
    elseBlock.controlStructureType shouldBe "ELSE"
    elseBlock.order shouldBe 3
    val elseAssign = elseBlock.astChildren.head.astChildren.head.asInstanceOf[Call]
    elseAssign.order shouldBe 1
    elseAssign.code shouldBe "x = 39"
  }

  "should handle a switch conditioned on `this`" in {
    val switchBlock = cpg.method.name("isConnected").switchBlock.l match {
      case List(block) => block
      case res         => fail(s"Expected single switch block but got $res")
    }

    switchBlock.astChildren.size shouldBe 2
    val List(cond: Identifier, body: Block) = switchBlock.astChildren.l: @unchecked

    cond.order shouldBe 1
    cond.code shouldBe "this"
    cond.typeFullName shouldBe "Foo"

    // The visible statements/labels + jump targets
    body.astChildren.size shouldBe 7
  }
}
