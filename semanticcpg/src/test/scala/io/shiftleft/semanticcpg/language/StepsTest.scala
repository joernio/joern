package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.Cpg.docSearchPackages
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{NodeTypes, Properties}
import io.shiftleft.semanticcpg.testing.MockCpg
import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.traversal.{Traversal, jIteratortoTraversal, toElementTraversal, toNodeTraversal}

class StepsTest extends AnyWordSpec with Matchers {

  val cpg = MockCpg()
    .withFile("afile.c")
    .withNamespace("anamespace")
    .withTypeDecl("AClass", inNamespace = Some("anamespace"), inFile = Some("afile.c"))
    .withMethod("foo", inTypeDecl = Some("AClass"))
    .withMethod("woo", inTypeDecl = Some("AClass"))
    .withCallInMethod("foo", "acall")
    .withLocalInMethod("foo", "local")
    .withLiteralArgument("acall", "moo")
    .withIdentifierArgument("acall", "anidentifier")
    .cpg

  "generic cpg" should {

    "filter by regex" in {
      cpg.literal.code(".*").size shouldBe 1
    }

    "filter on cpg type" in {
      cpg.method
        .name(".*")
        .count(_.fullName.matches(".*woo.*")) shouldBe 1
    }

    "filter with traversal on cpg type" in {
      def allMethods    = cpg.method.l
      val publicMethods = allMethods.to(Traversal).where(_.isPublic)
      allMethods.size should be > publicMethods.toList.size
    }

    "filter on id" when {
      "providing one" in {
        // find an arbitrary method so we can find it again in the next step
        val method: Method        = cpg.method.head
        val results: List[Method] = cpg.method.id(method.id).toList
        results.size shouldBe 1
        results.head.underlying.id
      }

      "providing multiple" in {
        // find two arbitrary methods so we can find it again in the next step
        val methods               = cpg.method.toList.take(2)
        val results: List[Method] = cpg.method.id(methods.map(_.id): _*).toList

        results.size shouldBe 2
        results.toSet shouldBe methods.toSet
      }
    }
  }

  "find that all method returns are linked to a method" in {
    val returnsWithMethods = cpg.method.methodReturn.l
    val returns            = cpg.methodReturn.l
    returnsWithMethods.size shouldBe returns.size
  }

  "allow for comprehensions" in {
    case class MethodParamPairs(methodName: String, paramName: String)

    val query = for {
      method <- cpg.method
      param  <- method.parameter
    } yield MethodParamPairs(method.name, param.name)

    val pairs: List[MethodParamPairs] = query.toList
    pairs.size should be > 0
  }

  "allow lists in map/flatMap/forComprehension" in {
    cpg.method.map { method =>
      (method.name, method.parameter.l)
    }.size should be > 1
  }

  "allow side effects" in {
    var i = 0
    cpg.method.sideEffect(_ => i = i + 1).exec()
    i should be > 0
  }

  "allow retrieving ids" in {
    cpg.method.id.l should not be empty
  }

  "when calling for an AST node's parent block" should {

    "return the parent block for a block's AST child" in {
      val List(block: Block)        = cpg.method("woo").block.l
      val blockDirectChild: AstNode = cpg.method("woo").block.ast.head
      val blockLeafChild: AstNode   = cpg.method("woo").block.ast.last

      blockDirectChild.parentBlock.head shouldBe block
      blockLeafChild.parentBlock.head shouldBe block
    }

    "return an empty traversal if no block is found" in {
      cpg.method("woo").parentBlock.map(_.label).isEmpty shouldBe true
    }

  }

  "toJson" when {
    "operating on StoredNode" in {
      val json   = cpg.method.nameExact("foo").toJson
      val parsed = parse(json).children.head // exactly one result for the above query
      (parsed \ "_label") shouldBe JString("METHOD")
      (parsed \ "name") shouldBe JString("foo")
    }

    "operating on NewNode" in {
      implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder
      val json                                 = cpg.method.name("foo").location.toJson
      val parsed                               = parse(json).children.head // exactly one result for the above query
      (parsed \ "symbol") shouldBe JString("foo")
      (parsed \ "className") shouldBe JString("AClass")
      (parsed \ "filename") shouldBe JString("N/A")
    }

    "operating on primitive" in {
      val json   = cpg.method.name("foo").signature.toJson
      val parsed = parse(json).children.head // exactly one result for the above query
      parsed shouldBe JString("asignature")
    }
  }

  ".p for pretty printing" should {

    "use default `toString` if nothing else applies" in {
      case class Foo(i: Int)
      val steps: Steps[Foo] = new Steps(Traversal.fromSingle(Foo(42)))
      steps.p.head shouldBe "Foo(42)"
    }

    "render nodes as `(label,id): properties`" in {
      def mainMethods: Traversal[Method] = cpg.method.name("woo")

      val nodeId  = mainMethods.head.id
      val printed = mainMethods.p.head
      printed.should(startWith(s"""(METHOD,$nodeId):"""))
      printed.should(include("IS_EXTERNAL: false"))
      printed.should(include("FULL_NAME: woo"))
    }

    "allows to provide custom Show instance" in {
      def mainMethods: Steps[Method] =
        cpg.method.name("woo")

      implicit val customShowInstance = new Show[Method] {
        override def apply(node: Method): String = "my custom pretty printer"
      }

      mainMethods.p.head shouldBe "my custom pretty printer"
    }

    "uses Show instance from package" in {
      object SomePackage {
        implicit def packageShowInstance: Show[Method] = { _ =>
          "package defined pretty printer"
        }
      }

      import SomePackage._
      def mainMethods: Steps[Method] =
        cpg.method.name("woo")

      mainMethods.p.head shouldBe "package defined pretty printer"
    }
  }

  ".help step" should {
    "show domain overview" in {
      val domainStartersHelp = Cpg.emptyCpg.help
      domainStartersHelp should include(".comment")
      domainStartersHelp should include("All comments in source-based CPGs")
      domainStartersHelp should include(".arithmetic")
      domainStartersHelp should include("All arithmetic operations")
    }

    "provide node-specific overview" in {
      val methodStepsHelp = Cpg.emptyCpg.method.help
      methodStepsHelp should include("Available steps for Method")
      methodStepsHelp should include(".namespace")
      methodStepsHelp should include(".depth") // from AstNode

      val methodStepsHelpVerbose = Cpg.emptyCpg.method.helpVerbose
      methodStepsHelpVerbose should include("traversal name")
      methodStepsHelpVerbose should include("io.shiftleft.semanticcpg.language.types.structure.Method")

      val assignmentStepsHelp = Cpg.emptyCpg.assignment.help
      assignmentStepsHelp should include("Left-hand sides of assignments") // from AssignmentTraversal
    }

    "provides generic help" when {
      "using verbose mode" when {
        "traversing nodes" in {
          val methodTraversal = Traversal.empty[Method]
          methodTraversal.helpVerbose should include(".l")
          methodTraversal.helpVerbose should include(".label")
        }

        "traversing non-nodes" in {
          val stringTraversal = Traversal.empty[String]
          stringTraversal.helpVerbose should include(".l")
          stringTraversal.helpVerbose should not include ".label"
        }
      }
    }
  }

  "provides extension steps for Traversals and Nodes" in {
    /* n.b. interestingly, intellij puts some red squiggles on `Traversal.file` etc. if one imports
     * `overflowdb.traversal.iterableToTraversal`,  e.g. via `import overflowdb.traversal._`
     * Looks like thats a bug in intellij's presentation compiler, esp. given that both sbt and intellij compile this
     * code without errors, and intellij's autocomplete works.
     */
    def literal = cpg.literal.code("moo")
    literal.method.name.head shouldBe "foo"
    literal.head.method.name shouldBe "foo"

    def typ = cpg.typ.nameExact("AClass")
    typ.namespace.name.head shouldBe "anamespace"
    typ.head.namespace.name.head shouldBe "anamespace"

    def typeDecl = cpg.typeDecl.nameExact("AClass")
    typeDecl.namespace.name.head shouldBe "anamespace"
    typeDecl.head.namespace.name.head shouldBe "anamespace"

    def call = cpg.call.nameExact("acall")
    call.method.name.size shouldBe 1
    call.map(_.method.name).size shouldBe 1

    // not testable in this cpg, but if it compiles it's probably fine
    def controlStructure = cpg.controlStructure
    controlStructure.condition
    controlStructure.headOption.map(_.condition)

    def identifier = cpg.identifier.name("anidentifier")
    identifier.refsTo.size shouldBe 1
    identifier.head.refsTo.size shouldBe 1

    def member = cpg.member.name("amember")
    member.typeDecl.name.head shouldBe "AClass"
    member.head.typeDecl.name shouldBe "AClass"

    def local = cpg.local.name("local")
    local.typ.name.head shouldBe "alocaltype"
    local.head.typ.name.head shouldBe "alocaltype"

    def method = cpg.method.name("foo")
    method.parameter.size shouldBe 1
    method.head.parameter.size shouldBe 1

    def methodParameterIn = cpg.parameter.name("param1")
    methodParameterIn.typ.name.head shouldBe "paramtype"
    methodParameterIn.head.typ.name shouldBe "paramtype"

    def methodParameterOut =
      cpg.graph
        .nodes(NodeTypes.METHOD_PARAMETER_OUT)
        .cast[MethodParameterOut]
        .name("param1")
    methodParameterOut.typ.name.head shouldBe "paramtype"
    methodParameterOut.head.typ.name.head shouldBe "paramtype"

    def methodReturn = cpg.methodReturn.typeFullNameExact("int")
    methodReturn.method.name.toSetMutable shouldBe Set("foo", "woo")
    methodReturn.map(_.method.name).toSetMutable shouldBe Set("foo", "woo")

    def namespace = cpg.namespace.name("anamespace")
    namespace.typeDecl.name.toSetMutable shouldBe Set("AClass")
    namespace.head.typeDecl.name.toSetMutable shouldBe Set("AClass")

    def namespaceBlock = cpg.namespaceBlock.name("anamespace")
    namespaceBlock.typeDecl.name.toSetMutable shouldBe Set("AClass")
    namespaceBlock.flatMap(_.typeDecl.name).toSetMutable shouldBe Set("AClass")

    def file = cpg.file.name("afile.c")
    file.typeDecl.name.head shouldBe "AClass"
    file.head.typeDecl.name.head shouldBe "AClass"

    def block = cpg.graph.nodes(NodeTypes.BLOCK).cast[Block].typeFullName("int")
    block.local.name.size shouldBe 1
    block.flatMap(_.local.name).size shouldBe 1

    // not testable in this cpg, but if it compiles it's probably fine
    def methodRef = cpg.methodRef
    methodRef.referencedMethod
    methodRef.headOption.map(_.referencedMethod)

    def expression: Traversal[Expression] = cpg.identifier.name("anidentifier").cast[Expression]
    expression.expressionUp.isCall.size shouldBe 1
    expression.head.expressionUp.isCall.size shouldBe 1

//    def cfg: Traversal[CfgNode] = cpg.method.name("add")

    def ast: Traversal[AstNode] = cpg.method.name("foo").cast[AstNode]
    ast.astParent.property(Properties.NAME).head shouldBe "AClass"
    ast.head.astParent.property(Properties.NAME) shouldBe "AClass"

    // methodForCallGraph
    method.call.size shouldBe 1
    method.head.call.size shouldBe 1

    // callForCallGraph - only verifying that it compiles
    call.callee(NoResolve)
    call.head.callee(NoResolve)

    // AstNodeDot - only verifying that it compiles
    ast.dotAst
    ast.head.dotAst

    // dotCfg
    method.dotCfg.head.startsWith("digraph add {")
    method.head.dotCfg.head.startsWith("digraph add {")

    // typeFullName
    local.typeFullName.head shouldBe "alocaltype"
    local.head.typeFullName shouldBe "alocaltype"

    // modifierAccessors
    method.modifier.modifierType.toSetMutable shouldBe Set("modifiertype")
    method.head.modifier.modifierType.toSetMutable shouldBe Set("modifiertype")
  }

}
