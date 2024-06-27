package io.shiftleft.semanticcpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Languages, ModifierTypes}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder

package object testing {

  object MockCpg {

    def apply(): MockCpg = new MockCpg

    def apply(f: (DiffGraphBuilder, Cpg) => Unit): MockCpg = new MockCpg().withCustom(f)
  }

  case class MockCpg(cpg: Cpg = Cpg.empty) {

    def withMetaData(language: String = Languages.C): MockCpg = withMetaData(language, Nil)

    def withMetaData(language: String, overlays: List[String]): MockCpg = {
      withCustom { (diffGraph, _) =>
        diffGraph.addNode(NewMetaData().language(language).overlays(overlays))
      }
    }

    def withFile(filename: String, content: Option[String] = None): MockCpg =
      withCustom { (graph, _) =>
        val newFile = NewFile().name(filename)
        content.foreach(newFile.content(_))
        graph.addNode(newFile)
      }

    def withNamespace(name: String, inFile: Option[String] = None): MockCpg =
      withCustom { (graph, _) =>
        {
          val namespaceBlock = NewNamespaceBlock().name(name)
          val namespace      = NewNamespace().name(name)
          graph.addNode(namespaceBlock)
          graph.addNode(namespace)
          graph.addEdge(namespaceBlock, namespace, EdgeTypes.REF)
          if (inFile.isDefined) {
            val fileNode = cpg.file.name(inFile.get).head
            graph.addEdge(namespaceBlock, fileNode, EdgeTypes.SOURCE_FILE)
          }
        }
      }

    def withTypeDecl(
      name: String,
      isExternal: Boolean = false,
      inNamespace: Option[String] = None,
      inFile: Option[String] = None,
      offset: Option[Int] = None,
      offsetEnd: Option[Int] = None
    ): MockCpg =
      withCustom { (graph, _) =>
        {
          val typeNode = NewType().name(name)
          val typeDeclNode = NewTypeDecl()
            .name(name)
            .fullName(name)
            .isExternal(isExternal)

          offset.foreach(typeDeclNode.offset(_))
          offsetEnd.foreach(typeDeclNode.offsetEnd(_))

          val member   = NewMember().name("amember")
          val modifier = NewModifier().modifierType(ModifierTypes.STATIC)

          graph.addNode(typeDeclNode)
          graph.addNode(typeNode)
          graph.addNode(member)
          graph.addNode(modifier)
          graph.addEdge(typeNode, typeDeclNode, EdgeTypes.REF)
          graph.addEdge(typeDeclNode, member, EdgeTypes.AST)
          graph.addEdge(member, modifier, EdgeTypes.AST)

          if (inNamespace.isDefined) {
            val namespaceBlock = cpg.namespaceBlock(inNamespace.get).head
            graph.addEdge(namespaceBlock, typeDeclNode, EdgeTypes.AST)
          }
          if (inFile.isDefined) {
            val fileNode = cpg.file.name(inFile.get).head
            graph.addEdge(typeDeclNode, fileNode, EdgeTypes.SOURCE_FILE)
          }
        }
      }

    def withMethod(
      name: String,
      external: Boolean = false,
      inTypeDecl: Option[String] = None,
      fileName: String = "",
      offset: Option[Int] = None,
      offsetEnd: Option[Int] = None
    ): MockCpg =
      withCustom { (graph, _) =>
        val retParam  = NewMethodReturn().typeFullName("int").order(10)
        val param     = NewMethodParameterIn().order(1).index(1).name("param1")
        val paramType = NewType().name("paramtype")
        val paramOut  = NewMethodParameterOut().name("param1").order(1)
        val method =
          NewMethod().isExternal(external).name(name).fullName(name).signature("asignature").filename(fileName)
        offset.foreach(method.offset(_))
        offsetEnd.foreach(method.offsetEnd(_))
        val block    = NewBlock().typeFullName("int")
        val modifier = NewModifier().modifierType("modifiertype")

        graph.addNode(method)
        graph.addNode(retParam)
        graph.addNode(param)
        graph.addNode(paramType)
        graph.addNode(paramOut)
        graph.addNode(block)
        graph.addNode(modifier)
        graph.addEdge(method, retParam, EdgeTypes.AST)
        graph.addEdge(method, param, EdgeTypes.AST)
        graph.addEdge(param, paramOut, EdgeTypes.PARAMETER_LINK)
        graph.addEdge(method, block, EdgeTypes.AST)
        graph.addEdge(param, paramType, EdgeTypes.EVAL_TYPE)
        graph.addEdge(paramOut, paramType, EdgeTypes.EVAL_TYPE)
        graph.addEdge(method, modifier, EdgeTypes.AST)

        if (inTypeDecl.isDefined) {
          val typeDeclNode = cpg.typeDecl.name(inTypeDecl.get).head
          graph.addEdge(typeDeclNode, method, EdgeTypes.AST)
        }

        if (fileName != "") {
          val file = cpg.file
            .nameExact(fileName)
            .headOption
            .getOrElse(throw new RuntimeException(s"file with name='$fileName' not found"))
          graph.addEdge(method, file, EdgeTypes.SOURCE_FILE)
        }
      }

    def withTagsOnMethod(
      methodName: String,
      methodTags: List[(String, String)] = List(),
      paramTags: List[(String, String)] = List()
    ): MockCpg =
      withCustom { (graph, cpg) =>
        implicit val diffGraph: DiffGraphBuilder = graph
        methodTags.foreach { case (k, v) =>
          cpg.method.name(methodName).newTagNodePair(k, v).store()(diffGraph)
        }
        paramTags.foreach { case (k, v) =>
          cpg.method.name(methodName).parameter.newTagNodePair(k, v).store()(diffGraph)
        }
      }

    def withCallInMethod(methodName: String, callName: String, code: Option[String] = None): MockCpg =
      withCustom { (graph, cpg) =>
        val methodNode = cpg.method.name(methodName).head
        val blockNode  = methodNode.block
        val callNode   = NewCall().name(callName).code(code.getOrElse(callName))
        graph.addNode(callNode)
        graph.addEdge(blockNode, callNode, EdgeTypes.AST)
        graph.addEdge(methodNode, callNode, EdgeTypes.CONTAINS)
      }

    def withMethodCall(calledMethod: String, callingMethod: String, code: Option[String] = None): MockCpg =
      withCustom { (graph, cpg) =>
        val callingMethodNode = cpg.method.name(callingMethod).head
        val calledMethodNode  = cpg.method.name(calledMethod).head
        val callNode          = NewCall().name(calledMethod).code(code.getOrElse(calledMethod))
        graph.addEdge(callNode, calledMethodNode, EdgeTypes.CALL)
        graph.addEdge(callingMethodNode, callNode, EdgeTypes.CONTAINS)
      }

    def withLocalInMethod(methodName: String, localName: String): MockCpg =
      withCustom { (graph, cpg) =>
        val methodNode = cpg.method.name(methodName).head
        val blockNode  = methodNode.block
        val typeNode   = NewType().name("alocaltype")
        val localNode  = NewLocal().name(localName).typeFullName("alocaltype")
        graph.addNode(localNode)
        graph.addNode(typeNode)
        graph.addEdge(blockNode, localNode, EdgeTypes.AST)
        graph.addEdge(localNode, typeNode, EdgeTypes.EVAL_TYPE)
      }

    def withLiteralArgument(callName: String, literalCode: String): MockCpg = {
      withCustom { (graph, cpg) =>
        val callNode    = cpg.call.name(callName).head
        val methodNode  = callNode.method
        val literalNode = NewLiteral().code(literalCode)
        val typeDecl = NewTypeDecl()
          .name("ATypeDecl")
          .fullName("ATypeDecl")

        graph.addNode(typeDecl)
        graph.addNode(literalNode)
        graph.addEdge(callNode, literalNode, EdgeTypes.AST)
        graph.addEdge(methodNode, literalNode, EdgeTypes.CONTAINS)
      }
    }

    def withIdentifierArgument(callName: String, name: String, index: Int = 1): MockCpg =
      withArgument(callName, NewIdentifier().name(name).argumentIndex(index))

    def withCallArgument(callName: String, callArgName: String, code: String = "", index: Int = 1): MockCpg =
      withArgument(callName, NewCall().name(callArgName).code(code).argumentIndex(index))

    def withArgument(callName: String, newNode: NewNode): MockCpg = withCustom { (graph, cpg) =>
      val callNode   = cpg.call.name(callName).head
      val methodNode = callNode.method
      val typeDecl   = NewTypeDecl().name("abc")
      graph.addEdge(callNode, newNode, EdgeTypes.AST)
      graph.addEdge(callNode, newNode, EdgeTypes.ARGUMENT)
      graph.addEdge(methodNode, newNode, EdgeTypes.CONTAINS)
      graph.addEdge(newNode, typeDecl, EdgeTypes.REF)
      graph.addNode(newNode)
    }

    def withCustom(f: (DiffGraphBuilder, Cpg) => Unit): MockCpg = {
      val diffGraph = Cpg.newDiffGraphBuilder
      f(diffGraph, cpg)
      class MyPass extends CpgPass(cpg) {
        override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
          builder.absorb(diffGraph)
        }
      }
      new MyPass().createAndApply()
      this
    }
  }

}
