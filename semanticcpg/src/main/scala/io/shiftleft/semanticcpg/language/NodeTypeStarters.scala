package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.v2.{Cpg, CpgNodeStarters, nodes}
import io.shiftleft.codepropertygraph.generated.v2.nodes.*

import scala.jdk.CollectionConverters.IteratorHasAsScala

//object Test3 {
//  class Cpg // cpg
//  class GeneratedNodeStarter(cpg: Cpg) {
//    def x: Int = 1
//  }
//  class SemanticCpgNodeStarter(cpg: Cpg) {
//    def x(s: String): Int = 2
//  }
//  given cpg2GeneratedStarters: Conversion[Cpg, GeneratedNodeStarter] with
//    def apply(cpg: Cpg): GeneratedNodeStarter = GeneratedNodeStarter(cpg)
//
//  given cpg2SemanticStarters: Conversion[Cpg, SemanticCpgNodeStarter] with
//    def apply(cpg: Cpg): SemanticCpgNodeStarter = SemanticCpgNodeStarter(cpg)
//
//  val cpg = new Cpg
//  cpg.x
//  cpg.x(3)
//}

//object Test4 {
//  class Cpg // cpg
//  class GeneratedNodeStarter(cpg: Cpg) {
//    def x: Int = 1
//  }
//  class SemanticCpgNodeStarter(cpg: Cpg) {
//    def x(s: Int): Int = 2
//  }
//
//  // idea: two implicits with different hierarchy
//  trait LowerPrio {
//    implicit def toNodeTypeStarters(cpg: Cpg): SemanticCpgNodeStarter = ???
//  }
//  object Lang extends LowerPrio {
//    implicit def toGeneratedNodeTypeStarters(cpg: Cpg): GeneratedNodeStarter = ???
//  }
//
//  import Lang.*
//
//  val cpg = new Cpg
//  cpg.x
//  cpg.x(3)
//}



//object Test2 {
//  class Cpg // cpg
//
//  trait CpgGeneratedNodeStarters1 {
//    extension (wrappedGraph: Cpg) {
//      def x: Int
//      def y: Int
//    }
//  }
//
//  trait SemanticCpgNodeStarters extends CpgGeneratedNodeStarters1 {
//    extension (a: Cpg) {
//      def x(name: String): Int = 2
//    }
//  }
//
//  trait SecurityProfileNodeStarters {
//    extension (a: Cpg) {
//      def y(s: String): Int = 2
//    }
//  }
//
//  object X extends SemanticCpgNodeStarters
//  object Y extends SecurityProfileNodeStarters
//  import Y._
//  import X._
//
//  val a = new Cpg
//  a.x
//  a.x(3)
//}
//
//
//object NodeTypeStarters1 extends CpgGeneratedNodeStarters1 {
//  extension (wrappedGraph: Cpg) {
//    def call(name: String): Iterator[nodes.Call] = ???
//  }
//}
//
//object NodeTypeStarters2 {
//  extension (wrappedGraph: Cpg) {
//    def method(name: String): Iterator[nodes.Method] = ???
//  }
//}
//
//object Usage {
//  import NodeTypeStarters1.*
//  import NodeTypeStarters2.*
//  val cpg: Cpg = ???
//  cpg.call
//  cpg.call("asdf")
//
//}
//
//trait CpgGeneratedNodeStarters1 {
//  extension (wrappedGraph: Cpg) {
//    def call: Iterator[nodes.Call] = wrappedGraph.graph.nodes(26).asInstanceOf[Iterator[nodes.Call]]
//    def method: Iterator[nodes.Method] = wrappedGraph.graph.nodes(26).asInstanceOf[Iterator[nodes.Method]]
//  }
//}
