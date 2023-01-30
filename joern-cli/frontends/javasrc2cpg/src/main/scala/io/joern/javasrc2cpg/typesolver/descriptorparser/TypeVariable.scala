package io.joern.javasrc2cpg.typesolver.descriptorparser

case class TypeVariable(name: String, bounds: List[NamedItem]) extends NamedItem
