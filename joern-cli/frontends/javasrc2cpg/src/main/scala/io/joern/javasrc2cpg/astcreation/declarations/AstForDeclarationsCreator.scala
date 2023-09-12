package io.joern.javasrc2cpg.astcreation.declarations
import io.joern.javasrc2cpg.astcreation.AstCreator

trait AstForDeclarationsCreator extends AstForTypeDeclsCreator with AstForMethodsCreator { this: AstCreator => }
