package io.joern.kotlin2cpg.types

import org.jetbrains.kotlin.descriptors.{DeclarationDescriptor}
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.types.{ErrorType, ErrorUtils, KotlinType, UnresolvedType}
import org.jetbrains.kotlin.builtins.jvm.JavaToKotlinClassMap
import org.jetbrains.kotlin.renderer.{DescriptorRenderer, DescriptorRendererImpl, DescriptorRendererOptionsImpl}

import scala.jdk.CollectionConverters._

object TypeRenderer {

  private val cpgUnresolvedType = ErrorUtils.createUnresolvedType(TypeConstants.cpgUnresolved, List().asJava)

  def descriptorRenderer(): DescriptorRenderer = {
    val opts = new DescriptorRendererOptionsImpl
    opts.setParameterNamesInFunctionalTypes(false)
    opts.setInformativeErrorType(false)
    opts.setTypeNormalizer {
      case _: UnresolvedType => cpgUnresolvedType
      case _: ErrorType      => cpgUnresolvedType
      case t                 => t
    }
    new DescriptorRendererImpl(opts)
  }

  def renderFqName(desc: DeclarationDescriptor): String = {
    val renderer = descriptorRenderer()
    val fqName   = DescriptorUtils.getFqName(desc)
    stripped(renderer.renderFqName(fqName))
  }

  def render(t: KotlinType): String = {
    val renderer = descriptorRenderer()
    if (isFunctionXType(t)) {
      TypeConstants.kotlinFunctionXPrefix + (t.getArguments.size() - 1).toString
    } else {
      val fqName     = DescriptorUtils.getFqName(t.getConstructor.getDeclarationDescriptor)
      val mappedType = JavaToKotlinClassMap.INSTANCE.mapKotlinToJava(fqName)
      if (mappedType != null) {
        stripped(DescriptorRenderer.COMPACT.renderFqName(mappedType.asSingleFqName().toUnsafe))
      } else {
        val rendered = renderer.renderType(t)
        stripped(rendered)
      }
    }
  }

  private def isFunctionXType(t: KotlinType): Boolean = {
    val renderer            = descriptorRenderer()
    val renderedConstructor = renderer.renderTypeConstructor(t.getConstructor)
    renderedConstructor.startsWith(TypeConstants.kotlinFunctionXPrefix) ||
    renderedConstructor.startsWith(TypeConstants.kotlinSuspendFunctionXPrefix)
  }

  def stripped(typeName: String): String = {
    def stripTypeParams(typeName: String): String = {
      typeName.replaceAll("<.*>", "")
    }
    def stripOut(name: String): String = {
      if (name.contains("<") && name.contains(">") && name.contains("out")) {
        name.replaceAll("(<[^o]*)[(]?out[)]?[ ]*([a-zA-Z])", "<$2")
      } else {
        name
      }
    }
    def stripOptionality(typeName: String): String = {
      typeName.replaceAll("!", "").replaceAll("\\?", "")
    }
    def stripDebugInfo(typeName: String): String = {
      if (typeName.contains("/* =")) {
        typeName.split("/\\* =")(0)
      } else {
        typeName
      }
    }

    stripTypeParams(stripOptionality(stripDebugInfo(stripOut(typeName))).trim().replaceAll(" ", ""))
  }
}
