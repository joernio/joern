package io.joern.kotlin2cpg.types

import io.joern.kotlin2cpg.types.NameRenderer.builtinTypeTranslationTable
import io.joern.x2cpg.Defines
import org.jetbrains.kotlin.builtins.jvm.JavaToKotlinClassMap
import org.jetbrains.kotlin.descriptors.impl.TypeAliasConstructorDescriptor
import org.jetbrains.kotlin.descriptors.{ClassDescriptor, ConstructorDescriptor, DeclarationDescriptor, FunctionDescriptor, ModuleDescriptor, PackageFragmentDescriptor, TypeParameterDescriptor}
import org.jetbrains.kotlin.name.{FqName, FqNameUnsafe}
import org.jetbrains.kotlin.psi.{KtClassOrObject, KtConstructor, KtDestructuringDeclarationEntry, KtExpression, KtFunctionLiteral, KtNamedFunction}
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.calls.model.ResolvedCall
import org.jetbrains.kotlin.resolve.jvm.JvmPrimitiveType
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.error.ErrorClassDescriptor

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object NameRenderer {
  private val builtinTypeTranslationTable = mutable.HashMap(
    "kotlin.Unit"                          -> "void",
    "kotlin.Boolean"                       -> "boolean",
    "kotlin.Char"                          -> "char",
    "kotlin.Byte"                          -> "byte",
    "kotlin.Short"                         -> "short",
    "kotlin.Int"                           -> "int",
    "kotlin.Float"                         -> "float",
    "kotlin.Long"                          -> "long",
    "kotlin.Double"                        -> "double",
    "kotlin.BooleanArray"                  -> "boolean[]",
    "kotlin.CharArray"                     -> "char[]",
    "kotlin.ByteArray"                     -> "byte[]",
    "kotlin.ShortArray"                    -> "short[]",
    "kotlin.IntArray"                      -> "int[]",
    "kotlin.FloatArray"                    -> "float[]",
    "kotlin.LongArray"                     -> "long[]",
    "kotlin.DoubleArray"                   -> "double[]",
  )
}

class NameRenderer(bindingContext: BindingContext) {
  private val anonDescriptorToIndex = mutable.HashMap.empty[DeclarationDescriptor, Int]
  private var anonObjectCounter     = 0

  private def getAnonDescIndex(desc: DeclarationDescriptor): Int = {
    anonDescriptorToIndex.getOrElseUpdate(
      desc, {
        val index = anonObjectCounter
        anonObjectCounter += 1
        index
      }
    )
  }

  def descName(desc: DeclarationDescriptor): String = {
    if (desc.getName.isSpecial) {
      desc match {
        case _: ConstructorDescriptor =>
          Defines.ConstructorMethodName
        case functionDesc: FunctionDescriptor =>
          Defines.ClosurePrefix + getAnonDescIndex(desc)
        case _ =>
          "object$" + getAnonDescIndex(desc)
      }
    } else {
      desc.getName.getIdentifier
    }
  }

  def descFullName(desc: DeclarationDescriptor): Option[String] = {
    val dealiasedDesc =
      desc match {
        case typeAliasDesc: TypeAliasConstructorDescriptor =>
          typeAliasDesc.getUnderlyingConstructorDescriptor
        case _ =>
          desc
      }
    val fullName = descFullNameInternal(dealiasedDesc).map(_.reverse.mkString(""))
    fullName
  }

  private def descFullNameInternal(desc: DeclarationDescriptor): Option[List[String]] = {
    if (desc.isInstanceOf[ErrorClassDescriptor]) {
      return None
    }
    val parentDesc = desc.getContainingDeclaration

    val parentFnParts =
      Option(parentDesc) match {
        case None =>
          Some(Nil)
        case Some(parentDesc) =>
          descFullNameInternal(parentDesc)
      }

    if (parentFnParts.isEmpty) {
      return None
    }

    var extendedFnParts = parentFnParts.get
    desc match {
      case packageFragmentDesc: PackageFragmentDescriptor =>
        if (!packageFragmentDesc.getName.isSpecial) {
          extendedFnParts = packageFragmentDesc.getFqName.toString :: extendedFnParts
        }
      case moduleDesc: ModuleDescriptor =>
      // Nothing todo since this is just the root element which has no namespace representation.
      case _ =>
        if (extendedFnParts.nonEmpty) {
          val separator =
            if (parentDesc.isInstanceOf[ClassDescriptor] && desc.isInstanceOf[ClassDescriptor]) {
              // Nested class.
              "$"
            } else {
              "."
            }
          extendedFnParts = separator :: extendedFnParts
        }

        val name = descName(desc)

        extendedFnParts = name :: extendedFnParts
    }
    Some(extendedFnParts)
  }

  private def isConstructorDesc(functionDesc: FunctionDescriptor): Boolean = {
    functionDesc match {
      case _: ConstructorDescriptor => true
      case _                        => false
    }
  }

  def funcDescSignature(functionDesc: FunctionDescriptor): Option[String] = {
    val originalDesc = functionDesc.getOriginal

    val extRecvDesc         = Option(originalDesc.getExtensionReceiverParameter)
    val extRecvTypeFullName = extRecvDesc.flatMap(paramDesc => typeFullName(paramDesc.getType))

    if (extRecvDesc.nonEmpty && extRecvTypeFullName.isEmpty) {
      return None
    }

    val paramTypeFullNames = originalDesc.getValueParameters.asScala.map(paramDesc => typeFullName(paramDesc.getType))
    if (paramTypeFullNames.exists(_.isEmpty)) {
      return None
    }

    val returnTypeFullName =
      if (isConstructorDesc(originalDesc)) {
        Some("void")
      } else {
        typeFullName(originalDesc.getReturnType)
      }
    if (returnTypeFullName.isEmpty) {
      return None
    }

    val combinedParamTypeFn = paramTypeFullNames.prepended(extRecvTypeFullName)

    val signature = s"${returnTypeFullName.get}(${combinedParamTypeFn.flatten.mkString(",")})"
    Some(signature)
  }

  def combineFunctionFullName(descFullName: String, signature: String): String = {
    s"$descFullName:$signature"
  }

  def typeFullName(typ: KotlinType): Option[String] = {
    val kotlinFullName =
      typ.getConstructor.getDeclarationDescriptor match {
        case classDesc: ClassDescriptor =>
          descFullName(classDesc)
        case typeParamDesc: TypeParameterDescriptor =>
          val upperBoundTypeFns = typeParamDesc.getUpperBounds.asScala.map(typeFullName)
          if (upperBoundTypeFns.exists(_.isEmpty)) {
            None
          } else {
            Some(upperBoundTypeFns.flatten.mkString("&"))
          }
      }

    kotlinFullName.map { kotlinFullName =>
      val javaFullName = builtinTypeTranslationTable.get(kotlinFullName)
      if (javaFullName.isDefined) {
        javaFullName.get
      } else if (kotlinFullName.startsWith("kotlin.Function")) {
        // For historical reasons kotlin.Function types are not mapped via JavaToKotlinClassMap.
        // In order to not touch the policies, we mimic that for now.
        kotlinFullName
      } else {
        // Nested class fullnames contain '$' in our representation which need to be mapped to '.'
        // in order to make use of JavaToKotlinClassMap.
        val kotlinFullNameDotOnly = kotlinFullName.replace('$','.')
        val javaFullName = JavaToKotlinClassMap.INSTANCE.mapKotlinToJava(FqNameUnsafe(kotlinFullNameDotOnly))

        val result =
          if (javaFullName != null) {
            // In front of nested class sub names we find '.' which needs to be mapped to '$' in our representation.
            // After that we can map the normal name separator '/' to '.'.
            javaFullName.toString.replace('.', '$').replace('/', '.')
          } else {
            kotlinFullName
          }
        result
      }
    }
  }

  def astToDesc(classAst: KtClassOrObject): Option[ClassDescriptor] = {
    Option(bindingContext.get(BindingContext.CLASS, classAst))
  }

  def astToDesc(functionAst: KtNamedFunction): Option[FunctionDescriptor] = {
    Option(bindingContext.get(BindingContext.FUNCTION, functionAst))
  }

  def astToDesc(destructuringAst: KtDestructuringDeclarationEntry): Option[FunctionDescriptor] = {
    val resolvedCall = Option(bindingContext.get(BindingContext.COMPONENT_RESOLVED_CALL, destructuringAst))

    resolvedCall.map(_.getResultingDescriptor).collect { case functionDesc: FunctionDescriptor =>
      functionDesc
    }
  }

  def astToDesc(constructorAst: KtConstructor[?]): Option[ConstructorDescriptor] = {
    Option(bindingContext.get(BindingContext.CONSTRUCTOR, constructorAst))
  }

  def astToDesc(expressionAst: KtExpression): Option[FunctionDescriptor] = {
    val call         = Option(bindingContext.get(BindingContext.CALL, expressionAst))
    val resolvedCall = call.flatMap(call => Option(bindingContext.get(BindingContext.RESOLVED_CALL, call)))

    resolvedCall.map(_.getResultingDescriptor).collect { case functionDesc: FunctionDescriptor =>
      functionDesc
    }
  }

  def astToDesc(functionLiteralAst: KtFunctionLiteral): Option[FunctionDescriptor] = {
    Option(bindingContext.get(BindingContext.FUNCTION, functionLiteralAst))
  }

  def astToAmbiguousReferenceTargetDescs(expression: KtExpression): collection.Seq[FunctionDescriptor] = {
    val descriptors = bindingContext.get(BindingContext.AMBIGUOUS_REFERENCE_TARGET, expression)
    if (descriptors == null) {
      return Seq.empty
    }

    descriptors.asScala.toSeq.collect { case functionDescriptor: FunctionDescriptor =>
      functionDescriptor
    }

  }
  
  def astToResolvedCallDesc(expr: KtExpression): Option[ResolvedCall[?]] = {
    val call         = Option(bindingContext.get(BindingContext.CALL, expr))
    val resolvedCall = call.flatMap(call => Option(bindingContext.get(BindingContext.RESOLVED_CALL, call)))
    
    resolvedCall
  }
  
  def ktExprToKotlinType(expr: KtExpression): Option[KotlinType] = {
    Option(bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr))
      .map(_.getType)
  }

}
