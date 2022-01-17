package io.joern.kotlin2cpg.types

import com.intellij.util.keyFMap.KeyFMap
import org.jetbrains.kotlin.descriptors.{DeclarationDescriptor, FunctionDescriptor, ValueDescriptor}
import org.jetbrains.kotlin.descriptors.impl.{ClassConstructorDescriptorImpl, TypeAliasConstructorDescriptorImpl}
import org.jetbrains.kotlin.psi.{
  KtBinaryExpression,
  KtCallExpression,
  KtClassLiteralExpression,
  KtClassOrObject,
  KtElement,
  KtExpression,
  KtNameReferenceExpression,
  KtNamedFunction,
  KtParameter,
  KtProperty,
  KtQualifiedExpression,
  KtTypeAlias
}
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.DescriptorUtils.getSuperclassDescriptors
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.{LazyClassDescriptor, LazyTypeAliasDescriptor}
import org.jetbrains.kotlin.types.{ErrorType, SimpleType, UnresolvedType}
import org.jetbrains.kotlin.cli.jvm.compiler.{
  KotlinCoreEnvironment,
  KotlinToJVMBytecodeCompiler,
  NoScopeRecordCliBindingTrace
}
import org.jetbrains.kotlin.renderer.{DescriptorRenderer, DescriptorRendererImpl, DescriptorRendererOptionsImpl}
import org.jetbrains.kotlin.resolve.BindingContext

import scala.jdk.CollectionConverters._
import org.slf4j.LoggerFactory
import KotlinTypeInfoProvider._
import org.jetbrains.kotlin.resolve.`lazy`.NoDescriptorForDeclarationException
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt

// representative of `LazyJavaClassDescriptor`, `DeserializedClassDescriptor`, `TypeAliasConstructorDescriptor`, etc.
trait WithDefaultType {
  def getDefaultType(): SimpleType
}

object Constants {
  val kotlinAny = "kotlin.Any"
  val any = "ANY"
  val classLiteralReplacementMethodName = "getClass"
}

object BindingKinds extends Enumeration {
  type BindingKind = Value
  val Unknown, Static, Dynamic = Value
}

trait TypeInfoProvider {
  def returnType(elem: KtNamedFunction, or: String): String
  def containingDeclType(expr: KtQualifiedExpression, or: String): String
  def expressionType(expr: KtExpression, or: String): String
  def inheritanceTypes(expr: KtClassOrObject, or: Seq[String]): Seq[String]
  def parameterType(expr: KtParameter, or: String): String
  def propertyType(expr: KtProperty, or: String): String
  def fullName(expr: KtClassOrObject, or: String): String
  def fullName(expr: KtTypeAlias, or: String): String
  def aliasTypeFullName(expr: KtTypeAlias, or: String): String
  def typeFullName(expr: KtNameReferenceExpression, or: String): String
  def bindingKind(expr: KtQualifiedExpression): BindingKinds.BindingKind
  def fullNameWithSignature(expr: KtQualifiedExpression, or: (String, String)): (String, String)
  def fullNameWithSignature(call: KtCallExpression, or: (String, String)): (String, String)
  def fullNameWithSignature(call: KtBinaryExpression, or: (String, String)): (String, String)
  def fullNameWithSignature(expr: KtNamedFunction, or: (String, String)): (String, String)
  def fullNameWithSignature(expr: KtClassLiteralExpression, or: (String, String)): (String, String)
}

object KotlinTypeInfoProvider {
  private val logger = LoggerFactory.getLogger(getClass)

  def bindingsForEntity(bindings: BindingContext, entity: KtElement): KeyFMap = {
    try {

      val thisField = bindings.getClass.getDeclaredField("this$0")
      thisField.setAccessible(true)
      val bindingTrace = thisField.get(bindings).asInstanceOf[NoScopeRecordCliBindingTrace]

      val mapField = bindingTrace.getClass.getSuperclass.getSuperclass.getDeclaredField("map")
      mapField.setAccessible(true)
      val map = mapField.get(bindingTrace)

      val mapMapField = map.getClass.getDeclaredField("map")
      mapMapField.setAccessible(true)
      val mapMap = mapMapField.get(map).asInstanceOf[java.util.Map[Object, KeyFMap]]

      val mapForEntity = mapMap.get(entity)
      mapForEntity
    } catch {
      case noSuchField: NoSuchFieldException =>
        logger.debug(
          "Encountered _no such field_ exception while retrieving type info for `" + entity.getName + "`: `" + noSuchField + "`."
        )
        KeyFMap.EMPTY_MAP
      case e: Throwable =>
        logger.debug(
          "Encountered general exception while retrieving type info for `" + entity.getName + "`: `" + e + "`."
        )
        KeyFMap.EMPTY_MAP
    }
  }

  def bindingsForEntityAsString(bindings: BindingContext, entity: KtElement): String = {
    val mapForEntity = bindingsForEntity(bindings, entity)
    if (mapForEntity != null) {
      val keys = mapForEntity.getKeys
      entity.toString + ": " + entity.getText + "\n" +
        keys.map(key => s"$key: ${mapForEntity.get(key)}").mkString("  ", "\n  ", "")
    } else {
      "No entries"
    }
  }

  def printBindingsForEntity(bindings: BindingContext, entity: KtElement): Unit = {
    println(bindingsForEntityAsString(bindings, entity))
  }
}

class KotlinTypeInfoProvider(environment: KotlinCoreEnvironment) extends TypeInfoProvider {
  private val logger = LoggerFactory.getLogger(getClass)

  // TODO: remove this state
  var hasEmptyBindingContext = false

  // TODO: consider the psiFactory fns like `createExpression` for
  // adding information to the PSI graph which would not be there otherwise
  /*
  lazy val psiFactory = {
    new KtPsiFactory(environment.getProject)
  }
   */

  lazy val bindingContext = {
    logger.info("Running Kotlin compiler analysis...")
    try {
      val t0 = System.currentTimeMillis()
      val res = KotlinToJVMBytecodeCompiler.INSTANCE.analyze(environment)
      val t1 = System.currentTimeMillis()
      logger.info("Kotlin compiler analysis finished in `" + (t1 - t0) + "` ms.")
      res.getBindingContext
    } catch {
      case noDesc: NoDescriptorForDeclarationException =>
        logger.error(
          "Kotlin compiler analysis failed with _missing declaration_ exception `" + noDesc.toString + "`."
        )
        hasEmptyBindingContext = true
        BindingContext.EMPTY
      case e: Throwable =>
        logger.error("Kotlin compiler analysis failed with exception `" + e.toString + "`.")
        hasEmptyBindingContext = true
        BindingContext.EMPTY
    }
  }

  // replaces `ERROR` types with `kotlin.Any`
  // TODO: consider erasing types over here
  def descriptorRenderer(desc: DeclarationDescriptor): DescriptorRenderer = {
    val anyT = DescriptorUtilsKt.getBuiltIns(desc).getAny()
    val opts = new DescriptorRendererOptionsImpl
    opts.setTypeNormalizer { t =>
      if (t.isInstanceOf[UnresolvedType]) {
        anyT.getDefaultType
      } else {
        t
      }
    }
    new DescriptorRendererImpl(opts)
  }

  def isValidRender(render: String): Boolean = {
    !render.contains("ERROR")
  }

  def stripped(typeName: String): String = {
    stripTypeParams(stripOptionality(stripDebugInfo(stripOut(typeName))).trim().replaceAll(" ", ""))
  }

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

  def fqName(current: String, desc: DeclarationDescriptor): String = {
    if (desc != null) {
      fqName(desc.getName.toString + current, desc.getContainingDeclaration)
    } else {
      current
    }
  }

  def fullName(expr: KtTypeAlias, or: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    if (mapForEntity.getKeys.contains(BindingContext.TYPE_ALIAS.getKey)) {
      val variableDesc = mapForEntity.get(BindingContext.TYPE_ALIAS.getKey)
      if (variableDesc == null) {
        return or
      }
      val renderer = descriptorRenderer(variableDesc)
      val fqName = DescriptorUtils.getFqName(variableDesc)
      val rendered = renderer.renderFqName(fqName)
      if (isValidRender(rendered)) {
        stripped(rendered)
      } else {
        or
      }
    } else {
      or
    }
  }

  def aliasTypeFullName(expr: KtTypeAlias, or: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    if (mapForEntity.getKeys.contains(BindingContext.TYPE_ALIAS.getKey)) {
      val variableDesc = mapForEntity.get(BindingContext.TYPE_ALIAS.getKey)
      if (variableDesc == null) {
        return or
      }
      val desc = variableDesc.asInstanceOf[LazyTypeAliasDescriptor]
      val renderer = descriptorRenderer(desc)
      val rendered = renderer.renderType(desc.getUnderlyingType)
      if (isValidRender(rendered)) {
        stripped(rendered)
      } else {
        or
      }
    } else {
      or
    }
  }

  def returnType(expr: KtNamedFunction, or: String): String = {
    val bindingInfo = bindingContext.get(BindingContext.FUNCTION, expr)
    if (bindingInfo != null) {
      val renderer = descriptorRenderer(bindingInfo)
      val rendered = renderer.renderType(bindingInfo.getReturnType)
      if (isValidRender(rendered)) {
        stripped(rendered)
      } else {
        or
      }
    } else {
      or
    }
  }

  def propertyType(expr: KtProperty, or: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    if (mapForEntity.getKeys.contains(BindingContext.VARIABLE.getKey)) {
      val variableDesc = mapForEntity.get(BindingContext.VARIABLE.getKey)
      val renderer = descriptorRenderer(variableDesc)
      val rendered = renderer.renderType(variableDesc.getType)
      if (isValidRender(rendered)) {
        stripped(rendered)
      } else {
        or
      }
    } else {
      or
    }
  }

  def inheritanceTypes(expr: KtClassOrObject, or: Seq[String]): Seq[String] = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    val descVal = mapForEntity.get(BindingContext.CLASS.getKey)
    if (descVal == null) {
      return or
    }
    val desc = descVal.asInstanceOf[LazyClassDescriptor]
    val renderer = descriptorRenderer(desc)
    val superClassDescriptors = getSuperclassDescriptors(desc)
    if (superClassDescriptors.size() > 0) {
      superClassDescriptors.asScala.map { superClassDesc =>
        val rendered = renderer.renderType(superClassDesc.getDefaultType)
        stripped(rendered)
      }.toList
    } else {
      or
    }
  }

  def fullName(expr: KtClassOrObject, or: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    if (mapForEntity.getKeys.contains(BindingContext.CLASS.getKey)) {
      val desc = mapForEntity.get(BindingContext.CLASS.getKey)
      val renderer = descriptorRenderer(desc)
      val rendered = renderer.renderType(desc.getDefaultType)
      if (isValidRender(rendered)) {
        stripped(rendered)
      } else {
        or
      }
    } else {
      or
    }
  }

  def expressionType(expr: KtExpression, or: String): String = {
    val bindingInfo = bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr)
    if (bindingInfo != null && bindingInfo.getType != null) {
      // TODO: use the renderer to render for this case as well
      //val renderer = descriptorRenderer(bindingInfo)
      val rendered = DescriptorRenderer.FQ_NAMES_IN_TYPES.renderType(bindingInfo.getType)
      if (isValidRender(rendered)) {
        stripped(rendered)
      } else {
        or
      }
    } else {
      or
    }
  }

  def fullNameWithSignature(expr: KtClassLiteralExpression, or: (String, String)): (String, String) = {
    val typeInfo = bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr)
    if (typeInfo != null && typeInfo.getType != null && typeInfo.getType.getArguments.size() > 0) {
      val firstTypeArg = typeInfo.getType.getArguments.get(0)
      val rendered = DescriptorRenderer.FQ_NAMES_IN_TYPES.renderType(firstTypeArg.getType)
      val retType = expressionType(expr, Constants.any)
      val signature = retType + "()"
      val fullName = rendered + "." + Constants.classLiteralReplacementMethodName + ":" + signature
      (fullName, signature)
    } else {
      (or._1, or._2)
    }
  }

  def fullNameWithSignature(expr: KtCallExpression, or: (String, String)): (String, String) = {
    val firstChild = expr.getFirstChild
    if (firstChild != null && firstChild.isInstanceOf[KtExpression]) {
      val asExpr = firstChild.asInstanceOf[KtExpression]
      val y = bindingContext.get(BindingContext.CALL, asExpr)
      val z = bindingContext.get(BindingContext.RESOLVED_CALL, y)

      if (z != null) {
        z.getResultingDescriptor match {
          case fnDescriptor: FunctionDescriptor =>
            val renderer = descriptorRenderer(fnDescriptor)

            // TODO: check this functionality for more elaborate subclassing situations
            val erasedTypeDescriptor =
              if (fnDescriptor.getOverriddenDescriptors.size > 0) {
                fnDescriptor.getOverriddenDescriptors.asScala.toList(0)
              } else {
                fnDescriptor
              }

            // TODO: write descriptor renderer instead of working with the existing ones
            // that render comments in fqnames
            val renderedFqName = stripped(
              DescriptorUtils.getFqName(erasedTypeDescriptor).toString
            )

            // render `kotlin.Any` instead of `???` if one of the type arguments cannot be inferred
            val retTypeConstructor = erasedTypeDescriptor.getReturnType.getConstructor
            val typeErrorCount = erasedTypeDescriptor.getReturnType.getArguments.asScala.toList
              .map(_.getType.isInstanceOf[ErrorType])
              .size
            val retType = if (typeErrorCount > 0) {
              val classifierName = renderer.renderClassifierName(
                retTypeConstructor.getDeclarationDescriptor
              )
              val argNames =
                erasedTypeDescriptor.getReturnType.getArguments.asScala.toList
                  .map { arg =>
                    if (arg.getType.isInstanceOf[ErrorType]) {
                      Constants.kotlinAny
                    } else {
                      stripped(renderer.renderType(arg.getType))
                    }
                  }
              classifierName + "<" + argNames.mkString(",") + ">"
            } else {
              stripped(
                renderer.renderType(fnDescriptor.getReturnType)
              )
            }

            val renderedParameterTypes =
              fnDescriptor.getValueParameters.asScala
                .map { vp =>
                  val rendered = renderer.renderType(vp.getType)
                  stripped(rendered)
                }
                .mkString(",")
            val signature = stripped(retType) + "(" + stripped(renderedParameterTypes) + ")"
            val fullName =
              if (
                fnDescriptor.isInstanceOf[ClassConstructorDescriptorImpl] ||
                fnDescriptor.isInstanceOf[TypeAliasConstructorDescriptorImpl]
              ) {
                stripped(renderedFqName) + "<init>:" + signature
              } else {
                stripped(renderedFqName) + ":" + signature
              }

            if (!isValidRender(fullName) || !isValidRender(signature)) {
              return (or._1, or._2)
            } else {
              return (fullName, signature)
            }
          case unhandled: Any =>
            logger.debug(
              s"Unhandled class in fetching type info for `${expr.getText}` with class `${unhandled.getClass}`."
            )
            return (or._1, or._2)
        }
      }
    }
    (or._1, or._2)
  }

  def fullNameWithSignature(expr: KtBinaryExpression, or: (String, String)): (String, String) = {
    val secondChild = expr.getChildren.toList(1)
    if (secondChild != null && secondChild.isInstanceOf[KtExpression]) {
      val asExpr = secondChild.asInstanceOf[KtExpression]
      val y = bindingContext.get(BindingContext.CALL, asExpr)
      val z = bindingContext.get(BindingContext.RESOLVED_CALL, y)

      if (z != null) {
        z.getResultingDescriptor match {
          case fnDescriptor: FunctionDescriptor =>
            // TODO: check this functionality for more elaborate subclassing situations
            val erasedTypeDescriptor =
              if (fnDescriptor.getOverriddenDescriptors.size > 0) {
                fnDescriptor.getOverriddenDescriptors.asScala.toList(0)
              } else {
                fnDescriptor
              }

            // TODO: write descriptor renderer instead of working with the existing ones
            // that render comments in fqnames
            val fqName = DescriptorUtils.getFqName(erasedTypeDescriptor)
            val renderer = descriptorRenderer(erasedTypeDescriptor)
            val renderedFqName = stripped(renderer.renderFqName(fqName))
            val retType = stripped(renderer.renderType(erasedTypeDescriptor.getReturnType))

            val renderedParameterTypes =
              fnDescriptor.getValueParameters.asScala
                .map { vp =>
                  val rendered = renderer.renderType(vp.getType)
                  stripped(rendered)
                }
                .mkString(",")
            val signature = stripped(retType) + "(" + renderedParameterTypes + ")"
            val fullName =
              if (fnDescriptor.isInstanceOf[ClassConstructorDescriptorImpl]) {
                stripped(retType) + ".<init>:" + signature
              } else {
                renderedFqName + ":" + signature
              }
            if (!isValidRender(fullName) || !isValidRender(signature)) {
              return (or._1, or._2)
            } else {
              return (fullName, signature)
            }
          case unhandled: Any =>
            logger.debug(
              s"Unhandled class in fetching type info for `${expr.getText}` with class `${unhandled.getClass}`."
            )
            return or
        }
      }
    }
    (or._1, or._2)
  }

  def containingDeclType(expr: KtQualifiedExpression, or: String): String = {
    val selectorExpr = expr.getSelectorExpression

    selectorExpr match {
      case call: KtCallExpression =>
        val firstChild = call.getFirstChild
        if (firstChild != null && firstChild.isInstanceOf[KtExpression]) {
          val asExpr = firstChild.asInstanceOf[KtExpression]
          val y = bindingContext.get(BindingContext.CALL, asExpr)
          if (y == null) {
            return or
          }
          val z = bindingContext.get(BindingContext.RESOLVED_CALL, y)
          if (z != null) {
            z.getResultingDescriptor match {
              case fnDescriptor: FunctionDescriptor =>
                val decl = fnDescriptor.getContainingDeclaration
                val renderedFqName = DescriptorUtils.getFqName(decl)
                return renderedFqName.toString
              case unhandled: Any =>
                logger.debug(
                  s"Unhandled class in fetching type info for `${expr.getText}` with class `${unhandled.getClass}`."
                )
                return or
            }
          }
        }
      case _ =>
        or
    }
    or
  }

  def bindingKind(expr: KtQualifiedExpression): BindingKinds.BindingKind = {
    val selectorExpr = expr.getSelectorExpression

    selectorExpr match {
      case call: KtCallExpression =>
        val firstChild = call.getFirstChild
        if (firstChild != null && firstChild.isInstanceOf[KtExpression]) {
          val asExpr = firstChild.asInstanceOf[KtExpression]
          val y = bindingContext.get(BindingContext.CALL, asExpr)
          if (y == null) {
            logger.debug("Retrieved empty binding context info for `" + expr.getName + "`.")
            return BindingKinds.Unknown
          }
          val z = bindingContext.get(BindingContext.RESOLVED_CALL, y)
          if (z != null) {
            z.getResultingDescriptor match {
              case fnDescriptor: FunctionDescriptor =>
                val isStatic = DescriptorUtils.isStaticDeclaration(fnDescriptor)
                return if (isStatic) BindingKinds.Static else BindingKinds.Dynamic
              case unhandled: Any =>
                logger.debug(
                  s"Unhandled class in fetching type info for `${expr.getText}` with class `${unhandled.getClass}`."
                )
                return BindingKinds.Unknown
            }
          }
        }
      case _ =>
    }
    BindingKinds.Unknown
  }

  def fullNameWithSignature(
      expr: KtQualifiedExpression,
      or: (String, String)
  ): (String, String) = {
    val selectorExpr = expr.getSelectorExpression

    selectorExpr match {
      case call: KtCallExpression =>
        val firstChild = call.getFirstChild
        if (firstChild != null && firstChild.isInstanceOf[KtExpression]) {
          val asExpr = firstChild.asInstanceOf[KtExpression]
          val y = bindingContext.get(BindingContext.CALL, asExpr)
          // TODO: add more specific test case for situations in which type inference fails for
          // a certain type of expressions
          // e.g. for
          //   -> https://github.com/coil-kt/coil
          //   -> https://github.com/mozilla-lockwise/lockwise-android
          if (y == null) {
            logger.debug("Retrieved empty binding context info for `" + expr.getName + "`.")
            return (or._1, or._2)
          }
          val z = bindingContext.get(BindingContext.RESOLVED_CALL, y)

          if (z != null) {
            z.getResultingDescriptor match {
              case fnDescriptor: FunctionDescriptor =>
                val renderer = descriptorRenderer(fnDescriptor)

                // TODO: check this functionality for more elaborate subclassing situations
                val erasedTypeDescriptor =
                  if (fnDescriptor.getOverriddenDescriptors.size > 0) {
                    fnDescriptor.getOverriddenDescriptors.asScala.toList(0)
                  } else {
                    fnDescriptor
                  }
                val renderedFqName =
                  if (erasedTypeDescriptor.getExtensionReceiverParameter != null) {
                    val extType = erasedTypeDescriptor.getExtensionReceiverParameter.getType
                    val extName = erasedTypeDescriptor.getName
                    val rendered = renderer.renderType(extType)
                    stripped(rendered) + "." + extName
                  } else {
                    val fqName = DescriptorUtils.getFqName(erasedTypeDescriptor)
                    renderer.renderFqName(fqName)
                  }

                val renderedParameterTypes =
                  fnDescriptor.getValueParameters.asScala
                    .map { vp =>
                      // TODO: good place to handle Function1
                      val rendered = renderer.renderType(vp.getType)
                      stripped(rendered)
                    }
                    .mkString(",")

                val bindingInfo = bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr)
                if (bindingInfo != null && bindingInfo.getType != null) {
                  val rendered =
                    if (bindingInfo.getType.isInstanceOf[UnresolvedType]) {
                      Constants.kotlinAny
                    } else {
                      renderer.renderType(bindingInfo.getType)
                    }
                  val signature = stripped(rendered) + "(" + renderedParameterTypes + ")"
                  val fn = (renderedFqName + ":" + signature, signature)
                  return fn
                }

              case unhandled: Any =>
                logger.debug(
                  s"Unhandled class in fetching type info for `${expr.getText}` with class `${unhandled.getClass}`."
                )
                return (or._1, or._2)
            }
          }
        }
      case _ =>
    }
    (or._1, or._2)
  }

  def parameterType(expr: KtParameter, or: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    // TODO: add specific test for no binding info of parameter
    // triggered by exception in https://github.com/agrosner/DBFlow
    // TODO: ...also test cases for non-null binding info for other fns
    if (mapForEntity == null || mapForEntity.getKeys == null) {
      return or
    }
    if (mapForEntity.getKeys.contains(BindingContext.VALUE_PARAMETER.getKey)) {
      val variableDesc = mapForEntity.get(BindingContext.VALUE_PARAMETER.getKey)
      val renderer = descriptorRenderer(variableDesc)
      val rendered = renderer.renderType(variableDesc.getType)
      if (isValidRender(rendered)) {
        stripped(rendered)
      } else {
        or
      }
    } else {
      or
    }
  }

  def fullNameWithSignature(expr: KtNamedFunction, or: (String, String)): (String, String) = {
    val returnTypeFullName = returnType(expr, Constants.any)

    val paramTypeNames =
      try {
        val nodeParams = expr.getValueParameters()
        nodeParams.asScala
          .map { p =>
            val explicitTypeFullName = if (p.getTypeReference() != null) {
              p.getTypeReference().getText()
            } else {
              Constants.any
            }
            // TODO: return all the parameter types in this fn for registration, otherwise they will be missing
            val typeFullName = parameterType(p, stripped(explicitTypeFullName))
            typeFullName
          }
      } catch {
        case _: Throwable => List()
      }
    val paramListSignature = "(" + paramTypeNames.mkString(",") + ")"

    val bindingInfo = bindingContext.get(BindingContext.FUNCTION, expr)
    val methodName =
      if (bindingInfo != null && bindingInfo.getExtensionReceiverParameter != null) {
        val erpType = bindingInfo.getExtensionReceiverParameter.getType
        if (erpType.isInstanceOf[UnresolvedType]) {
          Constants.kotlinAny + "." + expr.getName
        } else {
          val renderer = descriptorRenderer(bindingInfo)
          val extFqName = renderer.renderType(
            bindingInfo.getExtensionReceiverParameter.getType
          )
          stripped(extFqName) + "." + expr.getName
        }
      } else {
        expr.getFqName
      }

    val signature = returnTypeFullName + paramListSignature
    val fullname = s"$methodName:$signature"
    (fullname, signature)
  }

  def typeFullName(expr: KtNameReferenceExpression, or: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    if (mapForEntity == null) {
      return or
    }
    val targetDesc = bindingContext.get(BindingContext.REFERENCE_TARGET, expr)
    if (targetDesc == null) {
      return or
    }
    val renderer = descriptorRenderer(targetDesc)
    val rendered: Option[String] = targetDesc match {
      case typedDesc: ValueDescriptor =>
        Some(stripped(renderer.renderType(typedDesc.getType())))
      case typedDesc: WithDefaultType =>
        Some(stripped(renderer.renderType(typedDesc.getDefaultType())))
      case unhandled: Any =>
        logger.debug(s"Unhandled class in fetching type info for `${expr.getText}` with class `${unhandled.getClass}`.")
        None
    }

    rendered match {
      case Some(r) => r
      case None    => or
    }
  }
}
