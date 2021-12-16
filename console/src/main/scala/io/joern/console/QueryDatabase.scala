package io.joern.console

import io.joern.console.Query
import org.reflections8.Reflections
import org.reflections8.util.{ClasspathHelper, ConfigurationBuilder}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.{StaticAnnotation, unused}
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

trait QueryBundle
class q() extends StaticAnnotation

class QueryDatabase(defaultArgumentProvider: DefaultArgumentProvider = new DefaultArgumentProvider,
                    namespace: String = "io.joern.scanners") {

  private val logger: Logger = LoggerFactory.getLogger(classOf[QueryDatabase])

  private val runtimeMirror: ru.Mirror =
    ru.runtimeMirror(getClass.getClassLoader)

  /**
    * Determine all bundles on the class path
    * */
  def allBundles: List[Class[_ <: QueryBundle]] =
    new Reflections(
      new ConfigurationBuilder().setUrls(
        ClasspathHelper.forPackage(namespace,
                                   ClasspathHelper.contextClassLoader(),
                                   ClasspathHelper.staticClassLoader()))
    ).getSubTypesOf(classOf[QueryBundle]).asScala.toList

  /**
    * Determine queries across all bundles
    * */
  def allQueries: List[Query] = {
    allBundles.flatMap { bundle =>
      queriesInBundle(bundle)
    }
  }

  /**
    * Return all queries inside `bundle`.
    * */
  def queriesInBundle[T <: QueryBundle](bundle: Class[T]): List[Query] = {
    queryCreatorsInBundle(bundle).map {
      case (method, args) => {
        val query = method.apply(args: _*).asInstanceOf[Query]
        val bundleNamespace = classToType(bundle).typeSymbol.fullName.toString
        // the namespace currently looks like `io.joern.scanners.c.CopyLoops`
        val namespaceParts = bundleNamespace.split('.')
        val language =
          if (bundleNamespace.startsWith("io.shiftleft.ocular.scanners")) {
            namespaceParts(4)
          } else if (namespaceParts.size > 3) {
            namespaceParts(3)
          } else {
            ""
          }
        query.copy(language = language)
      }
    }
  }

  /**
    * Obtain all (methodMirror, args) pairs from bundle, making it possible to override
    * default args before creating the query.
    * */
  def queryCreatorsInBundle[T <: QueryBundle](bundle: Class[T]): List[(ru.MethodMirror, List[Any])] = {
    methodsForBundle(bundle).map(m => (m, bundle)).flatMap {
      case (method, bundle) =>
        val args = defaultArgs(method.symbol, classToType(bundle))
        if (args.isDefined) {
          List((method, args.get))
        } else {
          logger.warn(s"Cannot determine default arguments for query: $method")
          List()
        }

    }
  }

  private def classToType[T](x: Class[T]) = {
    runtimeMirror.classSymbol(x).toType
  }

  private def methodsForBundle[T <: QueryBundle](bundle: Class[T]) = {
    val bundleType = classToType(bundle)
    val methods = bundleType.members
      .collect { case m if m.isMethod => m.asMethod }
      .filter { m =>
        m.annotations.map(_.tree.tpe.typeSymbol.name.toString).contains("q")
      }

    val im = runtimeMirror.reflect(
      runtimeMirror
        .reflectModule(bundleType.typeSymbol.asClass.module.asModule)
        .instance)
    methods.map { m =>
      im.reflectMethod(m)
    }.toList
  }

  private def defaultArgs(method: MethodSymbol, bundleType: Type): Option[List[Any]] = {
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val im = runtimeMirror.reflect(
      runtimeMirror
        .reflectModule(bundleType.typeSymbol.asClass.module.asModule)
        .instance)
    val args = (for (ps <- method.paramLists; p <- ps) yield p).zipWithIndex
      .map {
        case (x, i) => {
          val defaultValue = defaultArgumentProvider.defaultArgument(method, im, x, i)
          if (defaultValue.isEmpty) {
            throw new RuntimeException(
              s"No default value found for parameter `${x.toString}` of query creator method `$method` ")
          }
          defaultValue
        }
      }
    if (args.contains(None)) {
      None
    } else {
      Some(args.map(_.get))
    }
  }

}

/**
  * Joern and Ocular require different implicits to be present, and when
  * we encounter these implicits as parameters in a query that we invoke
  * via reflection, we need to obtain these implicits from somewhere.
  *
  * We achieve this by implementing a `DefaultArgumentProvider` for Ocular,
  * and one for Joern.
  * */
class DefaultArgumentProvider {

  def typeSpecificDefaultArg(argTypeFullName: String): Option[Any] = {
    None
  }

  final def defaultArgument(method: MethodSymbol, im: InstanceMirror, x: Symbol, i: Int): Option[Any] = {
    val defaultArgOption = typeSpecificDefaultArg(x.typeSignature.toString)
    defaultArgOption.orElse {
      val typeSignature = im.symbol.typeSignature
      val defaultMethodName = s"${method.name}$$default$$${i + 1}"
      val m = typeSignature.member(TermName(defaultMethodName))
      if (m.isMethod) {
        Some(im.reflectMethod(m.asMethod).apply())
      } else {
        None
      }
    }
  }

}
