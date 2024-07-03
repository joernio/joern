package io.joern.console

import org.reflections8.Reflections
import org.reflections8.util.{ClasspathHelper, ConfigurationBuilder}

import java.lang.reflect.{Method, Parameter}
import scala.annotation.unused
import scala.jdk.CollectionConverters.*

trait QueryBundle

class QueryDatabase(
  defaultArgumentProvider: DefaultArgumentProvider = new DefaultArgumentProvider,
  namespace: String = "io.joern.scanners"
) {

  /** Determine all bundles on the class path
    */
  def allBundles: List[Class[? <: QueryBundle]] =
    new Reflections(
      new ConfigurationBuilder().setUrls(
        ClasspathHelper.forPackage(namespace, ClasspathHelper.contextClassLoader(), ClasspathHelper.staticClassLoader())
      )
    ).getSubTypesOf(classOf[QueryBundle]).asScala.toList

  /** Determine queries across all bundles
    */
  def allQueries: List[Query] = {
    allBundles.flatMap { bundle =>
      queriesInBundle(bundle)
    }
  }

  /** Return all queries inside `bundle`.
    */
  def queriesInBundle[T <: QueryBundle](bundle: Class[T]): List[Query] = {
    val instance = bundle.getField("MODULE$").get(null)
    queryCreatorsInBundle(bundle).map { case (method, args) =>
      val query           = method.invoke(instance, args*).asInstanceOf[Query]
      val bundleNamespace = bundle.getPackageName
      // the namespace currently looks like `io.joern.scanners.c.CopyLoops`
      val namespaceParts = bundleNamespace.split('.')
      val language =
        if (bundleNamespace.startsWith("io.shiftleft.ocular.scanners")) {
          namespaceParts(4)
        } else if (namespaceParts.length > 3) {
          namespaceParts(3)
        } else {
          ""
        }
      query.copy(language = language)
    }
  }

  /** Obtain all (method, args) pairs from bundle, making it possible to override default args before creating the
    * query.
    */
  def queryCreatorsInBundle[T <: QueryBundle](bundle: Class[T]): List[(Method, List[Any])] = {
    val methods = bundle.getMethods.filter(_.getAnnotations.exists(_.isInstanceOf[q])).toList
    methods.map { method =>
      val args = defaultArgs(method, bundle)
      (method, args)
    }
  }

  private def defaultArgs[T <: QueryBundle](method: Method, bundle: Class[T]): List[Any] = {
    method.getParameters.zipWithIndex.map { case (parameter, index) =>
      defaultArgumentProvider.defaultArgument(method, bundle, parameter, index)
    }.toList
  }

}

/** Joern and Ocular require different implicits to be present, and when we encounter these implicits as parameters in a
  * query that we invoke via reflection, we need to obtain these implicits from somewhere.
  *
  * We achieve this by implementing a `DefaultArgumentProvider` for Ocular, and one for Joern.
  */
class DefaultArgumentProvider {

  def typeSpecificDefaultArg(@unused argTypeFullName: String): Option[Any] = {
    None
  }

  final def defaultArgument(method: Method, bundle: Class[?], parameter: Parameter, i: Int): Any = {
    val instance         = bundle.getField("MODULE$").get(null)
    val defaultArgOption = typeSpecificDefaultArg(parameter.getType.getTypeName)
    defaultArgOption.getOrElse {
      val defaultMethodName = s"${method.getName}$$default$$${i + 1}"
      try {
        val defaultMethod = bundle.getDeclaredMethod(defaultMethodName)
        val defaultValue  = defaultMethod.invoke(instance)
        defaultValue
      } catch {
        case e: NoSuchMethodException =>
          throw new RuntimeException(
            s"No default value found for parameter `${parameter.toString}` of query creator method `$method` "
          )
      }
    }
  }

}
