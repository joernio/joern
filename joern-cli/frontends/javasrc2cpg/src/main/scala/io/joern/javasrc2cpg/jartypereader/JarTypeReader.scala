package io.joern.javasrc2cpg.jartypereader

import io.joern.javasrc2cpg.jartypereader.descriptorparser.DescriptorParser
import io.joern.javasrc2cpg.jartypereader.model.{
  ClassSignature,
  ClassTypeSignature,
  NameWithTypeArgs,
  ResolvedVariableType,
  ResolvedMethod,
  ResolvedTypeDecl
}
import io.shiftleft.utils.ProjectRoot
import javassist.{ClassPool, CtClass, CtField, CtMethod, NotFoundException}
import org.slf4j.LoggerFactory

import java.util.jar.JarFile
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.{Failure, Success, Try}

object JarTypeReader {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val ObjectTypeSignature: ClassTypeSignature = {
    val packageSpecifier = Some("java.lang")
    val signature        = NameWithTypeArgs(name = "Object", typeArguments = Nil)
    ClassTypeSignature(packageSpecifier, signature, suffix = Nil)
  }

  def getTypes(jarPath: String): List[ResolvedTypeDecl] = {
    val cp = new ClassPool()
    cp.insertClassPath(jarPath)

    val jarFile = new JarFile(jarPath)
    jarFile
      .entries()
      .asScala
      .filter(_.getName.endsWith(".class"))
      .map { entry =>
        entry.getRealName.replace("/", ".").dropRight(6) // Drop .class extension
      }
      .flatMap(getTypeDeclForEntry(cp, _))
      .toList

  }

  def getTypeEntryForMethod(method: CtMethod, parentDecl: ResolvedTypeDecl): ResolvedMethod = {
    val name                = method.getName
    val signatureDescriptor = Option(method.getGenericSignature).getOrElse(method.getSignature)
    val signature           = DescriptorParser.parseMethodSignature(signatureDescriptor)
    val isAbstract          = method.isEmpty

    ResolvedMethod(name, parentDecl, signature, isAbstract)
  }

  /** This should only be used for classes without a generic signature. Generic type information will be missing
    * otherwise.
    */
  private def classTypeSignatureFromString(signature: String): ClassTypeSignature = {
    signature.split('.').toList match {
      case Nil =>
        logger.warn(s"$signature is not a valid class signature")
        ClassTypeSignature(None, NameWithTypeArgs("", Nil), Nil)

      case name :: Nil => ClassTypeSignature(None, typedName = NameWithTypeArgs(name, Nil), Nil)

      case nameWithPkg =>
        val packagePrefix = nameWithPkg.init.mkString(".")
        val name          = nameWithPkg.last
        ClassTypeSignature(Some(packagePrefix), NameWithTypeArgs(name, Nil), Nil)

    }
  }

  /** This should only be used for classes without a generic signature. Generic type information will be missing
    * otherwise.
    */
  private def getCtClassSignature(ctClass: CtClass): ClassSignature = {
    val classFile      = ctClass.getClassFile2
    val typeParameters = Nil
    val superclassSignature =
      Option.unless(classFile.isInterface)(classTypeSignatureFromString(classFile.getSuperclass))
    val interfacesSignatures = classFile.getInterfaces.map(classTypeSignatureFromString).toList

    ClassSignature(typeParameters, superclassSignature, interfacesSignatures)
  }

  private def getResolvedField(ctField: CtField): ResolvedVariableType = {
    val name                = ctField.getName
    val signatureDescriptor = Option(ctField.getGenericSignature).getOrElse(ctField.getSignature)
    val signature           = DescriptorParser.parseFieldSignature(signatureDescriptor)

    ResolvedVariableType(name, signature)
  }

  def getTypeDeclForEntry(cp: ClassPool, name: String): Option[ResolvedTypeDecl] = {
    Try(cp.get(name)) match {
      case Success(ctClass) =>
        val name             = ctClass.getSimpleName
        val packageSpecifier = ctClass.getPackageName
        val signature = Option(ctClass.getGenericSignature)
          .map(DescriptorParser.parseClassSignature)
          .getOrElse(getCtClassSignature(ctClass))
        val isInterface = ctClass.isInterface
        val isAbstract  = !isInterface && ctClass.getClassFile2.isAbstract
        val fields      = ctClass.getFields.map(getResolvedField).toList
        val typeDecl    = ResolvedTypeDecl(name, Some(packageSpecifier), signature, isInterface, isAbstract, fields)
        val methods     = ctClass.getMethods.map(getTypeEntryForMethod(_, typeDecl)).toList
        typeDecl.addMethods(methods)

        Some(typeDecl)

      case Failure(_: NotFoundException) =>
        // TODO: Expected for interfaces, but fix this
        None

      case Failure(_) =>
        None
    }
  }
}
