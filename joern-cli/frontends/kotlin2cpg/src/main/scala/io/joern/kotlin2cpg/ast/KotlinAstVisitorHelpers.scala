package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.descriptors.DeclarationDescriptor
import org.jetbrains.kotlin.descriptors.DeclarationDescriptorWithSource
import org.jetbrains.kotlin.descriptors.ValueParameterDescriptor
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.resolve.source.KotlinSourceElement

import scala.collection.mutable

trait KotlinAstVisitorHelpers {
  this: AstCreator =>

  protected def referencedCaptureNamesIn(root: KtElement, declaredNames: Set[String]): Set[String] = {
    val referenced     = mutable.Set.empty[String]
    val declaredScopes = mutable.ArrayBuffer(mutable.Set.from(declaredNames))

    def withScope(body: => Unit): Unit = {
      declaredScopes.addOne(mutable.Set.empty[String])
      try {
        body
      } finally {
        declaredScopes.remove(declaredScopes.size - 1)
      }
    }

    def addDeclaredName(name: String): Unit = {
      declaredScopes.last.add(name)
    }

    def isDeclared(name: String): Boolean = {
      declaredScopes.reverseIterator.exists(_.contains(name))
    }

    val visitor = new KtTreeVisitorVoid {
      override def visitBlockExpression(expression: KtBlockExpression): Unit = {
        withScope {
          super.visitBlockExpression(expression)
        }
      }

      override def visitParameter(parameter: KtParameter): Unit = {
        Option(parameter.getName).foreach(addDeclaredName)
        super.visitParameter(parameter)
      }

      override def visitProperty(property: KtProperty): Unit = {
        Option(property.getName).foreach(addDeclaredName)
        super.visitProperty(property)
      }

      override def visitDestructuringDeclarationEntry(entry: KtDestructuringDeclarationEntry): Unit = {
        Option(entry.getName).foreach(addDeclaredName)
        super.visitDestructuringDeclarationEntry(entry)
      }

      override def visitReferenceExpression(expression: KtReferenceExpression): Unit = {
        expression match {
          case nameRef: KtNameReferenceExpression =>
            val referencedName = nameRef.getReferencedName
            if (!isDeclared(referencedName)) {
              referenced.add(referencedName)
            }
            if (typeInfoProvider.usedAsImplicitThis(nameRef)) {
              if (!isDeclared(Constants.ThisName)) {
                referenced.add(Constants.ThisName)
              }
            }
          case _ =>
        }
        super.visitReferenceExpression(expression)
      }

      override def visitThisExpression(expression: KtThisExpression): Unit = {
        if (!isDeclared(Constants.ThisName)) {
          referenced.add(Constants.ThisName)
        }
        super.visitThisExpression(expression)
      }
    }

    visitor.visitKtElement(root)
    referenced.toSet
  }

  protected def collectReferencedNamesFromExpression(
    expression: KtExpression,
    visitedDescriptorKeys: mutable.Set[String],
    visitedPsiElements: mutable.Set[PsiElement]
  ): Set[String] = {
    val names = mutable.Set.empty[String]

    def addNamesFromPropertyInitializer(property: KtProperty): Unit = {
      if (visitedPsiElements.add(property)) {
        Option(property.getInitializer).foreach { initializer =>
          names.addAll(collectReferencedNamesFromExpression(initializer, visitedDescriptorKeys, visitedPsiElements))
        }
      }
    }

    def propertyFromDescriptor(descriptor: DeclarationDescriptor): Option[KtProperty] = {
      descriptor match {
        case descriptorWithSource: DeclarationDescriptorWithSource =>
          Option(descriptorWithSource.getSource)
            .collect { case source: KotlinSourceElement => source.getPsi }
            .collect { case property: KtProperty => property }
        case _ => None
      }
    }

    val visitor = new KtTreeVisitorVoid {
      override def visitReferenceExpression(referenceExpression: KtReferenceExpression): Unit = {
        val resolvedReferencePsi = referenceExpression match {
          case nameReference: KtNameReferenceExpression =>
            nameReference.getReferences.headOption.flatMap(reference => Option(reference.resolve()))
          case _ => None
        }

        resolvedReferencePsi.foreach {
          case parameter: KtParameter =>
            Option(parameter.getName).foreach(names.add)
          case property: KtProperty =>
            addNamesFromPropertyInitializer(property)
          case _ =>
        }

        bindingUtils.getDeclDesc(referenceExpression).foreach { descriptor =>
          val descriptorId = s"${descriptor.getClass.getName}:${descriptor.toString}"
          if (visitedDescriptorKeys.add(descriptorId)) {
            descriptor match {
              case valueParameterDescriptor: ValueParameterDescriptor =>
                names.add(valueParameterDescriptor.getName.asString)
              case _ =>
                propertyFromDescriptor(descriptor).foreach(addNamesFromPropertyInitializer)
            }
          }
        }
        super.visitReferenceExpression(referenceExpression)
      }
    }

    visitor.visitKtElement(expression)
    names.toSet
  }
}
