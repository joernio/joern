package io.shiftleft.resolver.impl

import cats.effect.Resource
import cats.effect.kernel.Sync
import cats.syntax.all.*
import io.shiftleft.resolver.api.{Coordinate, Resolver}
import org.apache.maven.artifact.repository.MavenArtifactRepository
import org.apache.maven.artifact.repository.layout.DefaultRepositoryLayout
import org.apache.maven.project.{DefaultProjectBuildingRequest, ProjectBuilder}
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.codehaus.plexus.component.repository.ComponentDescriptor
import org.codehaus.plexus.{DefaultContainerConfiguration, DefaultPlexusContainer, PlexusConstants, PlexusContainer}
import org.eclipse.aether.spi.connector.filter.{RemoteRepositoryFilter, RemoteRepositoryFilterSource}

import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import javax.inject.Named
import javax.inject.Singleton
import org.eclipse.aether.RepositorySystemSession
import org.eclipse.aether.artifact.Artifact
import org.eclipse.aether.metadata.Metadata
import org.eclipse.aether.repository.RemoteRepository

object ResolverMavenOfficial {
  def createPlexusContainer[F[_]: Sync](): Resource[F, PlexusContainer] = {
    Resource.make(Sync[F].blocking(createContainerInternal()))(container => Sync[F].blocking(container.dispose()))
  }
  
  private def createContainerInternal(): PlexusContainer = {
    val plexusConfig = new DefaultContainerConfiguration()
      .setName("plexus")
      .setAutoWiring(true)
      .setClassPathScanning(PlexusConstants.SCANNING_INDEX)

    val plexusContainer = new DefaultPlexusContainer(plexusConfig)
    val compDesc = new ComponentDescriptor[MyRemoteRepoFilterSource]()
    compDesc.setRole(classOf[RemoteRepositoryFilterSource].getName)
    compDesc.setImplementation(classOf[MyRemoteRepoFilterSource].getName)
    compDesc.setRoleHint("default")
    plexusContainer.addComponentDescriptor(compDesc)
    
    plexusContainer
  }

  
  @Singleton
  @Named("MyRemoteRepoFilterSource")
  final class MyRemoteRepoFilterSource extends RemoteRepositoryFilterSource {
    override def getRemoteRepositoryFilter(session: RepositorySystemSession) = new MyRemoteRepoFilter
  }

  class MyRemoteRepoFilter extends RemoteRepositoryFilter {

    override def acceptArtifact(remoteRepository: RemoteRepository,
                                artifact: Artifact): RemoteRepositoryFilter.Result = {
      new RemoteRepositoryFilter.Result {
        override def isAccepted: Boolean = {
          artifact.getExtension == "pom"
        }

        override def reasoning(): String = {
          "Only pom artifacts are accepted"
        }
      }

    }

    override def acceptMetadata(remoteRepository: RemoteRepository, metadata: Metadata): RemoteRepositoryFilter.Result = {
      new RemoteRepositoryFilter.Result {
        override def isAccepted: Boolean = {
          true
        }

        override def reasoning(): String = {
          "Always true"
        }
      }
    }
  }
}

class ResolverMavenOfficial[F[_]: Sync](plexusContainer: PlexusContainer,
                                        localRepoPath: Path) extends Resolver[F, IdMaven, Path] {
  def resolve(pomFile: Path): F[Vector[Coordinate[IdMaven]]] = {
    Sync[F].interruptible {
      val builder = plexusContainer.lookup(classOf[ProjectBuilder])

      val localRepo = new MavenArtifactRepository()
      //localRepo.setUrl("file:///tmp/localRepo2")
      localRepo.setUrl(localRepoPath.toUri.toURL.toString)
      localRepo.setLayout(new DefaultRepositoryLayout)

      val request = new DefaultProjectBuildingRequest()
      request.setLocalRepository(localRepo)
      request.setResolveDependencies(true)
      request.setRepositorySession(MavenRepositorySystemUtils.newSession())

      val result = builder.build(pomFile.toFile, request)
      result
    }.flatMap { result =>
      val problems = result.getProblems.asScala
      if (problems.nonEmpty) {
        Sync[F].raiseError(new RuntimeException(s"Problems while resolving:\n${problems.mkString("\n")}"))
      } else {
        Sync[F].delay(
          result.getDependencyResolutionResult.getDependencies.asScala.map { dep =>
            val artifact = dep.getArtifact
            Coordinate(IdMaven(artifact.getGroupId, artifact.getArtifactId), artifact.getVersion)
          }.toVector)
      }
    }
  }

}
