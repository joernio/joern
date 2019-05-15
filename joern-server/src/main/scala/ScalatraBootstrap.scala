import akka.actor.ActorSystem
import io.shiftleft.joern.server._
import org.scalatra._
import javax.servlet.ServletContext

class ScalatraBootstrap extends LifeCycle {

  implicit val swagger = new JoernSwagger

  val system = ActorSystem()

  override def init(context: ServletContext) {
    context.mount(new JoernController(system), "/*")
    context.mount(new ResourcesApp, "/api-docs")
  }
}
