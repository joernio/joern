import io.shiftleft.joern.server._
import org.scalatra._
import javax.servlet.ServletContext

class ScalatraBootstrap extends LifeCycle {

  implicit val swagger = new JoernSwagger

  override def init(context: ServletContext) {
    context.mount(new JoernController, "/", "root")
    context.mount (new ResourcesApp, "/api-docs")
  }
}