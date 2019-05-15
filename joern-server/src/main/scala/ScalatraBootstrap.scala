import io.shiftleft.cpgserver.CpgServerController
import io.shiftleft.cpgserver.{CpgServerSwagger, ResourcesApp}
import io.shiftleft.joern.server._
import javax.servlet.ServletContext
import org.scalatra._

class ScalatraBootstrap extends LifeCycle {

  implicit val swagger = new CpgServerSwagger

  override def init(context: ServletContext) {
    context.mount(new CpgServerController(new JoernServerImpl), "/*")
    context.mount(new ResourcesApp, "/api-docs")
  }
}
