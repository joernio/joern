import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MainTest extends AnyWordSpec with Matchers {
  "run main function" in {
    Main.main(Array())
  }
}
