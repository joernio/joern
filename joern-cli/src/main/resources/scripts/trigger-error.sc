import scala.util.control.NoStackTrace
throw new Exception("triggering an error for testing purposes") with NoStackTrace
