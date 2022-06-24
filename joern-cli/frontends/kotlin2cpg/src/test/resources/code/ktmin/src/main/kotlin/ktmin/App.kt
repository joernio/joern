package ktmin

import com.google.gson.Gson

data class Model(val id: Int, val message: String)
fun doSomething(msg: String): String {
  val g = Gson()
  val m = g.fromJson("{'id': 1, 'message': '$msg'}", Model::class.java)
  return m.message
}

fun main() {
  val aMessage = "ktmin"
  val out = doSomething(aMessage)
  println(out)
}
