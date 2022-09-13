//object WS0 {
 // def workspace0: String = "workspace0 str"
//}

@main def exec: Unit = {
  //import WS0._
  println("hello world")
  //workspace // .reset()
  //println(workspace0)
  
  //import WS1.bar
  println(bar(4))
}
