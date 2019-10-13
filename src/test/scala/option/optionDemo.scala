package option

import Option._

object optionDemo extends App {
  println("-"*50)
  println(map3_2(Some(1),Some(20),Some(3))((a,b,c)=>a+b+c))
  println("-"*50)
  println(lift(math.abs)(Some(-1)))
  println("-"*50)
  println(sequence(List(Option(1),Option(2),Option(3))).getOrElse(None))
  println("-"*50)
  println(sequence(List("1","2","3").map(a=>Try(a.toInt))))
  println("-"*50)
  println(sequence(List("1","2","abc").map(a=>Try(a.toInt))))
}
