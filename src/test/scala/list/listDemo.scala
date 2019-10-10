package list
import list._,List._
object listDemo extends App {

  println("-"*50)
  println(dropWhile(List(1,2,3,4,5),(a:Int)=>a<3))
  println("-"*50)
  println(length(List(1,2,3,4,5)))
  println("-"*50)
  println(flatten(List(List(1,2,3),List(1,2,3),List(1,2,3))))
  println("-"*50)
  println(plusOne(List(1,2,3,4,5)))
  println("-"*50)
  println(map(List(1,2,3,4,5))(a=>a+1))
  println("-"*50)
  println(filter(List(1,2,3,4,5))(a=>a>3))
  println("-"*50)
  println(flatMap(List(1,2,3))(i=>List(i,i)))
}
