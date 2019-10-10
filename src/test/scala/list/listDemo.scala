package list
import list._,List._
object listDemo extends App {

  println("-"*50)
  println(dropWhile(List(1,2,3,4,5),(a:Int)=>a<3))
  println(length(List(1,2,3,4,5)))
  println(flatten(List(List(1,2,3),List(1,2,3),List(1,2,3))))
}
