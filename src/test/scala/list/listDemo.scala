package list
import list._,List._
object listDemo extends App {

  println("-"*50)
  println(dropWhile(List(1,2,3,4,5),(a:Int)=>a<3))
}
