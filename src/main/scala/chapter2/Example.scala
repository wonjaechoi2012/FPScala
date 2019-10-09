package chapter2

object Example extends App {
  //example 2.3
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if(n>=ss.length) -1
      else if(ss(n)==key) n
      else loop(n+1)
    loop(0)
  }

  def findFirst(ss: Array[Int], key: Int): Int = {
    def loop(n: Int): Int =
      if(n>=ss.length)
        -1
      else if(ss(n)==key)
        n
      else
        loop(n+1)
    loop(0
    )
  }

  //example 2.4
  def findFirst[A](as: Array[A], p: A=>Boolean):Int = {
    def loop(n: Int): Int =
      if(n>=as.length) -1
      else if(p(as(n))) n
      else loop(n+1)
    loop(0)
  }

  println(findFirst(Array("a","b","c"),(a:String)=>a=="c"))

  //example partial
  def partial1[A,B,C](a:A, f:(A,B)=>C) : B=>C =
    (b:B) => f(a,b)
  //다형적 함수 구현시, 단 하나만의 구현만 가능.
}
