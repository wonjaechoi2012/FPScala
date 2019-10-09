package chapter2

object Quiz {
  //exercise2.1
  def fib(n: Int): Int = {
    def loop(n: Int, prev: Int, next: Int): Int =
      if (n == 0)
        prev
      else
        loop(n - 1, next, prev + next)
    loop(n, 0, 1)
  }

  //exercise2.2
  def isSorted[A](as: Array[A], ordered: (A,A)=>Boolean):Boolean = {
    def loop(n: Int):Boolean=
      if(n>=as.length-1)
        true
      else if(ordered(as(n),as(n+1)))
        false
      else
        loop(n+1)
    loop(0)
  }

  //exercise2.3
  def curry[A,B,C](f:(A,B)=>C):A=>(B=>C) =
    a=>b=>f(a,b)

  //exercise2.4
  def uncurry[A,B,C](f: A=>B=>C):(A,B)=>C =
    (a,b)=>f(a)(b)

  //exercise 2.5
  def compose[A,B,C](f: B=>C, g: A=>B): A=>C =
    a=>f(g(a))
}
