package list

sealed trait List[+A] //sealed => list의 모든 구현이 이 파일 안에 선언 되야함.
//리스트의 두가지 구현
final case object Nil extends List[Nothing]
final case class Cons[+A](head: A, tail: List[A]) extends List[A]
//+는 convariance를 뜻함.
//covariance는 A가 B의 서브타입이면 List[A]도 List[B]의 서브타입임을 의미함.
//Nil이 List[Nothing]을 상속받고, Nothing은 모든 타임의 서브타입이기때문에
//List[Nothing](Nil)은 모든 타입의 서브타입. 즉, Nil을 모든 타입의 리스트로 간주할 수 있음.

object List{
  def apply[A](as: A*):List[A] =
    if(as.isEmpty)
      Nil
    else
      Cons(as.head,apply(as.tail:_*))
  //생성자 역할 List(1,2,3,4,5), List.apply(1,2,3,4,5)
  //*A <- var_args, ...A
  //스칼라는 베리어블 아규먼트 시퀀스로 다룸, 자바는 어레이
  //Seq뒤에 :_* 하면 시퀀스를 풀어줌

  //3.2 pattern match
  def sum(ints: List[Int]):Int = ints match {
    case Nil => 0
    case Cons(h,t) => h+sum(t)
  }

  def product(ds: List[Double]):Double = ds match {
    case Nil => 0.0
    case Cons(h,t) => h*product(t)
  }

  //exercise 3.2
  def tail[A](as: List[A]):List[A] = as match {
    case Cons(_,t) => t
    case Nil => throw new UnsupportedOperationException
  }

  //exercise 3.3
  def setHead[A](h:A, as:List[A]):List[A] = as match {
    case Cons(_,t) => Cons(h,t)
    case Nil => throw new UnsupportedOperationException
  }

  //exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_,t) if n>0 => drop(t,n-1)
    case _ => l
  }

  //exercise 3.5
  def dropWhile[A](l: List[A], f: A=>Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t,f)
    case _ => l
  }

  //다음 인수 추론 가능하게 변경
  def dropWhile2[A](l: List[A])(f: A=>Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t,f)
    case _ => l
  }

  //append a1의 길이에만 의존 operation이 앞쪽에서만 일어나는게 좋음.
  def append[A](a1:List[A], a2:List[A]):List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h,append(t,a2))
  }

  //exercise 3.6
  def init[A](l:List[A]):List[A] = l match {
    case Nil => throw new UnsupportedOperationException
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  //sum과 product의 차이는 Nil일 때의 반환 값과 결과를 결합하는데 쓰이는 연산.
  //을 제외한 부분은 코드 중복!
  //이것을 일반화

  def foldRight[A,B](as:List[A], z:B)(f: (A,B)=>B):B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x,foldRight(xs,z)(f))
    }

  def sum2(ns: List[Int]):Int =
    foldRight(ns,0)((x,y)=>x+y)

  def product2(ns: List[Double]): Double =
    foldRight(ns,0.0)(_*_)

  //foldRight는 꼬리재귀가 아니기때문에 스택오버플로우발생할 수 있음.

  //exercise 3.8
  //원래 리스트가 나옴.
  //foldRight(List(1,2,3),Nil:List[Int])(Cons(_,_))

  //exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as,0)((_,acc)=>acc+1)

  //exercise 3.10
  //꼬리재귀로 만들어서 스택오버플로우에 안전한 foldLeft작성
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A)=>B):B =
    as match {
      case Nil => z
      case Cons(h,t) => foldLeft(t,f(z,h))(f)
    }
  //퍼즐 맞추기처럼 반환타입에 맞게 끼워넣으면 구현 완성.

  //exercise 3.11
  def sum3(ns: List[Int]): Int =
    foldLeft(ns,0)(_+_)

  def product3(ns: List[Double]): Double =
    foldLeft(ns,0.0)(_*_)

  //exercise 3.12
  def reverse[A](ns: List[A]): List[A] =
    foldLeft(ns,Nil:List[A]){(acc,a)=>Cons(a,acc)}

  //exercise 3.13


  //exercise 3.14
  def append2[A](a1: List[A],a2: List[A]): List[A] =
    foldRight(a1,a2)(Cons(_,_))

  //exercise 3.15
  def flatten[A](ns: List[List[A]]): List[A] =
    foldRight(ns,Nil:List[A])(append(_,_))

}

