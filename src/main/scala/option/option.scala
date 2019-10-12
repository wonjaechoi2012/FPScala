package option

sealed trait Option[+A]{
  //exercise 4.1
  def map[B](f: A=>B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }
  //None이면 종료
  def flatMap[B](f: A=>Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }


  def flatMap1[B](f: A=>Option[B]): Option[B] =
    map(a => f(a)).getOrElse(None)
//option of option에서 값을 꺼내서 option으로 만들기
  def flatMap2[B](f: A=>Option[B]): Option[B] =
    map(f).getOrElse(None)

  def flatMap3[B](f: A=>Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: =>B): B = this match {
    case Some(a) => a
    case None => default
  }

//실패하였을 때 다른 옵션을 돌려줘서, 다른 시도를 할 수 있게하는 용도.
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }

  def orElse1[B >: A](ob: => Option[B]): Option[B] =
    map(a => Some(a)).getOrElse(ob)
  //option of option으로 바꾼 뒤 getOrElse로 값 꺼내오기.


  def orElse2[B >: A](ob: => Option[B]): Option[B] =
    map(Option.apply).getOrElse(ob)

  def orElse2[B >: A](ob: => Option[B]): Option[B] =
    map(Option[A]).getOrElse(ob)

  def orElse3[B >: A](ob: => Option[B]): Option[B] =
    flatMap(a => Option(Option(a))).getOrElse(ob)
  //option of option of option으로 만든 뒤

  def filter(f: A=>Boolean): Option[A] = this match {
    case Some(a) if(f(a)) => this
    case None => None
  }

  def filter1(f: A=>Boolean): Option[A] =
    flatMap(a=>if(f(a)) this else None)

  def lift[A,B](f:A=>B): Option[A] => Option[B] = a=>a.map(f)

  def lift2[A,B](f:A=>B): Option[A] => Option[B] = _ map f
  //일반 함수들을 옵션에 대해 작용하는 함수로 승급시킬 수 있음.
  //옵션을 다룰 때 기존 함수들을 옵션에 대해 작동하도록 새로 작성하지않고,
  //그대로 사용 가능하게 해줌.

  //exercise 4.3
  def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] =
    fa.flatMap(fa=>fb.map(fb=>f(fa,fb)))
  //option a를 Option[A]=>Option[B]=>Option[C]로 만듬.

  def map2_2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] =
    for {
     a<-fa
     b<-fb
    }yield (f(a,b))
  //인수가 두개인 어떤 함수라도 옵션에 대응 시킬 수 있음.

  //승급 함수들을 편하게 만들기 위해 for-comprehention 사용
  def map3[A,B,C,D](fa: Option[A], fb: Option[B], fc: Option[C])(f: (A,B,C)=>D): Option[D] =
    for {
      a<-fa
      b<-fb
      c<-fc
    }yield f(a,b,c)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option{
  def apply[A](a: A): Option[A] = Some(a)

  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum/xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m,2))))
  //V(X) = E((X-E(X))^2), E(X)를 구한 뒤 => E((X-E(X)^2)를 구함. flatMap을 통해 순차적인 연산 가능.
  //순차적인 연산 중간에 None이 발생하면 나머지 모든 과정이 취소!
}