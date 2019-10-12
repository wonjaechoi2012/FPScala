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