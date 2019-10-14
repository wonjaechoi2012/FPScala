package option

sealed trait Option[+A] {
  //exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  //f로 새로운 Option을 전달시킬 수 있음.
  //a가 None이면 f실행 되지않고, Some이면 f가 실행되어 새로운 옵션 생성.
  //시퀀싱 메커니즘을 만들어 냄!!
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def flatMap1[B](f: A => Option[B]): Option[B] =
    map(a => f(a)).getOrElse(None)

  //option of option에서 값을 꺼내서 option으로 만들기
  def flatMap2[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def flatMap3[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
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

  def orElse3[B >: A](ob: => Option[B]): Option[B] =
    map(Option[A]).getOrElse(ob)

  def orElse4[B >: A](ob: => Option[B]): Option[B] =
    flatMap(a => Option(Option(a))).getOrElse(ob)

  //option of option으로 만든 뒤 값을 꺼냄.

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case None => None
  }

  def filter1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) this else None)

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


  def lift[A,B](f:A=>B): Option[A] => Option[B] = a=>a.map(f)

  def lift2[A,B](f:A=>B): Option[A] => Option[B] = _ map f
  //일반 함수들을 옵션에 대해 작용하는 함수로 승급시킬 수 있음.
  //옵션을 다룰 때 기존 함수들을 옵션에 대해 작동하도록 새로 작성하지않고,
  //그대로 사용 가능하게 해줌.

  //exercise 4.3
  def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] =
    fa.flatMap(a=>fb.map(b=>f(a,b)))
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

  def map3_2[A,B,C,D](fa: Option[A], fb: Option[B], fc: Option[C])(f: (A,B,C)=>D): Option[D] =
    fa.flatMap(a =>
      fb.flatMap(b =>
        fc.map(c =>
          f(a,b,c))))

  def Try[A](a: =>A):Option[A] =
    try Some(a)
    catch {case _ : Exception => None}
  //a 평가 도중 오류가 발생할 수 있기때문에 a를 call by name으로 받음.
  //예외 정보는 버려짐. (Either를 사용하면 저장 가능)

  //하나라도 None이 있으면 None, 전부가 Some일 경우, List of Option으로 변경.
  def sequence[A](a: List[Option[A]]):Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h::t => h flatMap (hh => sequence(t) map (hh::_))
  }
  //h::t일 경우 h(Option[A])를 flatMap(a=>Option[List[A])으로 Option[List[A]]로 바꿔줌.
  //a=>Option[List[A]] = hh=>sequence(t) map (hh::_)
  //Option[tail의 리스트] => Option[h::tail의 리스트]로 매핑.
  //None을 만나면 flatMap에서 연산이 종료되면서 None반환하고 종료

  def sequence_1[A](a: List[Option[A]]):Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h::t => map2(h,sequence(t))(_::_)
  }
  //h(Option[A])와 t(Option[List[A]]) 2개의 옵션을 받고, Option[A]안의 a와 Option[List[A]]의 List[A]를 ::로 연결.


  def sequence_2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y)=>map2(x,y)((x,y)=>(x::y)))
  //패턴매칭을 foldRight로 변경


  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a.map(i=>Try(i.toInt)))
  //실패할 수 있는 함수를 리스트의 원소에 적용하였을 때, 하나라도 실패하면 None이 되게해야할때 sequence 사용가능.
  //map으로 전체 리스트를 한번 훑고, sequence로 다시 훑기때문에 반복이 2번!

  //traverse를 만들어서 리스트를 한번만 훑게 변경.
  def traverse[A,B](a: List[A])(f: A=> Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => f(h).flatMap(hh => traverse(t)(f).map(hh::_))
  }

  def traverse_1[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h),traverse(t)(f))((x,y)=>(x::y))
  }

  def traverse_2[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t)=> map2(f(h),t)((_::_)))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
  //traverse(a)(x=>x)
    traverse(a)(identity)
  //Option[A] => Option[A] 그대로.


  //map, lift, sequence, traverse, map2, map3가 있다면 Option을 다룰 때 기존 함수를 수정없이 그대로 사용할 수 있음.
}