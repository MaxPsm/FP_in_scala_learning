import laziness.Stream._

object laziness {
  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def toListRecursive: List[A] = this match {
      case Cons(h,t) => h() :: t().toListRecursive
      case _ => List()
    }

    def toList: List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h,t) => go(t(), h() :: acc)
        case _ => acc
      }
      go(this, List()).reverse
    }

    def toListFast: List[A] = {
      val buf = new collection.mutable.ListBuffer[A]
      @annotation.tailrec
      def go(s: Stream[A]): List[A] = s match {
        case Cons(h,t) =>
          buf += h()
          go(t())
        case _ => buf.toList
      }
      go(this)
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match{
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
      case _ => empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile_1(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) =>
        if (f(a)) cons(a,b)
        else empty
      )

    def headOption_1: Option[A] =
      foldRight(None: Option[A])((h, _) => Some(h))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h, t) => cons(f(h), t))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) =>
        if (f(h)) cons(h, t)
        else t
      )

    def append[B>:A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((h, t) => f(h) append t)

    def find(p: A => Boolean): Option[A] = filter(p).headOption

    def mapViaUnfold[B](f: A => B): Stream[B] =
      unfold(this){
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }

    def takeViaUnfold(n: Int): Stream[A] =
      unfold((this, n)){
        case (Cons(h, t), 1) => Some(h(), (empty, 0))
        case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
        case _ => None
      }

    def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if f(h()) => Some(h(), t())
        case _ => None
      }

    def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
      unfold((this, s2)){
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      }

    def zip[B](s2: Stream[B]): Stream[(A,B)] =
      zipWith(s2)((_,_))

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
      zipWithAll(s2)((_,_))

    def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      unfold((this, s2)){
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
        case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A], t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
      }

    def startsWith[A](s: Stream[A]): Boolean =
      zipAll(s).takeWhile(oa => !oa._2.isEmpty) forAll {
        case (h,h2) => h == h2
      }

    def tails: Stream[Stream[A]] =
      unfold(this){
        case Empty => None
        case s => Some ((s, s drop 1))
      } append Stream(empty)

    def hasSubsequence[A](s: Stream[A]): Boolean =
      tails exists (_ startsWith s)

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z)))((a, p0) => {
        // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, cons(b2, p1._2))
      })._2

  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = Stream.cons(1, ones)

    def constant[A](a: A): Stream[A] = {
      lazy val tail: Stream[A] = Cons(() => a, () => tail)
      tail
    }

    def from(n: Int): Stream[Int] = {
      cons(n, from(n+1))
    }

    val fibs: Stream[Int] = {
      def go(f0: Int, f1: Int): Stream[Int] =
        cons(f0, go(f1, f0+f1))
      go(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

    val fibsViaUnfold: Stream[Int] =
      unfold(0, 1){ case (f0, f1) => Some((f0, (f1, f1 + f0)))}

    def fromViaUnfold(n: Int) =
      unfold(n)(n => Some((n, n + 1)))

    def constantViaUnfold[A](a: A) =
      unfold(a)(_ => Some((a, a)))

    val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1,1)))


  }


  def main(args:Array[String]): Unit = {
    println(Stream(1,2,3).take(2).toList)
    println(Stream(1,2,3).takeWhile_1(_ % 2 == 1).toList)
    println(ones.take(5).toList)
    println(ones.exists(_ % 2 != 0))
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println(Stream(1, 2, 3).tails.map(x => x.toList).toList)
  }
}
