object datastructures {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ints: List[Int]): Double = ints match {
      case Nil => 1
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def tail[A](list: List[A]):List[A] = list match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }

    def setHead[A](list: List[A], value: A):List[A] = list match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(value,t)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }

    def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case Cons(h,t) if f(h) => dropWhile(t)(f)
      case _ => as
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

    def init[A](l: List[A]): List[A] =
      l match {
        case Nil => sys.error("init of empty list")
        case Cons(_,Nil) => Nil
        case Cons(h,t) => Cons(h,init(t))
      }

    def init2[A](l: List[A]): List[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A]
      @annotation.tailrec
      def go(cur: List[A]): List[A] = cur match {
        case Nil => sys.error("init of empty list")
        case Cons(_,Nil) => List(buf.toList: _*)
        case Cons(h,t) => buf += h; go(t)
      }
      go(l)
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_, acc) => acc + 1)
    }

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
    }

    def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

    def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

    def length2[A](l: List[A]): Int = {
      foldLeft(l, 0)((acc, _) => acc + 1)
    }

    def reverse[A](l: List[A]): List[A] = {
      foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))
    }

    def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
      foldLeft(reverse(l), z)((b,a) => f(a,b))
    }

    def foldRightViaFoldLeft_1[A,B](as: List[A], outerIdent: B)(combiner: (A,B) => B): B = {
      type BtoB = B => B
      def innerIdent:BtoB = (b:B) => b
      def combinerDelayer:(BtoB, A) => BtoB =
        (delayFunc: BtoB, a: A) => (b:B) => delayFunc(combiner(a, b))
      def go: BtoB = foldLeft(as, innerIdent)(combinerDelayer)
      go(outerIdent)
    }

    def foldRightViaFoldLeft_2[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
      foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
    }

    def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
      foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

    def foldLeftViaFoldRight_1[A,B](as: List[A], outerIdent: B)(combiner: (B, A) => B): B = {
      type BtoB = B => B
      def innerIdent:BtoB = (b:B) => b
      def combinerDelayer:(A, BtoB) => BtoB =
        (a: A, delayFunc: BtoB) => (b:B) => delayFunc(combiner(b, a))
      def go:BtoB = foldRight(as, innerIdent)(combinerDelayer)
      go(outerIdent)
    }

    def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = {
      foldRight(l, r)(Cons(_, _))
    }

    def concat[A](l: List[List[A]]): List[A] = {
      foldRight(l, Nil: List[A])(append)
    }

    def add1(l: List[Int]): List[Int] = {
      foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))
    }

    def doubleToString(l: List[Double]): List[String] = {
      foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))
    }

    def map[A,B](l: List[A])(f: A => B): List[B] =
      foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

    def map_1[A,B](l: List[A])(f: A => B): List[B] =
      foldRightViaFoldLeft(l, Nil:List[B])((h,t) => Cons(f(h),t))

    def map_2[A,B](l: List[A])(f: A => B): List[B] = {
      val buf = new collection.mutable.ListBuffer[B]
      def go(l: List[A]): Unit = l match {
        case Nil => ()
        case Cons(h,t) => buf += f(h); go(t)
      }
      go(l)
      List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
    }

    def filter_1[A](l: List[A])(f: A => Boolean): List[A] =
      foldRightViaFoldLeft(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

    def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
      val buf = new collection.mutable.ListBuffer[A]
      def go(l: List[A]): Unit = l match {
        case Nil => ()
        case Cons(h,t) => if (f(h)) buf += h; go(t)
      }
      go(l)
      List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
      concat(map(as)(f))
    }

    def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] = {
      foldRight(as, Nil: List[B])((x, y) => append(f(x), y))
    }

    def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
      flatMap(l)(a => if (f(a)) List(a) else Nil)

    def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }
  }


  def main(args: Array[String]): Unit = {
    import List._

    val simpleList = List(1, 2, 3)
    val simpleList2 = List(4, 5, 6, 7)

    val l = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    println(l)

    val l2 = foldLeftViaFoldRight_1(List(1,2,3), Nil:List[Int])((b, a) => Cons(a, b))
    println(l2)

    val l3 = foldRightViaFoldLeft_1(List(1,2,3), Nil:List[Int])((a, b) => Cons(a, b))
    println(l3)

    val l4 = append(List(1, 2, 3), List(4, 5, 6))
    println(l4)

    val l5 = concat(List(List(1, 2, 3), List(4, 5, 6)))
    println(l5)

    println(add1(simpleList))
    println(filter(simpleList)(_%2 == 0))
    println(flatMap(List(1,2,3))(i => List(i,i)))
    println(flatMap2(List(1,2,3))(i => List(i,i)))
    println(filterViaFlatMap(simpleList)(_%2 == 0))
    println(addPairwise(simpleList, simpleList2))
  }

}
