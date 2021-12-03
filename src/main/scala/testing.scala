import pure_functional_state.{RNG, State}
import testing.Prop.{Falsified, MaxSize, Passed, Result, TestCases}



object testing {
  case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
    def &&(p: Prop): Prop = Prop {
      (max, n, rng) =>
        run(max, n, rng) match {
          case Passed => p.run(max, n, rng)
          case x => x
        }
    }

    def ||(p: Prop): Prop = Prop {
      (max, n, rng) =>
        run(max, n, rng) match {
          case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
          case x => x
        }
    }

    def tag(msg: String) = Prop {
      (max,n,rng) => run(max,n,rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
    }

  }

  object Prop {
    type SuccessCount = Int
    type TestCases = Int
    type MaxSize = Int
    type FailedCase = String

    sealed trait Result {
      def isFalsified: Boolean
    }
    case object Passed extends Result {
      def isFalsified = false
    }
    case class Falsified(failure: FailedCase,
                         successes: SuccessCount) extends Result {
      def isFalsified = true
    }

  }

  case class Gen[+A](sample: State[RNG,A]) {
    def map[B](f: A => B): Gen[B] =
      Gen(sample.map(f))

    def boolean: Gen[Boolean] =
      Gen(State(RNG.boolean))

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Int): Gen[List[A]] =
      Gen.listOfN(size, this)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size flatMap (n => this.listOfN(n))

    def unsized: SGen[A] = SGen(_ => this)


    def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
      Gen(sample.map2(g.sample)(f))

    def **[B](g: Gen[B]): Gen[(A,B)] =
      (this map2 g)((_,_))
  }

  object Gen {
    val boolean: Gen[Boolean] =
      Gen(State(RNG.boolean))

    def unit[A](a: => A): Gen[A] =
      Gen(State.unit(a))

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

    def choose2(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(rng => RNG.nonNegativeInt(rng) match {
        case (n, rng2) => (start + n % (stopExclusive - start), rng2)
      }))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(b => if (b) g1 else g2)

    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
      val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

      Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
    }

    def listOf1[A](g: Gen[A]): SGen[List[A]] =
      SGen(n => g.listOfN(n max 1))
  }

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  case class SGen[+A](g: Int => Gen[A]) {
    def apply(n: Int): Gen[A] = g(n)

    def map[B](f: A => B): SGen[B] =
      SGen{g(_) map f }

    def flatMap[B](f: A => SGen[B]): SGen[B] = {
      val g2: Int => Gen[B] = n => {
        g(n) flatMap { f(_).g(n) }
      }
      SGen(g2)
    }

    def **[B](s2: SGen[B]): SGen[(A,B)] =
      SGen(n => apply(n) ** s2(n))

  }


  def main(args: Array[String]): Unit = {


    val ns = List[Int](4, 15, 6)
    val nss = ns.sorted
    val z = nss.zip(nss.tail)

    // We specify that every sorted list is either empty, has one element,
    // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
    val a = nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
      case (a,b) => a > b
    }
  println(z)
  }

}
