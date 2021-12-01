import pure_functional_state.{RNG, State}

object testing {
  trait Prop {
    def check: Boolean
    def &&(p: Prop): Prop = new Prop {
      def check: Boolean = Prop.this.check && p.check
    }
  }

  case class Gen[A](sample: State[RNG,A]) {
    def boolean: Gen[Boolean] =
      Gen(State(RNG.boolean))

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Int): Gen[List[A]] =
      Gen.listOfN(size, this)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size flatMap (n => this.listOfN(n))
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
  }
}
