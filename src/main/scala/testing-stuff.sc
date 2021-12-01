import pure_functional_state.RNG

object testing {
  import Prop._

  case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
    def &&(p: Prop): Prop = Prop {
      (max, n, rng) =>
        run(max, n, rng) match {
          case Passed | Proved => p.run(max, n, rng)
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
    case object Proved extends Result {
      def isFalsified = false
    }

  }
}
