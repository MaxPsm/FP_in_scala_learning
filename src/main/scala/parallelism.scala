import java.util.concurrent._
import java.util.concurrent.ExecutorService

object parallelism {
  object Par {
    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
      }

    def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
      es => es.submit(new Callable[A] {
          def call = a(es).get
        }
      )

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sortPar2(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(()))((a, _) => a.sorted)

    def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

    def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
      l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

    // This implementation forks the recursive step off to a new logical thread,
    // making it effectively tail-recursive. However, we are constructing
    // a right-nested parallel program, and we can get better performance by
    // dividing the list in half, and running both halves in parallel.
    // See `sequenceBalanced` below.
    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
      }

    // We define `sequenceBalanced` using `IndexedSeq`, which provides an
    // efficient function for splitting the sequence in half.
    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] =
        l map (asyncF((a: A) => if (f(a)) List(a) else List()))
      map(sequence(pars))(a => a.flatten)
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es =>
        if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
        else f(es)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => {
        val ind = run(es)(n).get
        run(es)(choices(ind))
      }

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
      es => {
        val k = run(es)(key).get
        run(es)(choices(k))
      }

    def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      es => {
       val r = run(es)(pa).get
       run(es)(choices(r))
      }

    def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
      es => {
        val k = run(es)(p).get
        run(es)(choices(k))
      }

    def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      flatMap(p)(b => if (b) t else f)

    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(p)(i => choices(i))

    def join[A](a: Par[Par[A]]): Par[A] =
      es => run(es)(run(es)(a).get)

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(x => x)

    def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      join(map(p)(f))

  }

  def main(args: Array[String]): Unit = {

  }

}

