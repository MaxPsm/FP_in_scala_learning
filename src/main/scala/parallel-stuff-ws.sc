import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Try

def main(args: Array[String]): Unit = {
  implicit val ex = scala.concurrent.ExecutionContext.global
  //    val x = 3
  //    val a = 2 * x
  //    val b = 3 * x

  def asd(fA: Future[String], fB: Future[String]): Future[Int] = {

    ???
    //      Future.successful(Option(a + b).get)

  }

  //    val fA = Future.successful(a * x - a)
  ////    val fB = Future.successful(b * x - b)
  //
  //    for {
  //      a <- fA
  //      b <- fB
  //    } yield a + b

  def fA = Future.successful {
    println(1)
    "1"
  }

  def fB = Future.successful {
    println(2)
    "2"
  }
  //    def fB = Future.successful("1")
  def fAsd = for {
    a <- fA
    aInt <- Future.fromTry(Try(a.toInt))
    b <- fB
    bInt <- Future.fromTry(Try(b.toInt))
  } yield aInt + bInt

  val result = Await.result(fAsd, 5.seconds)
  //    asd(fA, fB).onComplete(println)
  println(result)
}