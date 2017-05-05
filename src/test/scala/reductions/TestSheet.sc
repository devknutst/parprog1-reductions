import java.util.Collections
import java.util.concurrent.ForkJoinPool

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


val f: Future[List[String]] = Future {
  List("a", "b", "c")
}


f onComplete {
  case Success(l) => print("aaa")
  case Failure(e) => println(e)
}

val a = f onSuccess {
  case l => l.head
}

val b = f onFailure {
  case x => x
}





