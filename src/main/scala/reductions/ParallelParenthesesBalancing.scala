package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val chars = new Array[Char](length)
    val threshold = 500000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    chars.foldLeft(0: Int)((b,a) => if (b<0) b else {
      a match {
        case '(' => b+1
        case ')' => b-1
        case _ => b
      }
    } ) == 0
  }


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    val size = chars.size

    def traverse(idx: Int, until: Int, open: Int, close: Int): (Int, Int) = {
      if (idx < until && idx < size) {
        chars(idx) match {
          case '(' => traverse(idx+1, until, open+1, close)
          case ')' => traverse(idx+1, until, open, close+1)
          case _ => traverse(idx+1, until, open, close)
        }
      } else (open, close)
    }

    def reduce(from: Int, until: Int):(Int, Int) = {
      val size = until - from
      if (size > threshold) {
        val mid = from + size/2
        val (left,right) = parallel(reduce(from, mid), reduce(mid, until))
        (left._1 + right._1, left._2 + right._2)
      } else {
        traverse(from, until, 0, 0)
      }
    }

    val (open, close) = reduce(0, chars.length)
    open == close
  }


  // For those who want more:
  // Prove that your reduction operator is associative!

}
