package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def cum_balance(chars: Array[Char], acc: Int): Boolean = {
      if (chars.isEmpty) acc == 0
      else {
        val (first, rest) = (chars.head, chars.tail)
        if (first == '(') cum_balance(rest, acc-1)
        else if (first == ')' && acc < 0) cum_balance(rest, acc+1)
        else false
      }
    }
    val parenthesis = chars.filter("()".contains(_))
    cum_balance(parenthesis, acc=0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, imb_open: Int, imb_close: Int): (Int, Int) = {
      if (idx == until) (imb_open, imb_close)
      else  chars(idx) match {
        case '(' => traverse(idx+1, until, imb_open+1, imb_close)
        case ')' =>
          if (imb_open > 0) traverse(idx+1, until, imb_open-1, imb_close)
          else traverse(idx+1, until, imb_open, imb_close+1)
        case _ => traverse(idx+1, until, imb_open, imb_close)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from < threshold) traverse(from, until, 0, 0)
      else {
        val m = (from + until) / 2
        val ((imb_open1, imb_close1), (imb_open2, imb_close2)) = parallel(
          traverse(from, m, 0, 0), traverse(m, until, 0, 0)
        )
        (imb_open1 - imb_open2, imb_close1 - imb_close2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
