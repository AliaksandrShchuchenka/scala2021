package scala2021.ashchuchenka.task02

import scala.annotation.tailrec

object HW {

  def calcBrackets(str: String): Boolean = {

    @tailrec
    def calcBracketsHelper(l: List[Char], balance: Int): Boolean = {

      if (balance < 0) false
        else
      l match {
        case Nil => (balance == 0)
        case head::tail => {
          val balanceIncrement = l.head match {
            case '(' => 1
            case ')' => -1
            case _ => 0
          }
          calcBracketsHelper(l.tail, balance + balanceIncrement)
        }
      }
    }

    calcBracketsHelper(str.toList, 0)
  }

  def main(args: Array[String]): Unit = {

    val str = "if((2+x)*(3-y)==3)"
    println(calcBrackets(str))
  }
}