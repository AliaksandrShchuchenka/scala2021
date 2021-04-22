package scala2021.ashchuchenka.task02

import scala.annotation.tailrec

object HW {

  def calcBrackets(str: String): Boolean = {

    @tailrec
    def calcBracketsHelper(l: List[Char], balance: Int): Boolean = {

      val balanceIncrement = l.head match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }

      val calcBalance = balance + balanceIncrement

      if (calcBalance < 0) false
        else if (l.length == 1) {
        if (calcBalance > 0) false else true
      } else {
        calcBracketsHelper(l.tail, calcBalance)
      }
    }

    calcBracketsHelper(str.toList, 0)
  }

  def main(args: Array[String]): Unit = {

    val str = "if((2+x)*(3-y)==3)"
    println(calcBrackets(str))
  }
}