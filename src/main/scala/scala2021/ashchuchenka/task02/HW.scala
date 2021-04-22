package scala2021.ashchuchenka.task02

import scala.annotation.tailrec

object HW {

  def calcBrackets(str: String): Boolean = {

    @tailrec
    def calcBracketsHelper(l: List[Char], b: Int): Boolean = {

      l match {
        case _ if (b < 0) => false
        case Nil => (b == 0)
        case head::tail => {
            val i = head match {
              case '(' => 1
              case ')' => -1
              case _ => 0
            }
            calcBracketsHelper(tail, b + i)
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