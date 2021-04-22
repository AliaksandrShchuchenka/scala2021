package scala2021.ashchuchenka.task02

import scala.annotation.tailrec

object HW {

  def calcBrackets(str: String): Boolean = {

    @tailrec
    def calcBracketsHelper(l: List[Char], b: Int): Boolean =

      l match {
        case _ if b < 0 => false
        case Nil => b == 0
        case head::tail => {
          head match {
            case '(' => calcBracketsHelper(tail, b + 1)
            case ')' => calcBracketsHelper(tail, b - 1)
            case _ => calcBracketsHelper(tail, b)
          }
        }
      }


    calcBracketsHelper(str.toList, 0)
  }

  def main(args: Array[String]): Unit = {
    val str = ":-)"
    println(calcBrackets(str))
  }
}