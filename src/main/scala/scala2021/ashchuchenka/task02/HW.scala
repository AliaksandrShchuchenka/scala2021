package scala2021.ashchuchenka.task02

import scala.annotation.tailrec

object HW {

  def calcBrackets(str: String): Boolean = {

    @tailrec
    def calcB(l: List[Char], b: Int): Boolean =
      l match {
        case _ if b < 0 => false
        case Nil => b == 0
        case head :: tail =>
          head match {
            case '(' => calcB(tail, b + 1)
            case ')' => calcB(tail, b - 1)
            case _ => calcB(tail, b)
          }
      }

    calcB(str.toList, 0)
  }

  def main(args: Array[String]): Unit = {
    val str = ":-)"
    println(calcBrackets(str))
  }
}