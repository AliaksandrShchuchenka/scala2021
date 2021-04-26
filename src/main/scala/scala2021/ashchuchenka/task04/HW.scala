package scala2021.ashchuchenka.task04

import scala.annotation.tailrec

object HW {

  def checkSum(l: List[Int], x: Int): Boolean = {
    if (l.isEmpty && x == 0) true
    else if (l.isEmpty && x != 0) false
    else {

      if (l.contains(x))
        true
      else if (l.min > x)
        false
      else {
        @tailrec
        def calcB(l: List[Int], x: Int, acc: List[Int]): Boolean = {
          val res = acc.flatMap(av => l.map(lv => av + lv))
          if (res.contains(x)) true
          else if (res.min < x) calcB(l, x, res)
          else false
        }

        calcB(l, x, l)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val l = List()
    val x = 0
    println(checkSum(l, x))
  }
}