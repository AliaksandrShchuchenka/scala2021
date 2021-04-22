package scala2021.ashchuchenka.task03

object HW {

  def compress(a: List[Symbol]): List[(Int, Symbol)] = a match {
    case Nil => List()
    case x :: l =>
      val (h, t) = l.span(_ == x)
      (h.length + 1, x) :: compress(t)
  }

  def main(args: Array[String]): Unit = {
    val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(compress(l))
  }
}