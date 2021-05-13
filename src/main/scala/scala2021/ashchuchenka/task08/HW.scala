package scala2021.ashchuchenka.task08

object HW {

  def calcScore(strScore: String): Int = {
    val v = strScore
      .split('|')
      .toList

    val n = v.length match {
      case 10 => v :+ "00"
      case _ => v
    }

    val q = n.last.length match {
      case 2 => n
      case 1 => n.take(n.length-1):+(n.last+"0")
    }

    val p =
      (
        (
          q
            .map(n => n.toList.map(_.toString))
            .map(n => n.map(nn => if (nn == "-") "0" else nn))
            .map(n => n.map(nn => if (nn == "X") "10" else nn))
            .map(l => if ((l.length == 2) && l(1) == "/") List(l(0), (-1 * (10 - l(0).toInt)).toString) else l)
            .flatMap(l => l)
            .map(l => l.toInt)
          )
          :+ 0
        )
        .map(l => (l, 0))
        .sliding(2).toList
        .map(l => if (l.head._1 < 0) (-1 * l.head._1, l(1)._1) else l(0))
        .sliding(3).toList
        .map(l => (if(l(0)._1 == 10) (l(0)._1,l(1)._1+l(2)._1) else l(0)))
        .map(l => l._1+l._2)
        .sum
     p
  }

  def main(args: Array[String]): Unit = {
    println(calcScore("X|7/|9-|X|-8|8/|-6|X|X|X||81"))
    println(calcScore("9-|9-|9-|9-|9-|9-|9-|9-|9-|9-||"))
    println(calcScore("5/|5/|5/|5/|5/|5/|5/|5/|5/|5/||5"))
    println(calcScore("X|X|X|X|X|X|X|X|X|X||XX"))
  }
}