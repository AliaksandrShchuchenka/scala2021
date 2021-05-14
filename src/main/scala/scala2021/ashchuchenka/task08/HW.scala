package scala2021.ashchuchenka.task08

import scala.collection.immutable.ArraySeq

object HW {
  def stabilizeScoreStrLength(strScore: String): Array[String] = {
    val scoreArr = strScore
      .split('|')

    val EmptyBonusChecked = scoreArr.length match {
      case 10 => scoreArr :+ "00"
      case _ => scoreArr
    }

    EmptyBonusChecked.last.length match {
      case 2 => EmptyBonusChecked
      case 1 => EmptyBonusChecked.take(EmptyBonusChecked.length-1):+(EmptyBonusChecked.last+"0")
    }
  }

  def calcScore(strScore: String): Int = {

    val p =
      (
        (
          stabilizeScoreStrLength(strScore)
            .map(n => n.map(_.toString))
            .map(n => n.map(nn => if (nn == "-") "0" else nn)) //Replace "-" to 0 scores
            .map(n => n.map(nn => if (nn == "X") "10" else nn)) //Replace "X" to 10 scores
            //Replace "/" to scores with "-" sign
            .map(l => if ((l.length == 2) && l(1) == "/") ArraySeq(l(0), (-1 * (10 - l(0).toInt)).toString) else l)
            .flatMap(l => l)
            .map(l => l.toInt)
        )
          :+ 0 //Add extra element to Array (for avoiding lost elements after sliding
      )
        .map(l => (l, 0))
        .sliding(2)
        //replace all nubmers with "-" sign to scores (continue processing of '/' elements)
        .map(l => if (l.head._1 < 0) (-1 * l.head._1, l(1)._1) else l.head)
        .sliding(3)
        //continue processing of 'X' elements
        .map(l => (if(l.head._1 == 10) (l.head._1,l(1)._1+l(2)._1) else l.head))
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