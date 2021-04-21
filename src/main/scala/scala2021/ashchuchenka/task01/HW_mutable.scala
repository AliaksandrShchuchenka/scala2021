package scala2021.ashchuchenka.task01

import scala.collection.mutable

object HW_mutable {

  def calcCounts(array: Array[String], outMap: mutable.Map[String, Int]): Array[String] = {
    val res = array.foreach(r => {
      val rd = r.split(',')
      val count = rd(0).toInt
      val domain = rd(1).split('.')

      processDomain(count, domain, outMap)
    })
    array
  }

  def processDomain(qty: Int, array: Array[String], outMap: mutable.Map[String, Int]): Unit = {

    val domainStr = array.mkString(".")
    //println("Domain str = " + domainStr)

    if (outMap.contains(domainStr)) {
      val v = outMap(domainStr)
      outMap(domainStr) = v + qty
      println(v.toString + "+" + qty.toString + "=" + (v + qty).toString)
    }
    else {
      outMap.put(domainStr, qty)
     // println(qty)
    }

    if (array.length > 1) {
      processDomain(qty, array.drop(1), outMap)
    }
  }

  def main(args: Array[String]): Unit = {
    val countsResSet: mutable.Map[String, Int] = mutable.Map()

    val counts = Array(
      "900,google.com",
      "60,mail.yahoo.com",
      "10,mobile.sports.yahoo.com",
      "40,sports.yahoo.com",
      "10,stackoverflow.com",
      "2,en.wikipedia.org",
      "1,es.wikipedia.org",
      "1,mobile.sports"
    )

    calcCounts(counts, countsResSet)
    countsResSet.foreach(r => println(r._2 + " " + r._1))
  }

}