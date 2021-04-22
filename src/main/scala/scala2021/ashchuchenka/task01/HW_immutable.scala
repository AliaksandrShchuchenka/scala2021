package scala2021.ashchuchenka.task01

object HW_immutable {

  def processDomain(qty: Int, array: Array[String]): Array[(Int, String)] = {

    val domainStr = array.mkString(".")
    val result: Array[(Int, String)] = Array((qty,domainStr))

    if (array.length > 1) {
      result ++ processDomain(qty, array.drop(1))
    }
    else result
  }

  def main(args: Array[String]): Unit = {

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

    val ds = counts.map(r => r.split(',')).flatMap(a => processDomain(a(0).toInt,a(1).split('.')))
    val res = ds.groupBy(_._2).map(r => (r._1, r._2.map(a => a._1).toList.sum))
    res.foreach(r => println(r._2+" "+r._1))
  }
}