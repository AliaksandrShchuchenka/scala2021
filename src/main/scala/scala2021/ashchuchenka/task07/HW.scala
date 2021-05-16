package scala2021.ashchuchenka.task07

import java.io.{FileInputStream, File}
import scala.language.reflectiveCalls

object HW {

  case class Connection(port: Int) {
    def close(): Unit = println("Closed")

    def run(): Unit = println("Run")
  }

  def withResource[A, B, C](param: A)(f: A => B)(fc: (A) => C = (x: A) => x): B =
    try {
      f(param)
    }
//    catch {
//      case e: Exception => throw (e)
//    }
    finally {
      fc(param)
    }

  def main(args: Array[String]): Unit = {

    withResource(Connection(1)) { c => c.run }(_.close())

    withResource(new FileInputStream("d:\\scala2021\\build.sbt")) {
      fis => {
        val bytes = fis.available()
        println(s"Count bytes are $bytes")
      }
    }(_.close())

    withResource(new File("d:\\scala2021\\build.sbt")) {
      file => file.exists()
    }()
  }
}