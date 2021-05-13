package scala2021.ashchuchenka.task05

import scala2021.ashchuchenka.task05.Constants.NotFoundMessage
import cats.data.{EitherT, ValidatedNec}
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationDouble, DurationInt}
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object HW {

  case class Employee(id: Int, name: String, departmentId: Int)

  case class Department(id: Int, name: String)

  case class Manager(department: String, employeeId: Int)

  case class Info(employee: String, department: String, manager: String)

  private val employees = List(
    Employee(1, "Steve", 1),
    Employee(3, "Mark", 1),
    Employee(4, "Jane", 1),
    Employee(7, "Samuel", 2),
    Employee(10, "Igor", 2),
    Employee(11, "Naveen", 4),
    Employee(12, "Christy", 5),
    Employee(15, "Megan", 3)
  )
  private val departments = List(
    Department(1, "Marketing"),
    Department(2, "Sales"),
    Department(3, "Research"),
    Department(4, "IT"),
  )
  private val managers = List(
    Manager("Marketing", 1),
    Manager("Sales", 10),
    Manager("IT", 14),
  )

  def findManagerName(employee: String): Option[String] = {

    val empMan = employees.filter(_.name == employee)
      .flatMap(e => departments.filter(_.id == e.departmentId))
      .flatMap(d => managers.filter(_.department == d.name))
      .flatMap(m => employees.filter(_.id == m.employeeId))

    if (empMan.isEmpty) None else Some(empMan.head.name)
  }

  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так
  def findManagerNameOrError(employee: String): Either[String, String] = {

    val emp = employees.filter(_.name == employee)

    (if (!(emp.isEmpty))
      Right(emp.flatMap(e => departments.filter(_.id == e.departmentId)))
    else Left("Employee with name " + employee + " didn't find")) match {
      case Right(dep) =>
        (if (!(dep.isEmpty))
          Right(dep.flatMap(d => managers.filter(_.department == d.name)))
        else Left("Departament for required employee didn't find.")) match {
          case Right(mng) =>
            (if (!(mng.isEmpty))
              Right(mng.flatMap(m => employees.filter(_.id == m.employeeId)))
            else Left("Manager of department for required employee didn't find.")) match {
              case Right(empMan) =>
                if (!(empMan.isEmpty))
                  Right(empMan.head.name)
                else Left("Employee of Manager of department of required employee didn't find.")
              case Left(i) => Left(i)
            }
          case Left(i) => Left(i)
        }
      case Left(i) => Left(i)
    }
  }

  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так и сделать все это асинхронно
  def findManagerNameOrErrorAsync(employee: String): Future[Either[String, String]] = {

    Future {
      val emp = employees.filter(_.name == employee)

      (if (!(emp.isEmpty))
        Right(emp.flatMap(e => departments.filter(_.id == e.departmentId)))
      else Left("Employee with name " + employee + " didn't find")) match {
        case Right(dep) =>
          (if (!(dep.isEmpty))
            Right(dep.flatMap(d => managers.filter(_.department == d.name)))
          else Left("Departament for required employee didn't find.")) match {
            case Right(mng) =>
              (if (!(mng.isEmpty))
                Right(mng.flatMap(m => employees.filter(_.id == m.employeeId)))
              else Left("Manager of department for required employee didn't find.")) match {
                case Right(empMan) =>
                  if (!(empMan.isEmpty))
                    Right(empMan.head.name)
                  else Left("Employee of Manager of department of required employee didn't find.")
                case Left(i) => Left(i)
              }
            case Left(i) => Left(i)
          }
        case Left(i) => Left(i)
      }
    }
  }

  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так и сделать каждую операцию асинхронной(операция = вызов репозитория)
  def findManagerNameOrErrorAsyncOperations(employee: String): EitherT[Future, String, String] = {

    for {
      empl <- EitherT.fromOption[Future](employees.find(_.name == employee), s"Employee with name $employee didn't find")
      dep <- EitherT.fromOption[Future](departments.find(_.id == empl.departmentId), s"Departament for required employee didn't find.")
      mng <- EitherT.fromOption[Future](managers.find(_.department == dep.name), s"Manager of department for required employee didn't find.")
      empMng <- EitherT.fromOption[Future](employees.find(_.id == mng.employeeId), s"Employee of Manager of department of required employee didn't find.")
    }
    yield empMng.name

  }

  // вывести список всех сотрудников, вместе с именем департамента и именем менеджера, если департамента или менеджера нет то использовать константу "Not Found"
  def findEmployeeManagers: List[Info] = {

    {
      for {
        empl <- employees
      }
      yield {
        val r = {
          for {
            dep <- EitherT.fromOption[Future](departments.find(_.id == empl.departmentId), Info(empl.name, NotFoundMessage, NotFoundMessage))
            mng <- EitherT.fromOption[Future](managers.find(_.department == dep.name), Info(empl.name, dep.name, NotFoundMessage))
            empMng <- EitherT.fromOption[Future](employees.find(_.id == mng.employeeId), Info(empl.name, dep.name, NotFoundMessage))
          }
          yield Info(empl.name, dep.name, empMng.name)
        }
        Await.result(r.value, 100.millis)
      }
    }.map(r => r match {
      case Right(value) => value
      case Left(value) => value
    })
  }

  def printManagerName(employee: String): Unit = {
    val res = findManagerName(employee)
    val out = if (res.isDefined) res.get else NotFoundMessage
    println(s"$employee $out")
  }

  def printManagerNameOrError(employee: String): Unit = {
    findManagerNameOrError(employee) match {
      case Right(m) => println("Found manager with name: " + m)
      case Left(e) => println("Manager didn't find by the following reason: " + e)
    }
  }

  def printManagerNameOrErrorAsync(employee: String): Unit = {
    println(
      findManagerNameOrErrorAsync(employee).map(s => s match {
        case Right(m) => ("Found manager with name: " + m)
        case Left(e) => ("Manager didn't find by the following reason: " + e)
      }
      )
    )
  }

  def getManagerNameOrErrorAsync(employee: String): Either[String,String] ={
    Await.result(findManagerNameOrErrorAsync(employee), 1.seconds)
  }

  def getManagerNameOrErrorAsyncOperations(employee: String): Either[String,String] ={
    Await.result(findManagerNameOrErrorAsyncOperations(employee).value, 1.seconds)
  }

  def main(args: Array[String]): Unit = {

//    printManagerName("John")

//        printManagerName("Steve")
//        printManagerName("Mark")
//        printManagerName("Igor")
//        printManagerName("Christy")
//        printManagerName("Naveen")
//        printManagerName("Megan")
    //
//        printManagerNameOrError("John")
//        printManagerNameOrError("Steve")
//        printManagerNameOrError("Mark")
//        printManagerNameOrError("Igor")
//        printManagerNameOrError("Christy")
//        printManagerNameOrError("Naveen")
//        printManagerNameOrError("Megan")

//    println(findManagerNameOrError("John"))
//    println(findManagerNameOrError("Steve"))
//    println(findManagerNameOrError("Mark"))
//    println(findManagerNameOrError("Igor"))
//    println(findManagerNameOrError("Christy"))
//    println(findManagerNameOrError("Naveen"))
//    println(findManagerNameOrError("Megan"))

//    println(getManagerNameOrErrorAsync("John"))
//    println(getManagerNameOrErrorAsync("Steve"))
//    println(getManagerNameOrErrorAsync("Mark"))
//    println(getManagerNameOrErrorAsync("Igor"))
//    println(getManagerNameOrErrorAsync("Christy"))
//    println(getManagerNameOrErrorAsync("Naveen"))
//    println(getManagerNameOrErrorAsync("Megan"))



//        val result = Await.result(findManagerNameOrErrorAsync("Mark"), 1.seconds)
//        println(s"result = $result")

    //    val result = Await.result(findManagerNameOrErrorAsyncOperations("Mark1"), 1.seconds)
    //    println(s"result = $result")

    //    val result = Await.result(findManagerNameOrErrorAsyncOperations("Christy").value, 1.seconds)
    //    println(s"result = $result")
    //    findEmployeeManagers.foreach(println(_))

//    println(findEmployeeManagers)

  }
}