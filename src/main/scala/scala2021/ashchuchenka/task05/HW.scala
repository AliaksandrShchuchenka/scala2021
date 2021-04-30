package scala2021.ashchuchenka.task05

import scala2021.ashchuchenka.task05.Constants.NotFoundMessage

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

    val emp = employees.find(_.name == employee)

    val dep =
      if (emp.isDefined)
        departments.find(_.id == emp.get.departmentId)
      else None

    val man = if (dep.isDefined)
      managers.find(_.department == dep.get.name)
    else None

    val empMan = if (man.isDefined)
      employees.find(_.id == man.get.employeeId)
    else None

    if (empMan.isDefined)
      Some(empMan.get.name)
    else None
  }

  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так
  def findManagerNameOrError(employee: String): Either[String, String] = {

    val emp = employees.find(_.name == employee)

    (if (emp.isDefined)
      Right(departments.find(_.id == emp.get.departmentId))
    else Left("Employee with name " + employee + " didn't find")) match {
      case Right(dep) =>
        (if (dep.isDefined)
          Right(managers.find(_.department == dep.get.name))
        else Left("Departament for required employee didn't find.")) match {
          case Right(mng) =>
            (if (mng.isDefined)
              Right(employees.find(_.id == mng.get.employeeId))
            else Left("Manager of department for required employee didn't find.")) match {
              case Right(empMng) =>
                if (empMng.isDefined)
                  Right(empMng.get.name)
                else Left("Employee of Manager of department of required employee didn't find.")
              case Left(i) => Left(i)
            }
          case Left(i) => Left(i)
        }
      case Left(i) => Left(i)
    }
  }

  //  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так и сделать все это асинхронно
  //  def findManagerNameOrErrorAsync(employee: String): Future[Either[String, String]] = {
  //
  //  }

  //  // вывести список всех сотрудников, вместе с именем департамента и именем менеджера, если департамента или менеджера нет то использовать константу "Not Found"
  //  def findEmployeeManagers: List[Info] = ???

  def printManagerName(employee: String): Unit = {
    val res = findManagerName(employee)
    val out = if (res.isDefined) res.get else NotFoundMessage
    println(out)
  }

  def printManagerNameOrError(employee: String): Unit = {
    findManagerNameOrError(employee) match {
      case Right(m) => println("Found manager with name: "+m)
      case Left(e) => println("Manager didn't find by the following reason: "+e)
    }
  }

  def main(args: Array[String]): Unit = {

    printManagerName("John")
    printManagerName("Steve")
    printManagerName("Mark")
    printManagerName("Igor")
    printManagerName("Christy")
    printManagerName("Naveen")
    printManagerName("Megan")

    printManagerNameOrError("John")
    printManagerNameOrError("Steve")
    printManagerNameOrError("Mark")
    printManagerNameOrError("Igor")
    printManagerNameOrError("Christy")
    printManagerNameOrError("Naveen")
    printManagerNameOrError("Megan")
  }
}