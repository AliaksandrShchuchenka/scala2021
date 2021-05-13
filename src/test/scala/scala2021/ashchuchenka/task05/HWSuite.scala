package scala2021.ashchuchenka.task05

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala2021.ashchuchenka.task05.HW.{Info, findEmployeeManagers, findManagerNameOrError, getManagerNameOrErrorAsync, getManagerNameOrErrorAsyncOperations}

class HWSuite extends AnyFunSuite with TableDrivenPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {
  import HW.findManagerName

  val tblfindManagerName = Table(
    ("input", "expected"),
    ("John", None),
    ("Steve", Some("Steve")),
    ("Mark", Some("Steve")),
    ("Igor", Some("Igor")),
    ("Christy", None),
    ("Naveen", None),
    ("Megan", None)
  )

  test("check findManagerName correctly (table)") {
    forAll(tblfindManagerName) {
      (input, expected) => {
        findManagerName(input) should be(expected)
      }
    }
  }

  val tblfindManagerNameOrError = Table(
    ("input", "expected"),
    ("John", Left("Employee with name John didn't find")),
    ("Steve", Right("Steve")),
    ("Mark", Right("Steve")),
    ("Igor", Right("Igor")),
    ("Christy", Left("Departament for required employee didn't find.")),
    ("Naveen", Left("Employee of Manager of department of required employee didn't find.")),
    ("Megan", Left("Manager of department for required employee didn't find."))
  )

  test("check findManagerNameOrError correctly (table)") {
    forAll(tblfindManagerNameOrError) {
      (input, expected) => {
        findManagerNameOrError(input) should be(expected)
      }
    }
  }

  test("check findManagerNameOrErrorAsync correctly (table)") {
    forAll(tblfindManagerNameOrError) {
      (input, expected) => {
        getManagerNameOrErrorAsync(input) should be(expected)
      }
    }
  }

  test("check findManagerNameOrErrorAsyncOperations correctly (table)") {
    forAll(tblfindManagerNameOrError) {
      (input, expected) => {
        getManagerNameOrErrorAsyncOperations(input) should be(expected)
      }
    }
  }

  val listOfEmployeeManagers = List(
    Info("Steve","Marketing","Steve"),
    Info("Mark","Marketing","Steve"),
    Info("Jane","Marketing","Steve"),
    Info("Samuel","Sales","Igor"),
    Info("Igor","Sales","Igor"),
    Info("Naveen","IT","Not Found"),
    Info("Christy","Not Found","Not Found"),
    Info("Megan","Research","Not Found"))

  test("check findEmployeeManagers correctly (list)") {
    findEmployeeManagers should be(listOfEmployeeManagers)
  }

}
