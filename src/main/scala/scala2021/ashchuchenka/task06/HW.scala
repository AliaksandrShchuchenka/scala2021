package scala2021.ashchuchenka.task06

import cats.data.{NonEmptyChain, ValidatedNec}
import scala.concurrent.{Await, Future}
import scala.Enumeration
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global

object HW {

  sealed trait DVal {
    def err: String
  }

  case object NameIsInvalid extends DVal {
    override def err: String = "Name is invalid"
  }

  case object AgeIsInvalid extends DVal {
    override def err: String = "Age is invalid"
  }

  case object EmailIsInvalid extends DVal {
    override def err: String = "Email is invalid"
  }

  case object SexHeightIsInvalid extends DVal {
    override def err: String = "Sex/Height is invalid"
  }

  object Sex extends Enumeration {
    type Sex = Value
    val Male, Female = Value
  }

  import Sex._

  type ValidationResult[A] = ValidatedNec[DVal, A]

  def valName(name: String): ValidationResult[String] = if (name.matches("^[A-Za-z]+$")) name.validNec else NameIsInvalid.invalidNec

  def valAge(age: Int): ValidationResult[Int] = if (age > 0 && age < 100) age.validNec else AgeIsInvalid.invalidNec

  def valEmail(email: String): ValidationResult[String] =
    if (email.matches("(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])"))
      email.validNec
    else EmailIsInvalid.invalidNec

  def valSex(sex: Sex, height: Double): ValidationResult[Sex] = if ((sex == Male) && (height > 100)) sex.validNec else SexHeightIsInvalid.invalidNec

  def valHeight(sex: Sex, height: Double): ValidationResult[Double] = if (((sex == Male) && (height > 100)) || (height > 0)) height.validNec else SexHeightIsInvalid.invalidNec

  case class User(name: String, age: Int, email: String, sex: Sex, height: Double)

  def validateUserOneCheck(name: String, age: Int, email: String, sex: Sex, height: Double): String = {
    false match {
      case x if (valName(name).isValid == x) => valName(name).toString
      case x if (valAge(age).isValid == x) => valAge(age).toString
      case x if (valEmail(email).isValid == x) => valEmail(email).toString
      case x if (valSex(sex, height).isValid == x) => valSex(sex, height).toString
      case x if (valHeight(sex, height).isValid == x) => valHeight(sex, height).toString
      case _ => "Valid"

    }
  }

  def validateForm(name: String, age: Int, email: String, sex: Sex, height: Double): ValidationResult[User] =
    (valName(name), valAge(age), valEmail(email), valSex(sex, height), valHeight(sex, height)).mapN(User)

  def validateFormPar(name: String, age: Int, email: String, sex: Sex, height: Double): Future[ValidationResult[User]] = {
    val nameF = Future(valName(name))
    val ageF = Future(valAge(age))
    val emailF = Future(valEmail(email))
    val sexF = Future(valSex(sex,height))
    val heightF = Future(valHeight(sex,height))

    for {
      name <- nameF
      age <- ageF
      email <- emailF
      sex <- sexF
      height <- heightF
    } yield (name, age, email, sex, height).mapN(User)
  }

  def main(args: Array[String]): Unit = {
    println(validateUserOneCheck("user", -9, "abc@google.com", Male, 110))
    println(validateForm("user", 9, "abc@google.com", Male, 110))
  }
}