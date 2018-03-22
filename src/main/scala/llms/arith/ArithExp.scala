package llms.arith

import llms.base.BaseExp

import scala.language.implicitConversions

trait ArithExp extends Arith with BaseExp {

  def num_mult(value: Exp[Double], value1: Exp[Double]): Exp[Double] = Times(value, value1)

  def num_add(value: Exp[Double], value1: Exp[Double]): Exp[Double] = Plus(value, value1)

  def num_sub(value: Exp[Double], value1: Exp[Double]): Exp[Double] = Sub(value, value1)

  implicit override def unit(x: Double): Exp[Double] = Const(x)

  case class Plus(x: Exp[Double], y: Exp[Double]) extends Def[Double]

  case class Sub(x: Exp[Double], y: Exp[Double]) extends Def[Double]

  case class Times(x: Exp[Double], y: Exp[Double]) extends Def[Double]

  class NumericOpsCls(lhs: Exp[Double]) {
    def *(rhs: Exp[Double]): Exp[Double] = num_mult(lhs, rhs)

    def +(rhs: Exp[Double]): Exp[Double] = num_add(lhs, rhs)

    def -(rhs: Exp[Double]): Exp[Double] = num_sub(lhs, rhs)
  }


  implicit def doubleToExp(x: Double): NumericOpsCls = new NumericOpsCls(unit(x))

  implicit def doubleToExp(x: Exp[Double]): NumericOpsCls = new NumericOpsCls(x)


}
