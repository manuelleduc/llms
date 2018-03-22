package llms.arith

import llms.base.Base

import scala.language.implicitConversions

trait Arith extends Base {


  def num_mult(value: Rep[Double], value1: Rep[Double]): Rep[Double]

  def num_add(value: Rep[Double], value1: Rep[Double]): Rep[Double]

  def num_sub(value: Rep[Double], value1: Rep[Double]): Rep[Double]

  implicit def numbericToNumericMult(x: Double): NumericOpsCls = new NumericOpsCls(unit(x))

  implicit def numbericToNumericRepMult(x: Rep[Double]): NumericOpsCls = new NumericOpsCls(x)

  class NumericOpsCls(lhs: Rep[Double]) {
    def *(rhs: Rep[Double]): Rep[Double] = num_mult(lhs, rhs)

    def +(rhs: Rep[Double]): Rep[Double] = num_add(lhs, rhs)

    def -(rhs: Rep[Double]): Rep[Double] = num_sub(lhs, rhs)
  }

}
