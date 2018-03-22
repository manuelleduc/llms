package llms.arith

import llms.interpreter.Interpreter

trait ArithInterpreter extends Arith with Interpreter {
  override def num_mult(value: Double, value1: Double): Double = value * value1

  override def num_add(value: Double, value1: Double): Double = value + value1

  override def num_sub(value: Double, value1: Double): Double = value - value1
}
