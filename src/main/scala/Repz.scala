import llms.arith.{Arith, ArithInterpreter}
import llms.compiler.{Compile, CompileScala}
import llms.interpreter.Interpreter
import llms.power.PowerA

import scala.language.{higherKinds, implicitConversions, postfixOps}




trait PowerA2 {
  this: Compile with Arith with PowerA =>
  val p4 = compile { x: Rep[Double] =>
    power(x + x, 4)
  }

  println(p4(6))
}

trait PowerA2b {
  this: Compile with Arith with PowerA =>

  trait Tmp extends Arith with PowerA {
    def calc(x: Double) {
      power(x + x, 4)
    }
  }

  trait CompileScalaTmp extends CompileScala with Tmp

  //val p2 = new CompileScalaTmp {} // TODO: comment avoir compilation et interpretation sur le même programme???

  trait WrapCompiler {
    this: Compiler =>
    def compile()
  }

  //println(p4(6))
}

trait PowerA3 {
  this: Interpreter with Arith with PowerA =>
  val p4 = { x: Rep[Double] =>
    power(x + x, 4)
  }

  println(p4(6))
}


object TestPower extends App {
  //new PowerA2 with llms.arith.ScalaGenArith with llms.compiler.CompileScala with llms.arith.ArithExp with PowerA
  new PowerA3 with PowerA with ArithInterpreter
}
