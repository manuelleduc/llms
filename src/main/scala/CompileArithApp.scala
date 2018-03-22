import java.io.PrintWriter

import llms.arith.{Arith, ArithExp, ScalaGenArith}
import llms.power.{Power, PowerA}
import llms.scalacompile.ScalaGenFlat

object CompileArithApp extends App {
  val powerT = 3

  trait PowerA2 extends Arith {
    this: Power =>
    def f(x: Rep[Double]): Rep[Double] = power(x, powerT)
  }

  val o = new PowerA2 with ArithExp with PowerA {
    self =>
    val codegen = new ScalaGenFlat with ScalaGenArith {
      val IR: self.type = self
    }
  }

  import o._
  //val aComp = new PowerA2 with PowerA with ArithG

  val prog: Exp[Double] => Exp[Double] = (input: Rep[Double]) => f(input)
  codegen.emitSource(prog, "PowerA2", new PrintWriter(System.out))
}
