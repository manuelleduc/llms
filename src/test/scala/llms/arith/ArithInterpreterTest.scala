package llms.arith

import java.io.PrintWriter

import llms.base.BaseExp
import llms.compiler.Compile
import llms.power.{Power, PowerA, PowerB}
import llms.scalacompile.ScalaGenFlat
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class ArithInterpreterTest extends Properties("ArithInterpreter") {

  val powerT = 2

  trait PowerA2 extends Arith {
    this: Power =>
    def f(x: Rep[Double]): Rep[Double] = power(x, powerT)
  }

  val a = new PowerA2 with ArithInterpreter with PowerA
  val b = new PowerA2 with ArithInterpreter with PowerB


  // trait CompileScala extends Compile with BaseExp

  val o = new PowerA2 with ArithExp with PowerA //with CompileScala
  {
    self =>
    val codegen = new ScalaGenFlat with ScalaGenArith {
      val IR: self.type = self
    }
  }

  import o._
  //val aComp = new PowerA2 with PowerA with ArithG

  val xxxxx: Exp[Double] => Exp[Double] = (input: Rep[Double]) => f(input)
  codegen.emitSource(xxxxx, "PowerA2", new PrintWriter(System.out))


  property("Implementations iso-functionals A/default") =
    forAll {
      (x: Int) => {
        val impleA: Double = a.f(x)
        val default = Math.pow(x, powerT)
        impleA == default
      }
    }

  property("Implementations iso-functionals B/default") =
    forAll {
      (x: Int) => {
        val implemB = b.f(x)
        val default = Math.pow(x, powerT)
        implemB == default
      }
    }

  property("Implementations iso-functionals A/B") =
    forAll {
      x: Int => {
        val impleA: Double = a.f(x)
        val implemB = b.f(x)
        impleA == implemB
      }
    }
}
