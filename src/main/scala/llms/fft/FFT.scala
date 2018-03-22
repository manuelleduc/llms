package llms.fft

import llms.arith.Arith
import llms.base.Base
import llms.trig.Trig

import scala.language.postfixOps

trait FFT extends Base {
  this: Arith with Trig =>

  def omega(k: Int, N: Int): Complex = {
    val kth: Double = -2.0 * k * Math.PI / N
    Complex(cos(kth), sin(kth))
  }


  def fft(xs: List[Complex]): List[Complex] = xs match {
    case (x :: Nil) => xs
    case _ =>
      val N = xs.length
      val (even0, odd0) = splitEvenOdd(xs)
      val (even1, odd1) = (fft(even0), fft(odd0))
      val (even2, odd2) = (even1 zip odd1 zipWithIndex) map {
        case ((x, y), k) =>
          val z = omega(k, N) * y
          (x + z, x - z)
      } unzip;
      even2 ::: odd2
  }

  def splitEvenOdd(complexes: List[FFT.this.Complex]): (List[Complex], List[Complex])

  case class Complex(re: Rep[Double], im: Rep[Double]) {
    def +(that: Complex) = Complex(this.re + that.re, this.im + that.im)

    def -(that: Complex) = Complex(this.re - that.re, this.im - that.im)

    def *(that: Complex) = ???
  }

}
