package llms.gen

import java.io.PrintWriter

import llms.expressions.Expressions

trait ScalaCodegen extends GenericCodegen {

  val IR: Expressions

  import IR._


  def emitSource[A](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {

    val sA =  "Any"//??? // remap(typ[A])

    val staticData = getFreeDataBlock(body)

    withStream(out) {
      stream.println("/*****************************************\n" +
        "  Emitting Generated Code                  \n" +
        "*******************************************/")
      emitFileHeader()

      // TODO: separate concerns, should not hard code "pxX" name scheme for static data here
      stream.println("class " + className + (if (staticData.isEmpty) "" else "(" + staticData.map(p => "p" + quote(p._1) + ":" + p._1).mkString(",") + ")") + " extends ((" + args.map(a => remap(a)).mkString(", ") + ")=>(" + sA + ")) {")
      stream.println("def apply(" + args.map(a => quote(a) + ":" + remap(a)).mkString(", ") + "): " + sA + " = {")

      emitBlock(body)
      stream.println(quote(getBlockResult(body)))

      stream.println("}")

      stream.println("}")
      stream.println("/*****************************************\n" +
        "  End of Generated Code                  \n" +
        "*******************************************/")
    }

    staticData
  }


  def emitValDef(sym: Sym[Any], rhs: String): Unit =
    stream.println("val " + quote(sym) + " = " + rhs)

  def quote(x: Any): String = x match {
    case Const(s: String) => "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\"" // TODO: more escapes?
    case Const(c: Char) => "'" + ("" + c).replace("'", "\\'").replace("\n", "\\n") + "'"
    case Const(f: Float) => "%1.10f".format(f) + "f"
    case Const(l: Long) => l.toString + "L"
    case Const(null) => "null"
    case Const(z) => z.toString
    case Sym(n) => "x" + n
    case _ => throw new RuntimeException("could not quote " + x)
  }

  override def emitFileHeader() {
    // empty by default. override to emit package or import declarations.
  }
}
