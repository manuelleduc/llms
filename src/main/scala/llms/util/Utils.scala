package llms.util

// TODO: add logging, etc.
object Utils {

  val verbosity = 1
  val sourceinfo = 0

  def __ = throw new RuntimeException("unsupported embedded dsl operation")

  def printdbg(x: => Any) {
    if (verbosity >= 2) System.err.println(x)
  }

  def printlog(x: => Any) {
    if (verbosity >= 1) System.err.println(x)
  }

  def printerr(x: => Any) {
    System.err.println(x); hadErrors = true
  }

  def printsrc(x: => Any) {
    if (sourceinfo >= 1) System.err.println(x)
  }

  var hadErrors = false
}