//Phuong Pham
//CS 558
//Assignment 2
//Fall 2018
//-------------------------------------------------------------------------

// EL0 Interpreter
//
// Usage: linux> scala Interp0 <source file>
//
import EL0._

object Interp0 {
  case class InterpException(string: String) extends RuntimeException

  def interp(e: Expr): Int = {
    val v = e match {
      case Num(n) => n
      case Add(l,r) => interp(l) + interp(r)
      case Sub(l,r) => interp(l) - interp(r)
      case Mul(l,r) => interp(l) * interp(r)
      case Div(l,r) =>
      {
        val x = interp(r)
        if (x == 0) 
	  throw InterpException("Divide by 0")
	else 
	  interp(l) / x
      }
      case Rem(l,r) =>
      {
        val x = interp(r)
	if (x == 0)
	  throw InterpException("Divide by 0")
	else
	  interp(l) % x
      }
      case _ => throw ParseException("Cannot perform operation.")
    }
    v
  }

  def process(s:String, debug:Int = 0): Int = {
    try {
      val e: Expr = parse(s,debug)
      try {
        val r = interp(e)
	println("Result:" + r)
       	r
      } catch {
        case ex: InterpException => {
	  println("Interp Error:" + ex.string) ; throw ex
	}
      }
    } catch {
      case ex: ParseException => {
	println("Parser Error:" + ex.string) ; throw ex
      }
    }
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    val s = Source.fromFile(argv(0)).getLines.mkString("\n")
    val d = if (argv.length > 1) argv(1).toInt else 0
    process(s,d)
    ()
  }
}
//
