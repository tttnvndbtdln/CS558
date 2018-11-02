//Phuong Pham
//CS 558
//Assignment 3
//Fall 2018

//-------------------------------------------------------------------------

// EL1 Interpreter
//
// Usage: linux> scala Interp1 <source file>
//
import EL1._

object Interp1 {
  case class InterpException(string: String) extends RuntimeException

  type Store = collection.mutable.Map[String,Int]
  
  def interp(e:Expr, debug:Int = 0): Int = {
    
    val st: Store = collection.mutable.Map[String,Int]()

    def interpE(e:Expr): Int = {
      if (debug > 1) {
        println("  expr = " + e);
        println("  store = " + st)
      }
      e match {
        case Num(n) => n
        case Var(x) => 
          st get x match {
            case Some(v) => v
            case None    => 0
          }
        case Add(l,r) => interpE(l) + interpE(r)
	case Sub(l,r) => interpE(l) - interpE(r)
	case Mul(l,r) => interpE(l) * interpE(r)
	case Div(l,r) =>
	{
	  val x = interpE(r)
	  if (x == 0)
	    throw InterpException("Divide by 0")
	  interpE(l) / x
	}
	case Rem(l,r) =>
	{
	  val x = interpE(r)
	  if (x == 0)
	    throw InterpException("Divide by 0")
	  interpE(l) % x
	}
	case Le(l,r) =>
	{
	  val x = interpE(l)
	  val y = interpE(r)
	  if (x <= y)
	    1
	  else
	    0
	}
	case Assgn(x,e) =>
	{
	  st(x) = interpE(e)
	  st(x)
	}

        case While(c,b) =>
        {
	  var x = 0
	  var y = 0
	  do {
	  x = interpE(c)
	  if (x != 0)
	    y = interpE(b)
	  } while (x != 0)
	  0
	}
        case If(c,t,e) => 
        {
	  val x = interpE(c)
	  if (x != 0)
	  {
	    val y = interpE(t)
	    y
	  }
	  else
	  {
	    val z = interpE(e)
	    z
	  }
	}
        case Write(e) => {
          val v = interpE(e)
      	  println(v);
          v
        }
        case Seq(e1,e2) => {
          val v1 = interpE(e1)
          val v2 = interpE(e2)
          v2
        }
        case Skip() => 0
        case For(x,e1,e2,e3) => 
        {
	  val v1 = interpE(e1)
	  st(x) = v1
	  var v2 = interpE(e2)
	  var vx = st(x)
	  while (vx <= v2)
	  {
	    v2 = interpE(e2)
	    val v3 = interpE(e3)
	    vx = st(x) + 1
	    if (!(vx > v2))
	    {
	      val v4 = st(x) + 1
	      st(x) = v4
	    }
	  }
	  0
	}
      }
    }

    val v = interpE(e)
    if (debug > 0) println("Evaluates to: " + v)
    v
  } 
  
  def process(s:String, debug:Int = 0): Int = {
    try {
      val e: Expr = parse(s,debug)
      interp(e,debug)
    } catch {
      case ex: InterpException => 
        { println("Interp Error:" + ex.string) ; throw ex }
      case ex: ParseException => 
        { println("Parser Error:" + ex.string) ; throw ex }
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

