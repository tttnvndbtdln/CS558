//Phuong Pham
//CS 558
//Assignment 4
//Fall 2018
//-------------------------------------------------------------------------

// EL1 Compiler
//
// Usage: linux> scala Comp1 <source file>
//
import EL1._
import Machine1._

object Comp1 {
  var nextLabel: Int = 0
  var start = newLabel()

  def comp(e:Expr): Program = e match {
    case Var(x) => Load(x)::Nil
    case Num(i) => Const(i)::Nil
    case Add(e1,e2) => comp(e1) ::: comp(e2) ::: Plus::Nil
    case Sub(e1,e2) => comp(e1) ::: comp(e2) ::: Const(-1) :: Times :: Plus::Nil
    case Mul(e1,e2) => comp(e1) ::: comp(e2) ::: Times::Nil 
    case Div(e1,e2) => comp(e1) ::: comp(e2) ::: Divrem :: Pop::Nil
    case Rem(e1,e2) => comp(e1) ::: comp(e2) ::: Divrem::Nil
    case Le(e1,e2)  => comp(e1) ::: comp(e2) ::: Lessequ::Nil
    case Assgn(x,e) => comp(e) ::: Store(x) :: Load(x)::Nil
    case While(c,b) => Label(1) :: comp(c) ::: Branchz(2) ::
    comp(b) ::: Pop :: Branch(1) :: Label(2) :: Const(0)::Nil
    case If(c,t,f)  => comp(c) ::: Branchz(1) :: comp(t) ::: Branch(2) :: 
    Label(1) :: comp(f) ::: Label(2)::Nil
    case Write(e)   => comp(e) ::: Print :: comp(e) :::Nil
    case Seq(e1,e2) => comp(e1) ::: Pop :: comp(e2):::Nil
    case Skip()     => Const(0)::Nil
        
    //Warning: For case does not work. Associate tests will fail. 
    case For(x,e1,e2,e3) => comp(e1) ::: Store(x) :: comp(e2) ::: Load(x) :: Label(start) :: Lessequ :: Branchz(2) ::
    comp(e2) ::: comp(e3) ::: Pop :: Load(x) :: Const(1) :: Plus :: Store(x) :: Load(x) :: Lessequ :: Branchz(start) :: 
    Load(x) :: Const(1) :: Plus :: Store(x) :: Branch(1) :: Label(2) :: Const(0) :: Nil
  }

  def newLabel() = {
    val next = nextLabel
    nextLabel = nextLabel + 1
    next
  }

  def compile(e:Expr) = {
    nextLabel = 0
    comp(e)
  }

  def process(s:String, debug:Int = 0): Int = {
    try {
      val e: Expr = parse(s,debug)
      val p: Program = compile(e)
      exec(p,debug)
    } catch {
      case ex: ExecException =>
        { println("Exec Error:" + ex.string) ; throw ex }
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
//
//case For(x,e1,e2,e3) => comp(e1) ::: Store(x) :: newLabel() :: comp(e2) ::: 
    //Load(x) :: Lessequ :: Branchz(2) :: comp(e3) ::: Pop :: Load(x) :: Const(1) :: Plus :: 
    //Store(x) :: Branch(1) :: Label(2) :: Const(0) :: Nil
//case For(x,e1,e2,e3) => comp(e1) ::: Store(x) :: comp(e2) ::: Load(x) :: newLabel() ::: Store(y) :: Load(y) :: 
    //Label(y) :: Store(y) :: Lessequ :: Branchz(2) ::
    //comp(e2) ::: comp(e3) ::: Pop :: Load(x) :: Const(1) :: Plus :: Store(x) :: Load(x) :: Lessequ :: Branchz(1) :: 
    //Load(x) :: Const(1) :: Plus :: Store(x) :: Load(y) :: Branch(y) :: Label(2) :: Const(0) :: Nil

