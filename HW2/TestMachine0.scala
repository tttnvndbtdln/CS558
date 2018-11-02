//Phuong Pham
//CS 558
//Assignment 2
//Fall 2018

//Testing Machine0

import org.scalatest.FunSuite
import Machine0._

class TestMachine0 extends FunSuite
{
  test("Step A: 1 + (3 - 2)")
  {
    var p = List[Instr](Const(3), Const(-2), Plus, Const(1), Plus)
    assert(exec(p, 0) == 2) 
  }

  test("Step B: (2 * -3) - (5 / 3)")
  {
    var p = List[Instr](Const(2), Const(-3), Times, Const(5), Const(3), Divrem, Pop, Const(-1), Times, Plus)
    assert(exec(p, 0) == -7)
  }

  test("Step C: (-2 / 3) * 3 + (-2 % 3)")
  {
    var p = List[Instr](Const(-2), Const(3), Divrem, Pop, Const(3), Times, Const(-2), Const(3), Divrem, Swap, Pop, Plus)
    assert(exec(p, 0) == -2)
  }
}
