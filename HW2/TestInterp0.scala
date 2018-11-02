//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// Testing EL0 interpreter
//
import org.scalatest.FunSuite
import Interp0._

class TestInterp0 extends FunSuite {
  
  test("correctly interpret an expression involving add, mul, div") {
    assertResult(6)(process("(+ (* (/ 3 4) 5) 6)"))
  }
	
  test("correctly interpret an expression involving sub") {
    assertResult(1)(process("(- (- (- 10 3) 3) 3)"))
  }
	
  // test non-zero inputs for mod, 0 in each arg position, negative
  // args in each arg pos
  test("correctly interpret an expression involving rem"){
    assertResult(0)(process("(% 10 2)"))
  } 

  //Added test
  test("correct interpret a complicated expression involving rem") {
    assertResult(5)(process("(% (- 30 10) (+ 8 7))"))
  }
    
  test("correct behavior for div by 0") {
    intercept[InterpException]{process("(/ 10 0)")}
  }

  //Added test
  test("correct behavor for div by 0 in complicated expression") {
    intercept[InterpException]{process("(* 4 (/ 5 (- 6 6)))")}
  }
  
  test("correct behavior for rem by 0"){
    intercept[InterpException]{process("(% 10 0)")}
  }

  //Added test
  test("correct behavior for rem by 0 in complicated expression") {
    intercept[InterpException]{process("(+ 8 (% 3 (- 10 10)))")}
  }
  
  test("correct behavior for rem positive by negative"){
    assertResult(1)(process("(% 10 -3)"))
  } 
  
  test("correct behavior for rem negative by positive") {
    assertResult(-1)(process("(% -10 3)"))
  }
  
  test("correct behavior for rem negative by negative") {
    assertResult(-1)(process("(% -10 -3)"))
  }
	
  test("correctly interpret a complicated arithmetic expression") {
    assertResult(5)(process("(* 1 (+ 2 (- 3 (/ 4 (% 6 7)))))"))
  }

  //Added test
  test("correctly interpret another complicated arithmetic expression") {
    assertResult(1)(process("(% (/ (* (+ 16 5) (- 7 10)) (* -3 3)) (- 4 2))"))
  }
	
}
