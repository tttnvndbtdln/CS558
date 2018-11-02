//Phuong Pham
//CS 558
//Assignment 2
//Fall 2018

//Testing EL0 Compiler

import org.scalatest.FunSuite
import Comp0._

class TestComp0 extends FunSuite
{
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
    
  test("correct behavior for rem positive by negative"){
    assertResult(1)(process("(% 10 -3)"))
  } 
  
  test("correct behavior for rem negative by positive") {
    assertResult(-1)(process("(% -10 3)"))
  }
  
  test("correct behavior for rem negative by negative") {
    assertResult(-1)(process("(% -10 -3)"))
  }

  //Added test
  test("correct behavior for failing div by 0") {
    intercept[Exception](process("(/ 3 0)"))
  }

  //Added test
  test("correct behavior for failling div by 0 in complicated expression") {
    intercept[Exception](process("(/ (+ (% 8 5) (* 2 7)) (- (* 2 10) (/ 80 4)))"))
  }

  //Added Test
  test("correctly interpret a complicated arithmetic expression") {
    assertResult(0)(process("(/ (- (+ 2 6) (% 5 3)) (% (* -3 -2) (/ 21 3)))"))
  }

  //Added test
  test("correctly interpret another complicated arithmetic expression") {
    assertResult(1)(process("(% (/ (* (+ 16 5) (- 7 10)) (* -3 3)) (- 4 2))"))
  }	
}
