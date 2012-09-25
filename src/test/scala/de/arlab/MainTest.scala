package de.arlab

import formulas._
import formulas.And
import formulas.Or
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Tests the main object
 */
class MeinTest extends FunSuite with ShouldMatchers {
  test("test some basic things") {
    val list = List(1,2,3,4)
    list should have size (4)
    list.isEmpty should be (false)
    list(3) should be (4)
  }


  test("aufgabe 1"){
    val x = new Variable("x")
    val y = new Variable("y")
    val f = new Function("f",x)
    val R = new Predicate("R",f)
    val Q = new Predicate("Q",y)
    val P = new Predicate("P",x,y,f)

    True && False should be (new And(True, False))
    False || True should be (new Or(False, True))
    -R should be (new Not(R))
    R && -Q || P should be (new Or(new And(R, new Not(Q)),P))
    R && (-Q || P) should be (new And(R,new Or(new Not(Q),P)))
  }
}
