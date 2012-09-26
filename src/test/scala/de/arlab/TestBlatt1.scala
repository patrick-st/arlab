package de.arlab

import formulas._
import formulas.And
import formulas.Function
import formulas.Not
import formulas.Or
import formulas.Predicate
import formulas.Variable
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class TestBlatt1 extends FunSuite with ShouldMatchers{

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
