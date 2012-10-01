package de.arlab

import formulas._
import formulas.And
import formulas.Exists
import formulas.ForAll
import formulas.Function
import formulas.Not
import formulas.Or
import formulas.Predicate
import formulas.Variable
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import formulas.transformations.Transformations

class TestBlatt2_1 extends FunSuite with ShouldMatchers {


  test("aufgabe 2.1"){
    val x = new Variable("x")
    val y = new Variable("y")
    val z = new Variable("z")
    val f = new Function("f",x,z)
    val R = new Predicate("R",f)
    val Q = new Predicate("Q",y)
    val P = new Predicate("P",x,y,f)
    val formel1 =  R && -Q || P
    val formel2 = R && P
    val formel3 = -formel2
    val formel4 = True || False
    val formel5 = Exists(y,ForAll(x,formel2))
    val formel6 = R && Q && P

    x.toString should be ("x")
    formel1.toString should be ("((R(f(x,z)) & ~Q(y)) | P(x,y,f(x,z)))")
    formel3.toString should be ("~(R(f(x,z)) & P(x,y,f(x,z)))")
    formel4.toString should be ("(True | False)")
    formel5.toString should be ("?[y]: ![x]: (R(f(x,z)) & P(x,y,f(x,z)))")
    formel6.toString should be ("((R(f(x,z)) & Q(y)) & P(x,y,f(x,z)))")
  }







}
