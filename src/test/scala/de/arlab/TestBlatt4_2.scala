package de.arlab

import formulas._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class TestBlatt4_2 extends FunSuite with ShouldMatchers {

  test("aufgabe 4.2"){
    val x = Variable("x")
    val y = Variable("y")
    val w = Variable("w")
    val z = Variable("z")

    val f1 = Function("f",Function("g",x),z)
    val f2 = Function("f",Function("g",x),Function("f",x))
    val f3 = Function("f",Function ("g",z))

    val P1 = Predicate("P", Function("f",w),w, Function("f",z))
    val P2 = Predicate("P",f3,x,y)
    val Q1 = Predicate("Q",Function("f",x))
    val Q2 = Predicate("Q",x)


    Unification.unifiable(List[(Term,Term)]((x,y))) should be (true)
    Unification.unifiable(List[(Term,Term)]((f1,f2))) should be (true)
    Unification.unifiable(List[(Term,Term)]((f1,Function("f",Function("f",x),z)))) should be (false)

    Unification.unifiableLiteral(List[(Predicate,Predicate)]((P1,P2))) should be (true)
    Unification.unifiableLiteral(List[(Predicate,Predicate)]((Q1,Q2))) should be (false)




  }

}
