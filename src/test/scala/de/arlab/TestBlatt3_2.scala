package de.arlab

import formulas._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers




class TestBlatt3_2 extends FunSuite with ShouldMatchers {

  test("aufgabe 3.2"){
    val x = new Variable("x")
    val x1 = new Variable("x1")
    val x2 = new Variable("x2")
    val y = new Variable("y")
    val z = new Variable("z")

    val Rxy = new Predicate("R",x,y)
    val Ryz = new Predicate("R",y,z)
    val Rx1f = new Predicate("R",x1, new Function("f",x))
    val Rx2f = new Predicate("R",x2, new Function("f",x))
    val Ryf = new Predicate("R",y,new Function("f",x))
    val Ryf1 = new Predicate("R",y,new Function("f",x1))

    val Px = new Predicate("P",x)
    val Px1 = new Predicate("P",x1)
    val Px2 = new Predicate("P",x2)

    val teilFormelB = -Px || Ryz
    val formelA = new Exists(x,Rxy && new ForAll(y,teilFormelB))

    val map = Map(y -> new Function("f",x))
    val map2 = map + (z -> new Function("f",x))
    val map3 = map + (z -> new Function("f",x1))
    val map4 = Map(y -> z, z -> x)

    formelA.subst(map) should be (new Exists(x1,Rx1f && new ForAll(y,-Px1 || Ryz)))
    formelA.subst(map2) should be (new Exists(x1,Rx1f && new ForAll(y,-Px1 || Ryf)))
    formelA.subst(map3) should be ((new Exists(x2,Rx2f && new ForAll(y,-Px2 || Ryf1))))
    // Rxy.subst(map4) should be (new Predicate("R", x,x))

  }


}
