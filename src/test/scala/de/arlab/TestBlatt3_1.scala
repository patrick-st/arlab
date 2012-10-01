package de.arlab

import formulas._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class TestBlatt3_1 extends FunSuite with ShouldMatchers {

  test("aufgabe 3.1"){
    val x = new Variable("x")
    val y = new Variable("y")
    val Px = new Predicate("P",x)
    val Py = new Predicate("P",y)
    val Pxy = new Predicate("P",x,y)
    val Q = new Predicate("Q",x)
    val R = new Predicate("R",y)
    val teilFormel = Q || (R && new Exists(y, Py))

    val formelA = Px && new ForAll(x, teilFormel)
    formelA.free should be (Set[Variable](x,y))
    formelA.bound should be (Set[Variable](x,y))

    val formelB = new ForAll(x,Pxy)
    formelB.free should be (Set[Variable](y))
    formelB.bound should be (Set[Variable](x))


    val z = new Variable("z")
    val Rxy = new Predicate("R",y, new Function("f",x))
    val Ryz = new Predicate("R",y,z)
    val teilFormelB = -Px || Ryz

    val formelC = new Exists(x,Rxy && new ForAll(y,teilFormelB))
    formelC.free should be (Set[Variable](y,z))
    formelC.bound should be (Set[Variable](x,y))
  }


}
