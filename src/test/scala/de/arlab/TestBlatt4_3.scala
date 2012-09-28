package de.arlab

import formulas.transformations.Transformations
import formulas.{ForAll, Exists, Variable, Predicate}
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class TestBlatt4_3 extends FunSuite with ShouldMatchers {

 test("aufgabe 4.3"){
  val x = Variable("x")
  val y = Variable("y")
  val z = Variable("z")

  val P = Predicate("P",x)
   val Qz = Predicate("Q",z)
   val Qy = Predicate("Q",y)

   val formelA = P && Qz && Exists(y,Qy)
   Transformations.generalize(formelA) should be (ForAll(z,ForAll(x,formelA)))

}

}
