package de.arlab

import formulas.{False, True, Predicate}
import formulas.transformations.Transformations
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class TestBlatt2_3 extends FunSuite with ShouldMatchers{
  test("aufgabe 2.3"){
    val a = new Predicate("a")
    val b = new Predicate("b")
    val c = new Predicate("c")
    val d = new Predicate("d")


    Transformations.simplify(True) should be (True)
    Transformations.simplify(a && b || True) should be (True)
    Transformations.simplify(a && (b || True)) should be (a)
    // Transformations.simplify(a && (b || a)) should be (a)
    Transformations.simplify(a && (b || c)) should be (a && (b || c))
    Transformations.simplify((-(-a) || -(-a)) && (a || -a) && (True || a)
      && (False || False) && (a || b)) should be (False)
    Transformations.simplify((-(-a) || -(-a)) && (a || -a)
      &&(True || a) && (False || False) && (a || b) && (a || False)) should be (False)

    Transformations.simplify(((a || b || c) && (a || b)) &&
      ((a || b || c || d) && (-a))) should be (((a || b || c) && (a || b)) && ((a || b || c || d) && (-a)))


    val teilA = (a || b || c) && (a || b)
    val teilB = (a || b || c || d) && -a
    val formelA = teilA && teilB

   Transformations.simplify(formelA) should be (((a || b || c) && (a || b)) && ((a || b || c || d) && (-a)))
  }


}
