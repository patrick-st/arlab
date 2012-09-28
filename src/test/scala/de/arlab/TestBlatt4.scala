package de.arlab

import formulas.transformations.Transformations
import formulas.{ClauseSet, Formula, Not, Predicate}
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class TestBlatt4 extends FunSuite with ShouldMatchers {

  test("aufgabe 4.1"){
    val a =  Predicate("a")
    val b =  Predicate("b")
    val c =  Predicate("c")
    val d =  Predicate("d")

    //println((a || b )
    ClauseSet.create_((a || b )|| (c || d) ) should be (List[Formula](a,b,c,d))
    ClauseSet.create_(a || b || c || d) should be (List[Formula](a,b,c,d))



    val teilA = (a || b || c) && (a || b)
    val teilB = (a || b || c || d) && -a
    val formelA = teilA && teilB


    val klausel1 = List[Formula](a,b,c)
    val klausel2 = List[Formula](a,b)
    val klausel3 = List[Formula](a,b,c,d)
    val klausel4 = List[Formula](-a)
    val resA = List[List[Formula]](klausel1,klausel2,klausel3,klausel4)

    Transformations.skolemize(formelA) should be (formelA)

    Transformations.pnf(formelA) should be (formelA)

    Transformations.matrix(formelA) should be (formelA)

    Transformations.cnf(formelA) should be (formelA)

    Transformations.simplify(formelA) should be
    (formelA)

    ClauseSet.create(formelA) should be
    (resA)

   formelA should be  (((a || b || c) && (a || b)) && ((a || b || c || d) && (-a)))


  }

}
