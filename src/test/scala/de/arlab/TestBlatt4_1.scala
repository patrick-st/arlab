package de.arlab

import formulas.Predicate
import formulas.transformations.Transformations
import formulas._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class TestBlatt4_1 extends FunSuite with ShouldMatchers {

  test("aufgabe 4.1"){
    val a =  Predicate("a")
    val b =  Predicate("b")
    val c =  Predicate("c")
    val d =  Predicate("d")
    val x = Variable("x")
    val bx =  Predicate("b",x)

    ClauseSet.clauseToList((a || b )|| (c || d) ) should be (List[Formula](a,b,c,d))
    ClauseSet.clauseToList(a || b || c || d) should be (List[Formula](a,b,c,d))



    val teilA = (a || b || c) && (a || b)
    val teilB = (a || b || c || d) && -a
    val formelA = teilA && teilB


    val klausel1 = List[Formula](a,b,c)
    val klausel2 = List[Formula](a,b)
    val klausel3 = List[Formula](a,b,c,d)
    val klausel4 = List[Formula](-a)
    val resA = List[List[Formula]](klausel1,klausel2,klausel3,klausel4)

    ClauseSet.create(formelA) should be (resA)

    val formelB =  ((a || b || c) && Exists(x,(a || bx))) && ForAll(x,(a || b || c || d) && (-a))
    val resB =   List[List[Formula]](klausel1,List[Formula](a, Predicate("b",Function("sk1"))), klausel3, klausel4)
    ClauseSet.create(formelB) should be (resB)



  }

}
