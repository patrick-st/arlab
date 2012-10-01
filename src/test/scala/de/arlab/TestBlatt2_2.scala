package de.arlab


import formulas._
import formulas.And
import formulas.Exists
import formulas.ForAll
import formulas.Not
import formulas.Or
import formulas.Predicate
import formulas.Variable
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import formulas.transformations.Transformations


class TestBlatt2_2 extends FunSuite with ShouldMatchers {

  test("aufgabe 2.2"){
    val x1 = new Variable("x1")
    val x3 = new Variable("x3")
    val P = new Predicate("P",x3,x1)
    val Q = new Predicate("Q",x1)
    val A = new Predicate("A")
    val B = new Predicate("B")
    val C = new Predicate("C")
    val D = new Predicate("D")
    val G = new Predicate("G")

    //nnf Test
    val nnfTestFormel1 = new Not( new ForAll(x3,new And(P, new And(new Exists(x1,Q), True))))
    val transformed = Transformations.nnf(nnfTestFormel1)
    transformed should be (new Exists(x3,new Or(Not(P),new Or(new ForAll(x1,new Not(Q)),False))))
    transformed.toString should be ("?[x3]: (~P(x3,x1) | (![x1]: ~Q(x1) | False))")

    val nnfTestFormel2 = Transformations.nnf(-((A || B || -C) && (-A || D || -G)))
    nnfTestFormel2 should be ((-A && -B && C) || (A && -D && G))

    //cnf Test
    val testFormel = Transformations.cnf(((A && B) || C) && B)
    testFormel should be ((A || C) && (B || C) && B)


    val formelA = Transformations.cnf(-((A || B || -C) && (-A || D || -G)))
    formelA should be   (((((-A || A) && (-B || A)) && (C || A)) && (((-A || -D) && (-B || -D)) &&
      (C || -D))) && (((-A || G) && (-B || G)) && (C || G)))

    val formelB = Transformations.cnf(new Not(new Not(new Not(A))))
    formelB should be (new Not(A))

    val formelC = Transformations.cnf(new Or(A,new Not(new And(B, new Not(C)))))
    formelC should be (new Or(A,new Or(new Not(B), C)))
  }

}
