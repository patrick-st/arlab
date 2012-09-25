package de.arlab

import formulas._
import formulas.And
import formulas.Or
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import transformations.Transformations

/**
 * Tests the main object
 */
class MeinTest extends FunSuite with ShouldMatchers {
  test("test some basic things") {
    val list = List(1,2,3,4)
    list should have size (4)
    list.isEmpty should be (false)
    list(3) should be (4)
  }


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

    x.toString should be ("x")
    formel1.toString should be ("((R(f(x,z)) & ~Q(y)) | P(x,y,f(x,z)))")
    formel3.toString should be ("~((R(f(x,z)) & P(x,y,f(x,z))))")
    formel4.toString should be ("(True | False)")
    formel5.toString should be ("?[y]: ![x]: (R(f(x,z)) & P(x,y,f(x,z)))")
 }


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
    val formel = new Not( new ForAll(x3,new And(P, new And(new Exists(x1,Q), True))))
    val transformed = Transformations.nnf(formel)
    transformed should be (new Exists(x3,new Or(Not(P),new Or(new ForAll(x1,new Not(Q)),False))))
    transformed.toString should be ("?[x3]: (~P(x3,x1) | (![x1]: ~Q(x1) | False))")


     //cnf Test
   /* val formelA = Transformations.cnf(-((A || B || -C) && (-A || D || -G)))
    formelA should be   (((((-A | A) & (~B | A)) & (C | A)) & (((~A | ~D) & (~B | ~D)) &
      (C | ~D))) & (((~A | G) & (~B | G)) & (C | G))) */

    val formelB = Transformations.cnf(new Not(new Not(new Not(A))))
    formelB should be (new Not(A))

    val formelC = Transformations.cnf(new Or(A,new Not(new And(B, new Not(C)))))
    formelC should be (new Or(A,new Or(new Not(B), C)))
  }

  test("aufgabe 2.3"){
    val a = new Predicate("a")
    val b = new Predicate("b")
    val c = new Predicate("c")

    Transformations.simplify(True) should be (True)
    Transformations.simplify(a && b || True) should be (True)
    Transformations.simplify(a && (b || True)) should be (a)
    Transformations.simplify(a && (b || a)) should be (a)
    Transformations.simplify(a && (b || c)) should be (a && (b || c))
  }



}
