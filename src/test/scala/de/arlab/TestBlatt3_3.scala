package de.arlab

import formulas._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import transformations.Transformations


class TestBlatt3_3 extends FunSuite with ShouldMatchers{

  test("aufgabe 3.3"){
    val x = new Variable("x")
    val x1 = new Variable("x1")
    val y = new Variable("y")
    val z = new Variable("z")
    val w = new Variable("w")
    val Ryf = new Predicate("R",y,new Function("f",x,y,z))
    val Ryf2 = new Predicate("R",y,new Function("f",x,y))
    val Rzfg = new Predicate("P",z,new Function("f",y,new Function("g")))
    val Ry = new Predicate("R",y)
    val Px = new Predicate("P",x)
    val Pz = new Predicate("P",z)
    val Pxz = new Predicate("P",x,z)
    val Px1 = new Predicate("P",x1)
    val Qyw = new Predicate("Q",y,w)


    //test functions
    val formelA = new Exists(x,Ryf && new ForAll(y,-Px || Rzfg))
    val setA = Set[(String, Int)](("f",3),("f",2), ("g", 0))
    formelA.functions should be (setA)

    val formelB = new Exists(x,Ryf2 && new ForAll(y,-Px || Rzfg))
    val setB = Set[(String, Int)](("f",2), ("g", 0))
    formelB.functions should be (setB)


    //test pnf
    val formelC =  ForAll(x,Px && ForAll(y,Ry))
    Transformations.pnf(formelC) should be (ForAll(x,ForAll(y,Px && Ry)))

    val formelD = Px && ForAll(x,Px)
    Transformations.pnf(formelD) should be (ForAll(x1,Px && Px1))

    val formelG = ForAll(x,Px || ForAll(y,Ry))
    Transformations.pnf(formelG) should be (ForAll(x,ForAll(y,Px || Ry)))

    val formelH = Px || ForAll(x,Px)
    Transformations.pnf(formelH) should be (ForAll(x1,Px || Px1))


    //test skolem
    val skolem1 = new Function("sk1",x,w)
    val skolem2 = new Function("sk1",w)
    val formelE = ForAll(x,Exists(z,Pxz && Exists(y,Qyw)))
    Transformations.skolemize(formelE) should be (ForAll(x,Predicate("P",x,skolem1) && Predicate("Q",skolem2,w)))


    val formelF = ForAll(x,Exists(z,Pz && Exists(y,Qyw)))
    Transformations.skolemize(formelF) should be (ForAll(x,Predicate("P",skolem2) && Predicate("Q",skolem2,w)))

    //test matrix
    Transformations.matrix(ForAll(x,ForAll(y,Px && Ry))) should be (Px && Ry)
    Transformations.matrix(Exists(x,ForAll(y,Px && Ry))) should be (Px && Ry)
  }

}
