package de.arlab

import formulas._
import formulas.And
import formulas.Exists
import formulas.ForAll
import formulas.Function
import formulas.Not
import formulas.Or
import formulas.Predicate
import formulas.Variable
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import formulas.transformations.Transformations


class TestBlatt3 extends FunSuite with ShouldMatchers {

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



    //test skolem
    val skolem1 = new Function("sk1",x,w)
    val skolem2 = new Function("sk1",w)
    val formelE = ForAll(x,Exists(z,Pxz && Exists(y,Qyw)))
    Transformations.skolemize(formelE) should be (ForAll(x,Predicate("P",x,skolem1) && Predicate("Q",skolem2,w)))


    val skolem3 = new Function("sk1",w)
    val skolem4 = new Function("sk2",w)
    val formelF = ForAll(x,Exists(z,Pz && Exists(y,Qyw)))
    //Transformations.skolemize(formelF) should be (ForAll(x,Predicate("P",x,skolem3) && Predicate("Q",skolem4,w)))

    //test matrix
    Transformations.matrix(ForAll(x,ForAll(y,Px && Ry))) should be (Px && Ry)





  }
}
