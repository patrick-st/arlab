package de.arlab.formulas

import transformations.Transformations


object ClauseSet {


  def create(fm: Formula): List[List[Formula]] = {
    val skol = Transformations.skolemize(fm)
    val pnf = Transformations.pnf(skol)
    val matrix = Transformations.matrix(pnf)
    val simp = Transformations.simplify(matrix)
    val cnf = Transformations.cnf(simp)
    createClausesOr(cnf).map(create_(_))

  }


  def createClausesOr(fm: Formula) : List[Formula] = fm match{
    case And(a,b) => createClausesOr(a) union createClausesOr(b)
    case Or(a,b) => List[Formula](fm)
    case other => List[Formula](other)
  }

  def create_(fm: Formula): List[Formula] = fm match {
    case Or(a,b) if (a.isInstanceOf[Or] && b.isInstanceOf[Or]) => create_(a) union create_(b)
    case Or(a,b) if (a.isInstanceOf[Or]) => create_(a) union List[Formula](b)
    case Or(a,b) if (b.isInstanceOf[Or]) => List[Formula](a) union create_(b)
    case Or(a,b) if (!(a.isInstanceOf[Or]) && !(b.isInstanceOf[Or])) => List[Formula](a,b)
    case other => List[Formula](other)
}

}
