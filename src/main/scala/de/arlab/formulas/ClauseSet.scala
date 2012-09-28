package de.arlab.formulas

import transformations.Transformations


object ClauseSet {


  /**
   * Bildet nach der Skolemisierung und Pränexnormalform die Klauselform
   * @param fm  beliebige Formel
   * @return
   */
  def create(fm: Formula): List[List[Formula]] = {
    val skol = Transformations.skolemize(fm)
    val pnf = Transformations.pnf(skol)
    val matrix = Transformations.matrix(pnf)
    val simp = Transformations.simplify(matrix)
    val cnf = Transformations.cnf(simp)
    getClauses(cnf).map(clauseToList(_))

  }


  /**
   * die Methode sammelt alle KLauseln in einer Liste, ausgehend von einer CNF
   * @param fm  Formel in CNF
   * @return
   */
  def getClauses(fm: Formula) : List[Formula] = fm match{
    case And(a,b) => getClauses(a) union getClauses(b)
    case Or(a,b) => List[Formula](fm)
    case other => List[Formula](other)
  }


  /**
   *  Methode reräsentiert eine Klausel fm als Liste
   * @param fm Formel, die eine gültige Klausel ist
   * @return
   */
  def clauseToList(fm: Formula): List[Formula] = fm match {
    case Or(a,b) if (a.isInstanceOf[Or] && b.isInstanceOf[Or]) => clauseToList(a) union clauseToList(b)
    case Or(a,b) if (a.isInstanceOf[Or]) => clauseToList(a) union List[Formula](b)
    case Or(a,b) if (b.isInstanceOf[Or]) => List[Formula](a) union clauseToList(b)
    case Or(a,b) if (!(a.isInstanceOf[Or]) && !(b.isInstanceOf[Or])) => List[Formula](a,b)
    case other => List[Formula](other)
}

}
