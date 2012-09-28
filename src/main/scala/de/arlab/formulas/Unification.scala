package de.arlab.formulas

object Unification {

  /**
   * endscheidet ob eine Menge von Termtupeln unifizierbar ist
   * @param tms
   * @return
   */
  def unifiable(tms: List[(Term, Term)]): Boolean = {
    var uni = true
    for (i <- 0 to tms.size-1){
      val currentTupel = tms(i)
      currentTupel match {
        case (a: Function, b: Function) if a.name == b.name && a.terms.size == b.terms.size  => {
            val tupel = this.tupelize(a.terms.toList, b.terms.toList)
            uni = uni && this.unifiable(tupel)
        }
        case (a: Function, b: Variable) if (!a.free.contains(b)) => uni = true
        case (a: Variable, b: Function)  if (!b.free.contains(a)) => uni = true
        case (a: Variable, b: Variable)  => uni = true
        case other => uni = false
      }
    }
   uni

  }


  /**
   * erstellt aus zwei Listen eine (Term, Term)-Liste
   * @param ls1 Termliste
   * @param ls2 Termliste
   * @return
   */
  def tupelize(ls1: List[Term], ls2: List[Term]): List[(Term, Term)] = {
   var res = List[(Term,Term)]()
   for (i <- 0 to ls1.size-1){
     res ::= (ls1(i),ls2(i))
   }

    res
  }

  /**
   * Endscheidet ob eine Liste von Prädikattupeln unifizierbar ist
   * @param tms
   * @return
   */
  def unifiableLiteral(tms: List[(Predicate, Predicate)]): Boolean = {
    var uni = true
    for (i <- 0 to tms.size-1){
      val currentTupel = tms(i)
      currentTupel match {
        case (a: Predicate, b: Predicate) if (a.name == b.name && a.terms.size == b.terms.size) => {
             uni = uni && this.unifiable(this.tupelize(a.terms.toList, b.terms.toList))
        }
        case other => uni = false
      }
    }

    uni
  }


}
