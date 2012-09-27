package de.arlab.formulas


case class Function(name: String, terms: Term*) extends Term {

  override def toString = {
    if (terms.isEmpty){
      name
    } else {
      name + terms.mkString("(", "," , ")")
    }
  }

  /**
   * Bestimmung der freien Variablen aller Terme
   * anschließende Vereinigung
   * @return
   */
  def free: Set[Variable] =  terms.foldLeft(Set[Variable]())(_ union _.free)

  /**
   * Rückgabe einer leeren Menge
   * @return
   */
  def bound: Set[Variable] = Set[Variable]()

  def subst(sfn: Map[Variable,Term]): Term = new Function(name, terms.map(t => t.subst(sfn)):_*)

}
