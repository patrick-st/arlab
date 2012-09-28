package de.arlab.formulas

/**
 * repräsentiert Prädikate
 * @param name
 * @param terms  Terme des Prädikats
 */
case class Predicate(name: String, terms: Term*) extends Formula {

  override def toString = {
    if (terms.isEmpty){
      name
    } else{
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


  /**
   * substituiert alle Terme, die über die Map sfn definiert sind
   * @param sfn
   * @return
   */
  def subst(sfn: Map[Variable,Term]): Formula = new Predicate(name, terms.map(t => t.subst(sfn)):_*)


  /**
   * gibt die Funktionen (mit Stelligkeit) aller Terme zurück
   * @return
   */
  def functions: Set[(String, Int)]  = terms.foldLeft(Set[(String,Int)]())(_ union _.functions)

}
