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

  /**
   * substituiert alle Terme, die über die Map sfn definiert sind
   * @param sfn
   * @return
   */
  def subst(sfn: Map[Variable,Term]): Term = new Function(name, terms.map(t => t.subst(sfn)):_*)

  /**
   * gibt sich selbst und die Funktionen (mit Stelligkeit) aller Terme zurück
   * @return
   */
  def functions: Set[(String, Int)]  =
    Set[(String, Int)]((name,terms.length)) union terms.foldLeft(Set[(String,Int)]())(_ union _.functions)

  def variant(vars: Set[(String, Int)]): Function = {
    var exit = false
    var counter = 1
    var name = this.name
    while (!exit){
      name = name+counter
      if (vars.contains((name,terms.length))){
        counter = counter+1
      }  else {
        exit = true
      }
    }
   Function(name, terms:_*)
  }

}

