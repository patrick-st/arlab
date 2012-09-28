package de.arlab.formulas

/**
 *  repräsentiert eine Variable
 * @param value Variablenbezeichnung
 */
case class Variable(value: String) extends Term {
  override def toString = value

  /**
   *   Objekt gibt sich selbs in einer Menge zurück
   * @return
   */
  def free: Set[Variable] =  Set[Variable](this)

  /**
   * Gibt eine leere Menge zurück
   * @return
   */
  def bound: Set[Variable] = Set[Variable]()

  /**
   *  substituiert die Variable zu einem neuen Term, wenn sie in der Map enthalten ist
   * @param sfn
   * @return
   */
  def subst(sfn: Map[Variable,Term]): Term = sfn.getOrElse(this, this)

  /**
   * variiert eine Variable zu einer neuen, die noch nicht im Set enthalten ist
   * @param vars
   * @return
   */
  def variant(vars: Set[Variable]): Variable = {
     var exit = false
     var counter = 1
     var variable = this
     while (!exit){
       variable = new Variable(this.value+counter)
       if (vars.contains(variable)){
         counter = counter+1
       }  else {
         exit = true
       }
     }
     variable
   }

  /**
   * gibt die Funktionen mit Stelligkeit zurück
   * @return
   */
  def functions: Set[(String, Int)] = Set[(String, Int)]()




}
