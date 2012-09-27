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
  def bound: Set[Variable] = Set[Variable]()

  def subst(sfn: Map[Variable,Term]): Term = sfn.getOrElse(this, this)

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
      return variable
    }




}
