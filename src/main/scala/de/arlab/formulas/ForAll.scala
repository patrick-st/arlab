package de.arlab.formulas

/**
 * repräsentiert den Allquantor
 * @param variable
 * @param a entspricht einer Formel
 */
case class ForAll(variable: Variable, a: Formula ) extends Formula {

  override def toString = {
    "![" + variable + "]: " + a
  }

  /**
   * Bestimmung aller  Variablen der Formel a
   * anschließende Entfernung der gebundenen Variable
   * @return
   */
 def free: Set[Variable] = a.free - variable

  /**
   * Hinzufügen der gebundenen Variable, die über den Quantor definiert ist
   * @return
   */
  def bound: Set[Variable] = a.bound + variable


  /**
   * Substitution aller Terme, die über die Map sfn definiert sind
   * @param sfn
   * @return
   */
  def subst(sfn: Map[Variable,Term]): Formula = {
    var sfn_ = sfn
    var newVariable = variable
    //1. Fall: Man möchte eine gebundene Variable substituieren
    sfn_ -= variable

    //2. Fall beim Substituieren fällt eine freie Variable unter eine Bindung
    val allVariables = sfn.values.foldLeft(Set[Variable]())(_ union _.free)
    if (allVariables.contains(variable)){
      newVariable = variable.variant(this.bound union allVariables)
      sfn_ += (variable -> newVariable)
    }

    new ForAll(newVariable,a.subst(sfn_))
  }

  /**
   * Gibt alle Funktionen (mit Stelligkeit) der Formel a zurück
   * @return
   */
  def functions: Set[(String, Int)]  = a.functions
}
