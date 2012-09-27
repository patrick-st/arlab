package de.arlab.formulas

/**
 * repräsentiert True
 */
 case object True extends Formula {

  def free: Set[Variable] = Set[Variable]()

  def bound: Set[Variable] = Set[Variable]()

  def subst(sfn: Map[Variable,Term]): Formula = this
}
