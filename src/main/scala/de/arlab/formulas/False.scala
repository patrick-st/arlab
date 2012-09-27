package de.arlab.formulas

/**
 * repräsentiert False
 */
case object False extends Formula {

  def free: Set[Variable] = Set[Variable]()

  def bound: Set[Variable] = Set[Variable]()

  def subst(sfn: Map[Variable,Term]): Formula = this
}
