package de.arlab.formulas

/**
 * Trait für Terme
 */
trait Term {

  def free: Set[Variable]

  def bound: Set[Variable]

  def subst(sfn: Map[Variable,Term]): Term

  def functions: Set[(String, Int)]


}
