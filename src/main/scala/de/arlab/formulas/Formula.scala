package de.arlab.formulas

/**
 * Trait für Formeln
 */
trait Formula {

  def &&(that: Formula) = And(this, that)
  def ||(that: Formula) = Or(this, that)
  def unary_- = Not(this)

}
