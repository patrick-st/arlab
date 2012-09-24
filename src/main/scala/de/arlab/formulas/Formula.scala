package de.arlab.formulas


trait Formula {

  def &&(that: Formula) = And(this, that)
  def ||(that: Formula) = Or(this, that)
  def unary_- = Not(this)

}
