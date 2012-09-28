package de.arlab.formulas

/**
 * Trait für Formeln
 */
trait Formula {

  /**
   * Methoden zur vereinfachten Darstellung
   * @param that
   * @return
   */
  def &&(that: Formula) = And(this, that)
  def ||(that: Formula) = Or(this, that)
  def unary_- = Not(this)

  /**
   * Methode die eine Konversion als Parameter akzeptiert und diese auf sich selbst anwendet
   * @param f Funktion
   * @return
   */
  def transform(f: Formula => Formula) = f(this)


  def free: Set[Variable]
  def bound: Set[Variable]


  def subst(sfn: Map[Variable,Term]): Formula

  def functions: Set[(String, Int)]





}
