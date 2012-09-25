package de.arlab.formulas


case class Function(name: String, terms: Term*) extends Term {

  override def toString = {
    name + "(" + terms.mkString(",") + ")"
  }

}
