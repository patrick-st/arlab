package de.arlab

import formulas.{Variable, Predicate, Function}

object Main {
	def main(args: Array[String]) {
      val x = new Variable("x")
      val y = new Variable("y")
      val a = new Predicate("P",x,new Function("g",new Function("f"),x))&&(new Predicate("Q")||new Predicate("R",y))

	}


}
