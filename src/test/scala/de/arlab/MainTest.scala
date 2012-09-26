package de.arlab

import formulas._
import formulas.And
import formulas.Or
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import transformations.Transformations
import scala.Cell

/**
 * Tests the main object
 */
class MeinTest extends FunSuite with ShouldMatchers {
  test("test some basic things") {
    val list = List(1,2,3,4)
    list should have size (4)
    list.isEmpty should be (false)
    list(3) should be (4)
  }

}
