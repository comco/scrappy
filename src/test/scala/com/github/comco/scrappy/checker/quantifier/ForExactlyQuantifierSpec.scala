package com.github.comco.scrappy.checker.quantifier

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers

class ForExactlyQuantifierSpec extends FlatSpec with CustomMatchers {
  "A ForExactlyQuantifier" should "be invalid when empty" in {
    val q0 = ForExactlyQuantifier.from(0).createEmpty()
    q0.finish()
    q0.valid shouldEqual true
    val q1 = ForExactlyQuantifier.from(1).createEmpty()
    q1.finish()
    q1.valid shouldEqual false
  }
  
  it should "be valid when fed the right number of trues" in {
    val q = ForExactlyQuantifier.from(2).createEmpty()
    q.put(false)
    q.unusual shouldEqual false
    q.put(true)
    q.unusual shouldEqual true
    q.put(true)
    q.unusual shouldEqual true
    q.finish()
    q.valid shouldEqual true
  }
  
  it should "be invalid when less number of trues are put" in {
    val q = ForExactlyQuantifier.from(2).createEmpty()
    q.put(false)
    q.put(true)
    q.finish()
    q.valid shouldEqual false
  }
  
  it should "be invalid when more number of trues are put" in {
    val q = ForExactlyQuantifier.from(2).createEmpty()
    q.put(true)
    q.put(true)
    q.put(true)
    q.valid shouldEqual false
  }
  
  "A ForExactlyQuantifier during construction" should "check for non-negative count" in {
    itShouldBeDisallowed calling ForExactlyQuantifier.from(-1)
  }
}