package com.github.comco.scrappy.checker.quantifier

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers

class ForSomeQuantifierSpec extends FlatSpec with CustomMatchers {
  "A ForSomeQuantifier" should "be bad when empty" in {
    var q = ForSomeQuantifier.createEmpty()
    q.finish()
    q.valid shouldEqual false
  }
  
  it should "be invalid when fed only falses" in {
    var q = ForSomeQuantifier.createEmpty()
    q.put(false)
    q.unusual shouldEqual false
    q.put(false)
    q.unusual shouldEqual false
    q.finish()
    q.valid shouldEqual false
  }
  
  it should "stop validly at first true" in {
    var q = ForSomeQuantifier.createEmpty()
    q.put(true)
    q.unusual shouldEqual true
    q.valid shouldEqual true
  }
}