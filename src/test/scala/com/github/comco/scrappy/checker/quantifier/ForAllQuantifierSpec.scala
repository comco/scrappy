package com.github.comco.scrappy.checker.quantifier

import org.scalatest.FlatSpec
import com.github.comco.scrappy.CustomMatchers

class ForAllQuantifierSpec extends FlatSpec with CustomMatchers {
  "A ForAllQuantifier" should "be ok when empty" in {
    var q = ForAllQuantifier.createEmpty()
    q.finish()
    q.valid shouldEqual true
  }
  
  it should "be valid when fed with only trues" in {
    var q = ForAllQuantifier.createEmpty()
    q.put(true)
    q.unusual shouldEqual false
    q.put(true)
    q.unusual shouldEqual false
    q.finish()
    q.valid shouldEqual true
  }
  
  it should "stop at first false" in {
    var q = ForAllQuantifier.createEmpty()
    q.put(true)
    q.unusual shouldEqual false
    q.put(false)
    q.unusual shouldEqual true
    q.valid shouldEqual false
  }
}